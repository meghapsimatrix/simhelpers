#' @title Calculate absolute performance criteria and MCSE
#'
#' @description Calculates absolute bias, variance, mean squared error (mse)
#' and root mean squared error (rmse). The function also calculates the associated
#' Monte Carlo standard errors.
#'
#' @param data data frame or tibble containing the simulation results.
#' @param estimates Vector or name of column from \code{data} containing the estimates.
#' @param true_param Vector or name of column from \code{data} containing the true parameters.
#' @param criteria character or character vector indicating the performance criteria to be calculated.
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_absolute(data = t_res, estimates = est, true_param = true_param)
#'
#' @importFrom stats sd
#' @importFrom stats var
#' @importFrom magrittr "%>%"


calc_absolute <- function(data, estimates, true_param, criteria = c("bias", "variance", "mse", "rmse")) {

  if (!missing(data)) {
    cl <- match.call()
    estimates <- eval(cl$estimates, envir = data)
    true_param <- eval(cl$true_param, envir = data)

  }

  estimates <- estimates[!is.na(estimates)]
  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")

  K <- length(estimates) # number of iterations

  # calculate sample stats
  t_bar <- mean(estimates) # mean of estimates
  bias <- t_bar - true_param # bias
  s_t <- sd(estimates) # standard deviation
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3) # skewness
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4) # kurtosis

  mse <- mean((estimates - true_param)^2) # calculate mse

  #jacknife
  t_bar_j <- (1/(K - 1)) * (K * t_bar - estimates) # jacknife t bar
  bias_j_sq <- (t_bar_j - true_param)^2 # jacknife bias
  s_sq_t_j <- (1 / (K - 2)) * ((K - 1) * s_t^2 - (K / (K - 1)) * (estimates - t_bar)^2) # jacknife var

  rmse_j <- sqrt(bias_j_sq + s_sq_t_j) # jacknife rmse

  # initialize tibble
  dat <- tibble::tibble(K = K)

  if ("bias" %in% criteria) {
    dat$bias <- bias
    dat$bias_mcse <- s_t / sqrt(K)
  }

  if ("variance" %in% criteria){
    dat$var <- s_t^2
    dat$var_mcse <- s_t^2 * sqrt(((k_t - 1) / K))
  }


  if ("mse" %in% criteria) {
    dat$mse <- mse
    dat$mse_mcse <- sqrt((1/K) * (s_t^4 * (k_t -1) + 4 * s_t^3 * g_t * bias + 4 * s_t^2 * bias^2))
  }

  if ("rmse" %in% criteria) {
    dat$rmse <- sqrt(mse)
    dat$rmse_mcse <- sqrt(((K - 1)/(K)) * sum((rmse_j - rmse)^2))
  }

  return(dat)

}
