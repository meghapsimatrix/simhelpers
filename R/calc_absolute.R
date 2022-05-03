#' @title Calculate absolute performance criteria and MCSE
#'
#' @description Calculates absolute bias, variance, mean squared error (mse)
#' and root mean squared error (rmse). The function also calculates the associated
#' Monte Carlo standard errors.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param estimates name of the column containing the estimates.
#' @param true_param name of the column containing the true parameters.
#' @param perfm_criteria character or character vector indicating the performance criteria to be calculated.
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_absolute(res_dat = t_res, estimates = est, true_param = true_param)
#'
#' @importFrom stats sd
#' @importFrom stats var

calc_absolute <- function(res_dat, estimates, true_param, perfm_criteria = c("bias", "variance", "mse", "rmse")){

  estimates <- res_dat %>%
    dplyr::filter(!is.na({{estimates}})) %>%
    dplyr::pull({{estimates}}) # estimates

  K <- length(estimates) # number of iterations
  true_param <- res_dat %>% dplyr::pull({{true_param}}) # true param
  true_param <- true_param[1] # true param

  # calculate sample stats
  t_bar <- mean(estimates) # mean of estimates
  bias <- t_bar - true_param # bias
  var_t <- var(estimates) # variance
  s_t <- sd(estimates) # standard deviation
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3) # skewness
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4) # kurtosis

  mse <- mean((estimates - true_param)^2) # calculate mse

  #jacknife
  t_bar_j <- (1/(K - 1)) * (K * t_bar - estimates) # jacknife t bar
  bias_j_sq <- (t_bar_j - true_param)^2 # jacknife bias
  s_sq_t_j <- (1 / (K - 2)) * ((K - 1) * var_t - (K / (K - 1)) * (estimates - t_bar)^2) # jacknife var

  rmse_j <- sqrt(bias_j_sq + s_sq_t_j) # jacknife rmse


  # initialize tibble
  dat <- tibble::tibble(K = K)

  if("bias" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(bias = bias,
                    bias_mcse = sqrt(var_t / K))
  }

  if("variance" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(var = var_t,
                    var_mcse = var_t * sqrt(((k_t - 1) / K)))
  }


  if("mse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(mse = mse,
                    mse_mcse = sqrt((1/K) * (s_t^4 * (k_t -1) + 4 * s_t^3 * g_t * bias + 4 * var_t * bias^2)))
  }

  if("rmse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rmse = sqrt(mse),
                    rmse_mcse = sqrt(((K - 1)/(K)) * sum((rmse_j - rmse)^2)))
  }

  return(dat)

}
