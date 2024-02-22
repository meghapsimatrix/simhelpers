#' @title Calculate relative performance criteria and MCSE
#'
#' @description Calculates relative bias, mean squared error (relative mse), and root mean squared error (relative rmse).
#' The function also calculates the associated
#' Monte Carlo standard errors.
#'
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_relative(data = t_res, estimates = est, true_param = true_param)
#'
#'

calc_relative <- function(
  data,
  estimates, true_param,
  criteria = c("relative bias", "relative mse", "relative rmse")
) {

  if (!missing(data)) {
    cl <- match.call()
    estimates <- eval(cl$estimates, envir = data)
    true_param <- eval(cl$true_param, envir = data)
  }

  estimates <- estimates[!is.na(estimates)]
  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")

  # calculate sample stats
  K <- length(estimates) # number of iterations
  t_bar <- mean(estimates) # mean of estimates
  bias <- t_bar - true_param # bias
  s_t <- sd(estimates) # standard deviation
  g_t <- sum((estimates - t_bar)^3) / (K * s_t^3) # skewness
  k_t <- sum((estimates - t_bar)^4) / (K * s_t^4) # kurtosis

  # jacknife
  t_bar_j <- (K * t_bar - estimates) / (K - 1) # jacknife t bar
  bias_j_sq <- (t_bar_j - true_param)^2 # jacknife bias
  s_sq_t_j <- ((K - 1) * s_t^2 - (estimates - t_bar)^2 * K / (K - 1)) / (K - 2) # jacknife var

  mse <- bias^2 + s_t^2 # mse
  rel_mse <- mse / true_param^2 # relative mse
  rel_mse_j <- ((t_bar_j - true_param)^2 + s_sq_t_j) / true_param^2 # jacknife relative mse

  # initialize tibble
  dat <- tibble::tibble(K_relative = K)

  if ("relative bias" %in% criteria) {
    dat$rel_bias <- ifelse(true_param == 0, NA_real_, t_bar / true_param)
    dat$rel_bias_mcse <- ifelse(true_param == 0, NA_real_, s_t / (true_param * sqrt(K)))
  }

  if ("relative mse" %in% criteria) {
    dat$rel_mse <- ifelse(true_param == 0, NA_real_, rel_mse)
    dat$rel_mse_mcse <- ifelse(
      true_param == 0,
      NA_real_,
      sqrt((s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * bias + 4 * s_t^2 * bias^2) / K) / true_param
    )
  }

  if ("relative rmse" %in% criteria) {
    rel_rmse <- sqrt(rel_mse)
    dat$rel_rmse <- ifelse(true_param == 0, NA_real_, rel_rmse)
    dat$rel_rmse_mcse <- ifelse(
      true_param == 0,
      NA_real_,
      sqrt(sum((sqrt(rel_mse_j) - rel_rmse)^2) * (K - 1) / K)
    )
  }

  return(dat)

}

