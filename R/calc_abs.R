#' @title Calculate absolute performance criteria and MCSE.
#'
#' @description Calculates absolute bias, variance, mean squared error (mse)
#' and root mean squared error (rmse). The function also calculates the associated
#' Monte Carlo Standard errors.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param estimates name of the column containing the estimates.
#' @param true_param name of the column containing the true parameters.
#' @param perfm_criteria character or character vector indicating the performance criteria to be calculated.
#'
#' @return A tibble containing the performance criteria estimate(s) and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_abs(res_dat = t_res, estimates = est, true_param = true_param)
#'
#'

calc_abs <- function(res_dat, estimates, true_param, perfm_criteria = c("bias", "variance", "mse", "rmse")){

  estimates <- res_dat %>% dplyr::pull({{estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% dplyr::pull({{true_param}})
  true_param <- true_param[1]

  # calculate sample stats
  t_bar <- mean(estimates)
  bias <- t_bar - true_param
  var_t <- var(estimates)
  s_t <- sd(estimates)
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4)
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3)

  mse <- mean((estimates - true_param)^2)
  mse_mcse <- sqrt((1/K) * (s_t^4 * (k_t -1) + 4 * s_t^3 * g_t * bias + 4 * var_t * bias^2))
  rmse <- sqrt(mse)


  #jacknife
  t_bar_j <- (1/(K - 1)) * (K * t_bar - estimates)
  bias_j_sq <- (t_bar_j - true_param)^2
  s_sq_t_j <- (1 / (K - 2)) * ((K - 1) * var_t - (K / (K - 1)) * (estimates - t_bar)^2)

  rmse_j <- sqrt(bias_j_sq + s_sq_t_j)


  # initialize tibble
  dat <- tibble::as_tibble(data.frame(matrix(ncol = 0, nrow = 1)))

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
                    mse_mcse = mse_mcse)
  }

  if("rmse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rmse = rmse,
                    rmse_mcse = sqrt((1/(K)) * sum((rmse_j - rmse)^2)))
  }

  return(dat)

}
