#' @title Calculate relative performance criteria and MCSE.
#'
#' @description Calculates relative bias, mean squared error (mse)
#' and root mean squared error (rmse). The function also calculates the associated
#' Monte Carlo Standard errors.
#'
#' @param res_dat A dataframe or tibble containing the simulation results.
#' @param estimates The name of the column containing the estimates
#' @param true_param The name of the column containing the true parameters.
#' @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return A tibblecontaining the performance criteria estimate(s) and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_relative(res_dat = t_res, estimates = est, true_param = true_param)
#'
#'

calc_relative <- function(res_dat, estimates, true_param, perfm_criteria = c("relative bias", "relative mse")){

  require(dplyr)
  require(tibble)

  estimates <- res_dat %>% pull({{estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% pull({{true_param}})
  true_param <- true_param[1]

  #calculate sample stats
  t_bar <- mean(estimates)
  bias <- t_bar - true_param
  var_t <- var(estimates)
  s_t <- sd(estimates)
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4)
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3)
  mse <- bias^2 + var_t

  # initialize tibble
  dat <- as_tibble(data.frame(matrix(ncol = 0, nrow = 1)))

  if("relative bias" %in% perfm_criteria){
    dat <- dat %>%
      mutate(rel_bias = t_bar / true_param,
             rel_bias_mcse = sqrt(var_t / (K * true_param^2)),
             rel_bias = if_else(true_param == 0, as.numeric(NA), rel_bias),
             rel_bias_mcse = if_else(true_param == 0, as.numeric(NA), rel_bias_mcse))
  }

  if("relative mse" %in% perfm_criteria){
    dat <- dat %>%
      mutate(rel_mse = mse / (true_param^2),
             rel_mse_mcse = sqrt((1 / K * true_param^2) * (s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * bias + 4 * var_t * bias^2)),
             rel_mse = if_else(true_param == 0, as.numeric(NA), rel_mse),
             rel_mse_mcse = if_else(true_param == 0, as.numeric(NA), rel_mse_mcse))
  }

  return(dat)

}
