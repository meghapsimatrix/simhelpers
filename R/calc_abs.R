#' Calculate performance criteria and MCSE.
#'
#' @param res_dat A dataframe or tibble containing simulation results.
#' @param estimates The name of the column containing estimates.
#' @param true_param The name of the column containing true parameters.
#' @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_abs(res_dat = t_res, estimates = est, true_param = true_param)
#'
#'

calc_abs <- function(res_dat, estimates, true_param, perfm_criteria = c("bias", "variance", "mse", "rmse")){

  require(dplyr)

  estimates <- res_dat %>% pull({{estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% pull({{true_param}})
  true_param <- true_param[1]

  # calculate sample stats
  t_bar <- mean(estimates)
  bias <- t_bar - true_param
  var_t <- var(estimates)
  s_t <- sd(estimates)
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4)
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3)

  mse <- bias^2 + var_t
  mse_mcse <- sqrt((1/K) * (s_t^4 * (k_t -1) + 4 * s_t^3 * g_t * bias + 4 * var_t * bias^2))

  # initialize data frame
  dat <- data.frame(matrix(ncol = 0, nrow = 1))

  if("bias" %in% perfm_criteria){
    dat$bias <- bias
    dat$bias_mcse <- sqrt(var_t / K)
  }

  if("variance" %in% perfm_criteria){
    dat$var <- var_t
    dat$var_mcse <- var_t * sqrt(((k_t - 1) / K))
  }


  if("mse" %in% perfm_criteria){
    dat$mse <- mse
    dat$mse_mcse <- mse_mcse
  }

  if("rmse" %in% perfm_criteria){
    dat$rmse <- sqrt(mse)
    dat$rmse_mcse <- sqrt((mse_mcse^2) / (4 * mse))
  }

  return(dat)

}
