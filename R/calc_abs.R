#' Calculate performance criteria and MCSE.
#'
#' @param dat A dataframe or tibble containing a column called est - estimates
#' @param true_param A number indicating the true parameter.
#' @param K A number indicating number of simulation iterations.
#' @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return The performance criteria estimate and the associated MCSE.


#' @export
calc_abs <- function(dat, true_param, K, perfm_criteria = c("bias", "variance", "mse", "rmse")){

  estimates <- dat$est

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
