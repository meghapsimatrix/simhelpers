#' Calculate performance criteria and MCSE.
#'
#' @param dat A dataframe or tibble containing a column called est - estimates
#' @param true_param A number indicating the true parameter.
#' @param K A number indicating number of simulation iterations.
#' @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_relative(dat = t_res, true_param = .5, K = nrow(t_res))
#'
#'

calc_relative <- function(dat, true_param, K, perfm_criteria = c("relative bias", "relative mse")){

  estimates <- dat$est

  #calculate sample stats
  t_bar <- mean(estimates)
  bias <- t_bar - true_param
  var_t <- var(estimates)
  s_t <- sd(estimates)
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4)
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3)
  mse <- bias^2 + var_t

  # initialize data frame
  dat <- data.frame(matrix(ncol = 0, nrow = 1))

  if("relative bias" %in% perfm_criteria){
    dat$rel_bias <- t_bar / true_param
    dat$rel_bias_mcse <- sqrt(var_t / (K * true_param^2))
  }

  if("relative mse" %in% perfm_criteria){
    dat$rel_mse <- mse / (true_param^2)
    dat$rel_mse_mcse <- sqrt((1 / K * true_param^2) * (s_t^4 * (k_t -1) + 4 * s_t^3 * g_t * bias + 4 * var_t * bias^2))
  }

  return(dat)

}
