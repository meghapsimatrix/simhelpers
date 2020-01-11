#' Calculate jacknife Monte Carlo SE for variance estimators.
#'
#' @param dat A dataframe or tibble containing a column called var_est - variance estimates.
#' @param true_param A number indicating the true parameter.
#' @param K A number indicating number of simulation iterations.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_jacknife(var_dat = alpha_res, true_param = .8, K = nrow(alpha_res))
#'

calc_jacknife <- function(var_dat, true_param, K){

  var_est <- var_dat$var_est

  # calculate sample stats
  v_bar <- mean(var_est)
  v_var <- var(var_est)
  V_bar_j <- (1 / (K - 1)) * (K * v_bar - var_est)
  S_sq_t_j <- (1 / (K - 2)) * ((K-1) * var(var_est) - (K / (K-1)) * (var_est - v_bar)^2)
  T_bar_j <- (1 / (K - 1)) * (K * v_bar - var_est)
  RB_var <- v_bar/ v_var
  rmse_j <- sqrt((T_bar_j - true_param)^2 + ((K - 1) / K) * S_sq_t_j)
  rmse <- sqrt(mean((var_est - true_param)^2))

  # initialize data frame
  dat <- data.frame(matrix(ncol = 0, nrow = 1))


  dat$rbv <- RB_var
  dat$rbv_mcse <- sqrt(v_var / K)
  dat$rbv_jack_mcse <- sqrt((1/K) * sum((V_bar_j/S_sq_t_j - RB_var)^2))
  dat$rmsev <- rmse
  dat$rmsev_jack_mcse <- sqrt((1/K) * sum((rmse_j - rmse)^2))


  return(dat)

}
