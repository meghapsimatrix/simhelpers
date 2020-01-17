#' Calculate jacknife Monte Carlo SE for variance estimators.
#'
#' @param res_dat A dataframe or tibble containing variance estimates.
#' @param estimates The column containing the variance estimates.
#' @param true_param The name of the column containing true parameters.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_jacknife(res_dat = alpha_res, estimates = var_est, true_param = true_param)
#'

calc_jacknife <- function(res_dat, estimates, true_param){

  require(dplyr)

  var_est <- res_dat %>% dplyr::pull({{estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% dplyr::pull({{true_param}})
  true_param <- true_param[1]


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
