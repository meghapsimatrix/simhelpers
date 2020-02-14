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

calc_jacknife <- function(res_dat, estimates, var_estimates, true_param){

  require(dplyr)
  require(tibble)

  est <- res_dat %>% dplyr::pull({{estimates}})
  var_est <- res_dat %>% dplyr::pull({{var_estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% dplyr::pull({{true_param}})
  true_param <- true_param[1]


  # calculate sample stats
  v_bar <- mean(var_est) # sample mean of variance estimator
  v_var <- var(var_est)
  t_bar <- mean(est)
  t_var <- var(est)
  V_bar_j <- (1 / (K - 1)) * (K * v_bar - var_est)
  S_sq_t_j <- (1 / (K - 2)) * ((K-1) * t_var - (K / (K-1)) * (est - t_bar)^2)
  # T_bar_j <- (1 / (K - 1)) * (K * v_bar - var_est)
  RB_var <- v_bar/ v_var
  rmse_j <- sqrt((t_bar - true_param)^2 + ((K - 1) / K) * S_sq_t_j)
  rmse <- sqrt(mean((var_est - true_param)^2))

  # initialize data frame
  dat <- tibble(
    rbv = RB_var,
    rbv_mcse = sqrt(v_var / K),
    rbv_jack_mcse = sqrt((1/K) * sum((V_bar_j/S_sq_t_j - RB_var)^2)),
    rmsev = rmse,
    rmsev_jack_mcse = sqrt((1/K) * sum((rmse_j - rmse)^2))
  )


  return(dat)

}
