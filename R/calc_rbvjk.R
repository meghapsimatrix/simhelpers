#' Calculate jacknife Monte Carlo SE for variance estimators.
#'
#'#' @description Calculates relative bias of variance estimators.
#' The function also calculates the associated jacknife Monte Carlo Standard errors.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param estimates name of the column containing the estimates.
#' @param var_estimates name of the column containing the variance estimates.
#' @param true_param name of the column containing true parameters of the point estimator.
#'
#' @return A tibble containing the performance criteria estimate and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_rbvjk(res_dat = alpha_res, estimates = A, var_estimates = Var_A, true_param = true_param)
#'

calc_rbvjk <- function(res_dat, estimates, var_estimates, true_param){


  estimates <- res_dat %>% dplyr::pull({{estimates}})
  var_est <- res_dat %>% dplyr::pull({{var_estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% dplyr::pull({{true_param}})
  true_param <- true_param[1]


  # calculate sample stats
  v_bar <- mean(var_est) # sample mean of variance estimator
  t_bar <- mean(estimates) # sample mean of the estimates
  var_t <- var(estimates) # sample variance of the estiates
  v_bar_j <- (1 / (K - 1)) * (K * v_bar - var_est)
  s_sq_t_j <- (1 / (K - 2)) * ((K - 1) * var_t - (K / (K - 1)) * (estimates - t_bar)^2)
  rb_var <- v_bar/ var_t

  # initialize data frame
  dat <- tibble::tibble(
    rbv = rb_var,
    rbv_jack_mcse = sqrt((1/K) * sum((v_bar_j/s_sq_t_j - rb_var)^2)),
  )

  return(dat)

}
