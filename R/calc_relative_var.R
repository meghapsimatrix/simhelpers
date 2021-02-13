#' Calculate jack-knife Monte Carlo SE for variance estimators
#'
#' @description Calculates relative bias, mean squared error (relative mse), and root mean
#' squared error (relative rmse)  of variance estimators.
#' The function also calculates the associated jack-knife Monte Carlo standard errors.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param estimates name of the column containing the estimates.
#' @param var_estimates name of the column containing the variance estimates.
#' @param perfm_criteria character or character vector indicating the performance criteria to be calculated.
#'
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_relative_var(res_dat = alpha_res, estimates = A, var_estimates = Var_A)
#'

calc_relative_var <- function(res_dat, estimates, var_estimates, perfm_criteria = c("relative bias", "relative mse", "relative rmse")){

  res_dat <- res_dat %>%
    dplyr::select({{estimates}}, {{var_estimates}}) %>%
    dplyr::filter(stats::complete.cases(.))

  estimates <- res_dat %>%
    dplyr::pull({{estimates}}) # point estimates

  var_est <- res_dat %>%
    dplyr::pull({{var_estimates}}) # point estimates

  K <- length(var_est) # iterations

  # calculate sample stats
  v_bar <- mean(var_est) # sample mean of variance estimator
  t_bar <- mean(estimates) # sample mean of the estimates

  var_v <- var(var_est) # variance of variance estimates
  var_t <- var(estimates) # sample variance of the estimates

  # jack-knife
  v_bar_j <- (1 / (K - 1)) * (K * v_bar - var_est)  # jack-knife mean of var estimates
  s_sq_t_j <- (1 / (K - 2)) * ((K - 1) * var_t - (K / (K - 1)) * (estimates - t_bar)^2) # jack-knife var of point estimates
  s_sq_v_j <- (1 / (K - 2)) * ((K - 1) * var_v - (K / (K - 1)) * (var_est - v_bar)^2) # jack-knife var of var estimates

  rb_var <- v_bar/ var_t # reliative bias of variance estimates
  rel_mse_var <- ((v_bar - var_t)^2 + var_v) /  var_t^2
  rel_mse_var_j <- ((v_bar_j - s_sq_t_j)^2 + s_sq_v_j)/(s_sq_t_j)^2 # jack-knife relative mse of var estimates

  # initialize tibble
  dat <- tibble::tibble(K = K)


  if("relative bias" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rel_bias_var = rb_var,
                    rel_bias_var_mcse = sqrt(((K - 1)/K) * sum((v_bar_j/s_sq_t_j - rb_var)^2)),
                    rel_bias_var = dplyr::if_else(var_t == 0, as.numeric(NA), rel_bias_var),
                    rel_bias_var_mcse = dplyr::if_else(var_t== 0, as.numeric(NA), rel_bias_var_mcse))
  }

  if("relative mse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rel_mse_var = rel_mse_var,
                    rel_mse_var_mcse = sqrt(((K - 1)/K) * sum((rel_mse_var_j - rel_mse_var)^2)),
                    rel_mse_var = dplyr::if_else(var_t == 0, as.numeric(NA), rel_mse_var),
                    rel_mse_var_mcse = dplyr::if_else(var_t == 0, as.numeric(NA), rel_mse_var_mcse))
  }

  if("relative rmse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rel_rmse_var = sqrt(rel_mse_var),
                    rel_rmse_var_mcse = sqrt(((K - 1)/K) * sum((sqrt(rel_mse_var_j) - rel_rmse_var)^2)),
                    rel_rmse_var = dplyr::if_else(var_t == 0, as.numeric(NA), rel_rmse_var),
                    rel_rmse_var_mcse = dplyr::if_else(var_t == 0, as.numeric(NA), rel_rmse_var_mcse))
  }



  return(dat)

}
