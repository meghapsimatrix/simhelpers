#' @title Calculate relative performance criteria and MCSE.
#'
#' @description Calculates relative bias, mean squared error (relative mse), and root mean squared error (relative rmse).
#' The function also calculates the associated
#' Monte Carlo Standard errors.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param estimates name of the column containing the estimates
#' @param true_param name of the column containing the true parameters.
#' @param perfm_criteria character or character vector indicating the performance criteria to be calculated.
#'
#' @return A tibble containing the performance criteria estimate(s) and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_relative(res_dat = t_res, estimates = est, true_param = true_param)
#'
#'

calc_relative <- function(res_dat, estimates, true_param, perfm_criteria = c("relative bias", "relative mse", "relative rmse")){

  estimates <- res_dat %>% dplyr::pull({{estimates}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% dplyr::pull({{true_param}})
  true_param <- true_param[1]

  #calculate sample stats
  t_bar <- mean(estimates)
  bias <- t_bar - true_param
  var_t <- var(estimates)
  s_t <- sd(estimates)
  k_t <- (1/(K * s_t^4)) * sum((estimates - t_bar)^4)
  g_t <- (1/(K * s_t^3)) * sum((estimates - t_bar)^3)

  # jacknife
  t_bar_j <- (1/(K - 1)) * (K * t_bar - estimates)
  bias_j_sq <- (t_bar_j - true_param)^2
  s_sq_t_j <- (1 / (K - 2)) * ((K - 1) * var_t - (K / (K - 1)) * (estimates - t_bar)^2)


  mse <- bias^2 + var_t
  rel_mse <- mse / (true_param^2)
  rel_mse_j <- ((t_bar_j - true_param)^2 + s_sq_t_j)/(true_param)^2


  # initialize tibble
  dat <- tibble::as_tibble(data.frame(matrix(ncol = 0, nrow = 1)))

  if("relative bias" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rel_bias = t_bar / true_param,
                    rel_bias_mcse = sqrt(var_t / (K * true_param^2)),
                    rel_bias = dplyr::if_else(true_param == 0, as.numeric(NA), rel_bias),
                    rel_bias_mcse = dplyr::if_else(true_param == 0, as.numeric(NA), rel_bias_mcse))
  }

  if("relative mse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rel_mse = rel_mse,
                    rel_mse_mcse = sqrt((1 / K * true_param^2) * (s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * bias + 4 * var_t * bias^2)),
                    rel_mse = dplyr::if_else(true_param == 0, as.numeric(NA), rel_mse),
                    rel_mse_mcse = dplyr::if_else(true_param == 0, as.numeric(NA), rel_mse_mcse))
  }

  if("relative rmse" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(rel_rmse = sqrt(rel_mse),
                    rel_rmse_mcse = sqrt((1/K) * sum((sqrt(rel_mse_j) - rel_rmse)^2)),
                    rel_rmse = dplyr::if_else(true_param == 0, as.numeric(NA), rel_rmse),
                    rel_rmse_mcse = dplyr::if_else(true_param == 0, as.numeric(NA), rel_rmse_mcse))
  }

  return(dat)

}
