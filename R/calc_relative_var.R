#' Calculate jack-knife Monte Carlo SE for variance estimators
#'
#' @description Calculates relative bias, mean squared error (relative mse), and root mean
#' squared error (relative rmse)  of variance estimators.
#' The function also calculates the associated jack-knife Monte Carlo standard errors.
#'
#' @param var_estimates Vector or name of column from \code{data} containing variance estimates for point estimator in \code{estimates}.
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#'
#' @export
#'
#' @examples
#' calc_relative_var(data = alpha_res, estimates = A, var_estimates = Var_A)
#'
#' @importFrom stats var


calc_relative_var <- function(
  data,
  estimates, var_estimates,
  criteria = c("relative bias", "relative mse", "relative rmse")
) {

  if (!missing(data)) {
    cl <- match.call()
    estimates <- eval(cl$estimates, envir = data)
    var_estimates <- eval(cl$var_estimates, envir = data)
  }
  not_miss <- !is.na(estimates) & !is.na(var_estimates)
  estimates <- estimates[not_miss]
  var_est <- var_estimates[not_miss]


  # calculate sample stats
  K <- length(var_est) # iterations
  v_bar <- mean(var_est) # sample mean of variance estimator
  t_bar <- mean(estimates) # sample mean of the estimates

  var_v <- var(var_est) # variance of variance estimates
  var_t <- var(estimates) # sample variance of the estimates

  # jack-knife
  v_bar_j <- (K * v_bar - var_est) / (K - 1)  # jack-knife mean of var estimates
  s_sq_t_j <- ((K - 1) * var_t - (estimates - t_bar)^2 * K / (K - 1)) / (K - 2) # jack-knife var of point estimates
  s_sq_v_j <- ((K - 1) * var_v - (var_est - v_bar)^2 * K / (K - 1)) / (K - 2) # jack-knife var of var estimates

  rb_var <- v_bar/ var_t # reliative bias of variance estimates
  rel_mse_var <- ((v_bar - var_t)^2 + var_v) /  var_t^2
  rel_mse_var_j <- ((v_bar_j - s_sq_t_j)^2 + s_sq_v_j) / (s_sq_t_j)^2 # jack-knife relative mse of var estimates

  # initialize tibble
  dat <- tibble::tibble(K_relvar = K)

  if ("relative bias" %in% criteria) {
    dat$rel_bias_var <- ifelse(var_t == 0, NA_real_, rb_var)
    dat$rel_bias_var_mcse <- ifelse(
      var_t == 0,
      NA_real_,
      sqrt(sum((v_bar_j / s_sq_t_j - rb_var)^2) * (K - 1) / K)
    )
  }

  if ("relative mse" %in% criteria) {
    dat$rel_mse_var <- ifelse(var_t == 0, NA_real_, rel_mse_var)
    dat$rel_mse_var_mcse <- ifelse(
      var_t == 0,
      NA_real_,
      sqrt(sum((rel_mse_var_j - rel_mse_var)^2) * (K - 1) / K)
    )
  }

  if ("relative rmse" %in% criteria) {
    rel_rmse_var <- sqrt(rel_mse_var)
    dat$rel_rmse_var <- ifelse(var_t == 0, NA_real_, rel_rmse_var)
    dat$rel_rmse_var_mcse <- ifelse(
      var_t == 0,
      NA_real_,
      sqrt(sum((sqrt(rel_mse_var_j) - rel_rmse_var)^2) * (K - 1) / K)
    )
  }

  return(dat)
}
