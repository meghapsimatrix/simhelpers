#' @title Calculate relative performance criteria and MCSE
#'
#' @description Calculates relative bias, mean squared error (relative mse), and root mean squared error (relative rmse).
#' The function also calculates the associated
#' Monte Carlo standard errors.
#'
#' @param criteria character or character vector indicating the performance
#'   criteria to be calculated, with possible options \code{"relative bias"},
#'   \code{"relative mse"}, and \code{"relative rmse"}.
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_relative(data = t_res, estimates = est, true_param = true_param)
#'
#'

calc_relative <- function(
  data,
  estimates, true_param,
  criteria = c("relative bias", "relative mse", "relative rmse"),
  winz = Inf
) {

  criteria <- match.arg(criteria, choices = c("relative bias", "relative mse", "relative rmse"), several.ok = TRUE)
  abs_criteria <- substr(criteria, 10, 13)

  cl <- match.call()
  cl$criteria <- abs_criteria
  cl[[1]] <- quote(calc_absolute)
  abs_dat <- eval(cl, parent.frame())

  if (!missing(data)) {
    cl <- match.call()
    true_param <- eval(cl$true_param, envir = data, enclos = parent.frame())
  }

  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")

  # initialize tibble
  dat <- tibble::tibble(K_relative = abs_dat$K_absolute)

  if (winz < Inf) {
    dat$winsor_pct <- abs_dat$winsor_pct
    dat$winsor_pct_mcse <- abs_dat$winsor_pct_mcse
  }

  if ("relative bias" %in% criteria) {
    dat$rel_bias <- ifelse(true_param == 0, NA_real_, 1 + abs_dat$bias / true_param)
    dat$rel_bias_mcse <- ifelse(true_param == 0, NA_real_, abs_dat$bias_mcse / true_param)
  }

  if ("relative mse" %in% criteria) {
    dat$rel_mse <- ifelse(true_param == 0, NA_real_, abs_dat$mse / true_param^2)
    dat$rel_mse_mcse <- ifelse(true_param == 0, NA_real_, abs_dat$mse_mcse / true_param^2)
  }

  if ("relative rmse" %in% criteria) {
    dat$rel_rmse <- ifelse(true_param == 0, NA_real_, abs_dat$rmse / true_param)
    dat$rel_rmse_mcse <- ifelse(true_param == 0, NA_real_, abs_dat$rmse_mcse / true_param)
  }

  return(dat)

}

