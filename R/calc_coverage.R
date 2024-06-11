#' @title Calculate confidence interval coverage, width and MCSE
#'
#' @description Calculates confidence interval coverage and width. The function also calculates the associated
#' Monte Carlo standard errors. The confidence interval percentage is based on how you calculated the lower
#' and upper bounds.
#'
#' @param lower_bound Vector or name of column from \code{data} containing lower bounds of confidence intervals.
#' @param upper_bound Vector or name of column from \code{data} containing upper bounds of confidence intervals.
#' @param criteria character or character vector indicating the performance
#'   criteria to be calculated, with possible options \code{"coverage"} and \code{"width"}.
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_coverage(data = t_res, lower_bound = lower_bound,
#'               upper_bound = upper_bound, true_param = true_param)
#'
#'

calc_coverage <- function(
  data,
  lower_bound, upper_bound,
  true_param,
  criteria = c("coverage", "width"),
  winsorize = Inf
) {

  criteria <- match.arg(criteria, choices = c("coverage", "width"), several.ok = TRUE)

  if (!missing(data)) {
    cl <- match.call()
    true_param <- eval(cl$true_param, envir = data)
    lower_bound <- eval(cl$lower_bound, envir = data)
    upper_bound <- eval(cl$upper_bound, envir = data)
  }

  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")

  not_miss <- !is.na(lower_bound) & !is.na(upper_bound)
  lower_bound <- lower_bound[not_miss]
  upper_bound <- upper_bound[not_miss]

  K <- length(lower_bound) # iterations
  width <- upper_bound - lower_bound

  if (winsorize < Inf) {
    quartiles <- quantile(width, c(.25, .75))
    IQR <- diff(quartiles)
    trunc_points <- quartiles + c(-1, 1) * winsorize * IQR
    winsorization_pct <- mean((width < trunc_points[1]) | (width > trunc_points[2]))
    width <- pmax(pmin(width, trunc_points[2]), trunc_points[1])
  }

  # initialize tibble
  dat <- tibble::tibble(K_coverage = K)

  if (winsorize < Inf) {
    dat$width_winsor_pct <- winsorization_pct
    dat$width_winsor_pct_mcse <- sqrt(winsorization_pct * (1 - winsorization_pct) / K)
  }

  if ("coverage" %in% criteria) {
    coverage <- mean(lower_bound <= true_param & true_param <= upper_bound)
    dat$coverage <- coverage
    dat$coverage_mcse = sqrt(coverage * (1 - coverage) / K)
  }

  if ("width" %in% criteria) {
    dat$width <- mean(width)
    dat$width_mcse <- sqrt(var(width) / K)
  }

  return(dat)

}
