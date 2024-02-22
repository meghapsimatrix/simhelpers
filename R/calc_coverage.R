#' @title Calculate confidence interval coverage, width and MCSE
#'
#' @description Calculates confidence interval coverage and width. The function also calculates the associated
#' Monte Carlo standard errors. The confidence interval percentage is based on how you calculated the lower
#' and upper bounds.
#'
#' @param lower_bound Vector or name of column from \code{data} containing lower bounds of confidence intervals.
#' @param upper_bound Vector or name of column from \code{data} containing upper bounds of confidence intervals.
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
  criteria = c("coverage", "width")
) {

  if (!missing(data)) {
    cl <- match.call()
    lower_bound <- eval(cl$lower_bound, envir = data)
    upper_bound <- eval(cl$upper_bound, envir = data)
    true_param <- eval(cl$true_param, envir = data)
  }
  not_miss <- !is.na(lower_bound) & !is.na(upper_bound)
  lower_bound <- lower_bound[not_miss]
  upper_bound <- upper_bound[not_miss]

  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")

  K <- length(lower_bound) # iterations

  # initialize tibble
  dat <- tibble::tibble(K_coverage = K)

  if ("coverage" %in% criteria) {
    coverage <- mean(lower_bound <= true_param & true_param <= upper_bound)
    dat$coverage <- coverage
    dat$coverage_mcse = sqrt(coverage * (1 - coverage) / K)
  }

  if ("width" %in% criteria) {
    width <- upper_bound - lower_bound
    dat$width <- mean(width)
    dat$width_mcse <- sqrt(var(width) / K)
  }

  return(dat)

}
