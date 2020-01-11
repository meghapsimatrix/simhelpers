#' Calculate performance criteria and MCSE.
#'
#' @param cov_dat A dataframe or tibble containing two columns called lower_bound and upper_bound containing the confindence interval lower and upper bounds.
#' @param true_param A number indicating the true parameter.
#' @param alpha A number indicating the nominal alpha level.
#' @param K A number indicating number of simulation iterations.
#'  @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_coverage(cov_dat = t_res, true_param = .5, K = nrow(t_res))
#'
#'

calc_coverage <- function(cov_dat, true_param, alpha = .05, K, perfm_criteria = c("coverage", "width")){

  lower_bound <- cov_dat$lower_bound
  upper_bound <- cov_dat$upper_bound

  # initialize data frame
  dat <- data.frame(matrix(ncol = 0, nrow = 1))

  if("coverage" %in% perfm_criteria){
    dat$coverage <- mean(lower_bound < true_param & true_param < upper_bound)
    dat$coverage_mcse <- sqrt((dat$coverage * (1 - dat$coverage)) / K)
  }

  if("width" %in% perfm_criteria){
    dat$width <- mean(upper_bound) - mean(lower_bound)
    dat$width_mcse <- sqrt(var(upper_bound - lower_bound) / K)
  }

  return(dat)

}
