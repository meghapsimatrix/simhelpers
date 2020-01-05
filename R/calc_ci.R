#' Calculate performance criteria and MCSE.
#'
#' @param lower_bound A numeric vector indicating the lower bound for confidence interval coverage and width calculations.
#' @param upper_bound A numeric vector indicating the upper bound for confidence interval coverage and width calculations.
#' @param true_param A number indicating the true parameter.
#' @param alpha A number indicating the nominal alpha level.
#' @param K A number indicating number of simulation iterations.
#'  @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return The performance criteria estimate and the associated MCSE.


#' @export
calc_ci <- function(lower_bound = NULL, upper_bound = NULL, true_param, alpha = .05, K, perfm_criteria = c("coverage", "width")){

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
