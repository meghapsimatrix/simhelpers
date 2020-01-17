#' Calculate performance criteria and MCSE.
#'
#' @param res_dat A dataframe or tibble containing confidence interval results.
#' @param lower_bound The column containing the lower bound estimates of the confidence intervals.
#' @param upper_bound The column containing the upper bound estimates of the confidence intervals.
#' @param true_param A number indicating the true parameter.
#' @param alpha A number indicating the nominal alpha level.
#' @param perfm_criteria A character or a character vector indicating the performance criteria to be calculated.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_coverage(res_dat = t_res, lower_bound = lower_bound, upper_bound = upper_bound, true_param = .5)
#'
#'

calc_coverage <- function(res_dat, lower_bound, upper_bound, true_param, alpha = .05, perfm_criteria = c("coverage", "width")){

  lower_bound <- res_dat %>% dplyr::pull({{lower_bound}})
  upper_bound <- res_dat %>% dplyr::pull({{upper_bound}})

  K <- nrow(res_dat)

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
