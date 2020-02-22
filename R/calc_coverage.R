#' @title Calculate confidence interval coverage, width and MCSE.
#'
#' @description Calculates confidence interval coverage and width. The function also calculates the associated
#' Monte Carlo Standard error.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param lower_bound name of the column containing the lower bound estimates of the confidence intervals.
#' @param upper_bound name of the column containing the upper bound estimates of the confidence intervals.
#' @param true_param name of the column containing the true parameters.
#' @param alpha number indicating the nominal alpha level. Default value is set to the conventional .05.
#' @param perfm_criteria character or character vector indicating the performance criteria to be calculated.
#'
#' @return A tibble containing the  performance criteria estimate(s) and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_coverage(res_dat = t_res, lower_bound = lower_bound, upper_bound = upper_bound, true_param = true_param)
#'
#'

calc_coverage <- function(res_dat, lower_bound, upper_bound, true_param, alpha = .05, perfm_criteria = c("coverage", "width")){

  lower_bound <- res_dat %>% dplyr::pull({{lower_bound}})
  upper_bound <- res_dat %>% dplyr::pull({{upper_bound}})
  K <- nrow(res_dat)
  true_param <- res_dat %>% dplyr::pull({{true_param}})
  true_param <- true_param[1]


  # initialize tibble
  dat <- tibble::as_tibble(data.frame(matrix(ncol = 0, nrow = 1)))

  if("coverage" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(coverage = mean(lower_bound < true_param & true_param < upper_bound),
             coverage_mcse = sqrt((coverage * (1 - coverage)) / K))

  }

  if("width" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(width = mean(upper_bound) - mean(lower_bound),
             width_mcse = sqrt(var(upper_bound - lower_bound) / K))

  }

  return(dat)

}
