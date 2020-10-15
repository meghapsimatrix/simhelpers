#' @title Calculate confidence interval coverage, width and MCSE
#'
#' @description Calculates confidence interval coverage and width. The function also calculates the associated
#' Monte Carlo standard errors. The confidence interval percentage is based on how you calculated the lower
#' and upper bounds.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param lower_bound name of the column containing the lower bound estimates of the confidence intervals.
#' @param upper_bound name of the column containing the upper bound estimates of the confidence intervals.
#' @param true_param name of the column containing the true parameters.
#' @param perfm_criteria character or character vector indicating the performance criteria to be calculated.
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_coverage(res_dat = t_res, lower_bound = lower_bound,
#'               upper_bound = upper_bound, true_param = true_param)
#'
#'

calc_coverage <- function(res_dat, lower_bound, upper_bound, true_param, perfm_criteria = c("coverage", "width")){

  true_param <- res_dat %>% dplyr::pull({{true_param}}) # true parameters
  true_param <- true_param[1] # true param

  res_dat <- res_dat %>%
    dplyr::select({{upper_bound}}, {{lower_bound}}) %>%
    dplyr::filter(stats::complete.cases(.))

  lower_bound <- res_dat %>%
    dplyr::pull({{lower_bound}})  # lower bounds of cis

  upper_bound <- res_dat %>%
    dplyr::pull({{upper_bound}}) # upper bounds of cis

  K <- length(lower_bound) # iterations


  # initialize tibble
  dat <- tibble::tibble(K = K)

  if("coverage" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(coverage = mean(lower_bound <= true_param & true_param <= upper_bound),
                    coverage_mcse = sqrt((coverage * (1 - coverage)) / K))

  }

  if("width" %in% perfm_criteria){
    dat <- dat %>%
      dplyr::mutate(width = mean(upper_bound) - mean(lower_bound),
                    width_mcse = sqrt(var(upper_bound - lower_bound) / K))

  }

  return(dat)

}
