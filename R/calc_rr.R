#' @title Calculate rejection rate and MCSE.
#'
#' @description Calculates rejection rate. The function also calculates the associated
#' Monte Carlo Standard error.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param p_values name of the column containing the p values.
#' @param alpha number indicating the nominal alpha level. Default value is set to the conventional .05.
#'
#' @return A tibble containing the performance criteria estimate and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_rr(res_dat = t_res, p_values = p_val)
#'
#'


calc_rr <- function(res_dat, p_values, alpha = .05){

  require(dplyr)
  require(tibble)

  p_vals <- res_dat %>% pull({{p_values}})
  K <- nrow(res_dat)

  dat <- tibble(rej_rate = mean(p_vals < alpha),
                rej_rate_mcse = sqrt((rej_rate * (1 - rej_rate)) / K))

  return(dat)

}
