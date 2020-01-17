#' Calculate performance criteria and MCSE.
#'
#' @param res_dat A dataframe or tibble containing simulation results.
#' @param p_values The name of the column containing p values.
#' @param alpha A number indicating the nominal alpha level.
#'
#' @return The performance criteria estimate and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_rr(res_dat = t_res, p_values = p_val)
#'
#'


calc_rr <- function(res_dat, p_values, alpha = .05){

  require(dplyr)

  p_vals <- res_dat %>% pull({{p_values}})
  K <- nrow(res_dat)

  dat <- data.frame(rej_rate = mean(p_vals < alpha))
  dat$rej_rate_mcse <- sqrt((dat$rej_rate * (1 - dat$rej_rate)) / K)

  return(dat)

}
