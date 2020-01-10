#' Calculate performance criteria and MCSE.
#'
#' @param rr_dat A dataframe or tibble containing a column called p_vals containing p values
#' @param alpha A number indicating the nominal alpha level.
#' @param K A number indicating number of simulation iterations.
#'
#' @return The performance criteria estimate and the associated MCSE.


#' @export
calc_rr <- function(rr_dat, alpha = .05, K){

  p_vals <- rr_dat$p_val

  dat <- data.frame(rej_rate = mean(p_vals < alpha))
  dat$rej_rate_mcse <- sqrt((dat$rej_rate * (1 - dat$rej_rate)) / K)

  return(dat)

}
