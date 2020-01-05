#' Calculate performance criteria and MCSE.
#'
#' @param p_vals A numeric vector including the estimates.
#' @param alpha A number indicating the nominal alpha level.
#' @param K A number indicating number of simulation iterations.
#'
#' @return The performance criteria estimate and the associated MCSE.


#' @export
calc_rr <- function(p_vals, alpha = .05, K){

  dat <- data.frame(rej_rate = mean(p_vals < alpha))
  dat$rej_rate_mcse <- sqrt((dat$rej_rate * (1 - dat$rej_rate)) / K)

  return(dat)

}
