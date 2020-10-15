#' @title Calculate rejection rate and MCSE
#'
#' @description Calculates rejection rate. The function also calculates the associated
#' Monte Carlo standard error.
#'
#' @param res_dat data frame or tibble containing the simulation results.
#' @param p_values name of the column containing the p-values.
#' @param alpha number indicating the nominal alpha level. Default value is set to the conventional .05.
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_rejection(res_dat = t_res, p_values = p_val)
#'
#'


calc_rejection <- function(res_dat, p_values, alpha = .05){

  p_vals <- res_dat %>%
    dplyr::filter(!is.na({{p_values}})) %>% # p values
    dplyr::pull({{p_values}}) # p values

  K <- length(p_vals) # number of iterations

  dat <- tibble::tibble(K = K,
                        rej_rate = mean(p_vals < alpha),
                        rej_rate_mcse = sqrt((rej_rate * (1 - rej_rate)) / K))

  return(dat)

}
