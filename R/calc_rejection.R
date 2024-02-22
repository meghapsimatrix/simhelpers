#' @title Calculate rejection rate and MCSE
#'
#' @description Calculates rejection rate. The function also calculates the associated
#' Monte Carlo standard error.
#'
#' @param p_values Vector or name of column from \code{data} containing p-values.
#' @param alpha Scalar indicating the nominal alpha level. Default value is set to the conventional .05.
#' @inherit calc_absolute params
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_rejection(data = t_res, p_values = p_val)
#'
#'


calc_rejection <- function(
  data,
  p_values,
  alpha = .05
) {

  if (!missing(data)) {
    cl <- match.call()
    p_values <- eval(cl$p_values, envir = data)
  }

  p_values <- p_values[!is.na(p_values)]

  K <- length(p_values) # number of iterations

  dat <- tibble::tibble(
    K = K,
    rej_rate = mean(p_values < alpha),
    rej_rate_mcse = sqrt(rej_rate * (1 - rej_rate) / K)
  )

  return(dat)

}
