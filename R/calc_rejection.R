#' @title Calculate rejection rate and MCSE
#'
#' @description Calculates rejection rate. The function also calculates the
#'   associated Monte Carlo standard error.
#'
#' @param p_values Vector or name of column from \code{data} containing
#'   p-values.
#' @param alpha Scalar or vector indicating the nominal alpha level(s). Default
#'   value is set to the conventional .05.
#' @param format Option \code{"wide"} (the default) will produce a tibble with
#'   one row, with separate variables for each specified \code{alpha}. Option
#'   \code{"long"} will produce a tibble with one row per specified
#'   \code{alpha}.
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance
#'   criteria estimate and the associated MCSE.
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
  alpha = .05,
  format = "wide"
) {

  format <- match.arg(format, c("wide","long"))
  if (min(alpha) <= 0 | max(alpha) >= 1) stop("alpha must be larger than 0 and less than 1.")

  if (!missing(data)) {
    cl <- match.call()
    p_values <- eval(cl$p_values, envir = data)
  }

  p_values <- p_values[!is.na(p_values)]

  K <- length(p_values) # number of iterations

  rej_rate <- sapply(alpha, \(x) mean(p_values < x))
  rej_rate_mcse <- sqrt(rej_rate * (1 - rej_rate) / K)

  if (format == "wide") {
    if (length(alpha) > 1L) {
      alpha_digits <- max(nchar(as.character(alpha))) - 2L
      alpha_lab <- substr(formatC(alpha, format = "f", digits = alpha_digits), 3, 2 + alpha_digits)
      var_names <- c("K_rejection", paste("rej_rate", alpha_lab, sep = "_"), paste("rej_rate_mcse", alpha_lab, sep = "_"))
    } else {
      var_names <- c("K_rejection","rej_rate","rej_rate_mcse")
    }
    dat <- as.data.frame(c(list(K = K), rej_rate = rej_rate, rej_rate_mcse = rej_rate_mcse))
    names(dat) <- var_names
  } else if (format == "long") {
    dat <- tibble::tibble(
      K_rejection = K,
      alpha = alpha,
      rej_rate = rej_rate,
      rej_rate_mcse = rej_rate_mcse
    )
  }

  return(dat)

}
