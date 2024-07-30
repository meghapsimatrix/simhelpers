#' @title Calculate rejection rate and MCSE
#'
#' @description Calculates rejection rate. The function also calculates the
#'   associated Monte Carlo standard error.
#'
#' @param p_values vector or name of column from \code{data} containing
#'   p-values.
#' @param alpha scalar or vector indicating the nominal alpha level(s). Default
#'   value is set to the conventional .05.
#' @param format option \code{"wide"} (the default) will produce a tibble with
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
    p_values <- eval(cl$p_values, envir = data, enclos = parent.frame())
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


#' @title Extrapolate coverage and width using sub-sampled bootstrap confidence
#'   intervals.
#'
#' @description Given a set of bootstrap confidence intervals calculated across
#'   sub-samples with different numbers of replications, extrapolates confidence
#'   interval coverage and width of bootstrap confidence intervals to a
#'   specified (larger) number of bootstraps. The function also calculates the
#'   associated Monte Carlo standard errors. The confidence interval percentage
#'   is based on how you calculated the lower and upper bounds.
#'
#' @param pvalue_subsamples list or name of column from \code{data} containing list
#'   of confidence intervals calculated based on sub-samples with different
#'   numbers of replications.
#' @param B_target number of bootstrap replications to which the criteria should
#'   be extrapolated, with a default of \code{B = Inf}.
#' @param nested logical value controlling the format of the output. If
#'   \code{FALSE} (the default), then the results will be returned as a data
#'   frame with rows for each distinct number of bootstraps. If \code{TRUE},
#'   then the results will be returned as a data frame with a single row, with
#'   each performance criterion containing a nested data frame.
#' @param format character string controlling the format of the output when
#'   \code{CI_subsamples} has results for more than one type of confidence
#'   interval. If \code{"wide"} (the default), then each performance criterion
#'   will have a separate column for each CI type. If \code{"long"}, then each
#'   performance criterion will be a single variable, with separate rows for
#'   each CI type.
#' @inheritParams calc_rejection
#'
#' @return A tibble containing the number of simulation iterations, performance
#'   criteria estimate(s) and the associated MCSE.
#'
#' @export
#'
#' @references \insertRef{boos2000MonteCarloEvaluation}{simhelpers}
#'
#' @examples
#'

extrapolate_rejection <- function(
    data,
    pvalue_subsamples,
    B_target = Inf,
    alpha = .05,
    nested = FALSE,
    format = "wide"
) {

  format <- match.arg(format, choices = c("wide","long"))

  if (min(alpha) <= 0 | max(alpha) >= 1) stop("alpha must be larger than 0 and less than 1.")

  if (!missing(data)) {
    cl <- match.call()
    pvalue_subsamples <- eval(cl$pvalue_subsamples, envir = data, enclos = parent.frame())
  }

  K <- length(pvalue_subsamples) # number of iterations

  # check that all replications are same length
  rep_range <-
    sapply(pvalue_subsamples, \(x) lengths(x$pval)) |>
    apply(1, \(y) diff(range(y)))

  if (max(abs(rep_range)) != 0L) {
    msg <- paste0("All replications must use the same number of subsamples per B_val.",
                  "Smallest: ", min(rep_range), ". ",
                  "Largest: ", max(rep_range), ".")
    stop(msg)
  }

  # check that all replications use same B_vals
  B_reps <-
    sapply(pvalue_subsamples, \(x) x$bootstraps) |>
    apply(1, \(y) diff(range(y)))
  if (any(B_reps != 0L)) {
    stop("All replications must use the same set of B_vals.")
  }


  # calculate wts for each replication
  B_vals <- unique(pvalue_subsamples[[1]]$bootstraps)
  p <- length(B_vals)
  Btilde <- mean(1 / B_vals)
  x <- 1 / B_vals - Btilde
  S_B <- as.numeric(crossprod(x))
  B_wts <- 1 / p - x * (Btilde - 1 / B_target) / S_B

  # initialize results table
  dat <- data.frame(
    K_coverage = K
  )

  if (format == "wide") {
    dat$bootstraps <- list(c(B_vals, B_target))
  } else {
    dat$bootstraps <- list(data.frame(
      bootstraps = rep(c(B_vals, B_target), length(alpha)),
      alpha = rep(alpha, each = length(B_vals) + 1L)
    ))
  }

  alpha_digits <- max(nchar(as.character(alpha))) - 2L
  alpha_lab <- substr(formatC(alpha, format = "f", digits = alpha_digits), 3, 2 + alpha_digits)
  names(alpha) <- paste("reject", alpha_lab, sep = "_")

  pvalue_dat <- do.call(rbind, pvalue_subsamples)
  rejection_reps <- tapply(
    pvalue_dat$pval,
    pvalue_dat$bootstraps,
    summarize_rejections,
    alpha = alpha,
    simplify = FALSE
  )
  rejection_reps <- do.call(rbind, rejection_reps)
  bootstraps <- rep(B_vals, each = length(pvalue_subsamples))

  rej_rate_summary <- project_rejection_rate(rejection_reps)

  rej_rate_mcse <- sqrt(rej_rate * (1 - rej_rate) / K)

  if (format == "wide") {
    if (length(alpha) > 1L) {
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

  if (!nested) {
    dat_list <- unlist(dat, recursive = FALSE)
    dat_names <- lapply(names(dat), \(x) if (is.null(names(dat[[x]][[1]]))) x else paste(x, names(dat[[x]][[1]]), sep = "_"))
    dat <- do.call(cbind, dat_list)
    names(dat) <- unlist(dat_names)
    if (format == "long") names(dat)[2:3] <- c("bootstraps","CI_type")
  }

  return(dat)

}

summarize_rejections <- function(pvalues, alpha) {
  res <- lapply(alpha, \(a) sapply(pvalues, \(x) mean(x < a)))
  as.data.frame(res)
}

project_rejection(pvalues, B_wts) {}
