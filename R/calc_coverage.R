#' @title Calculate confidence interval coverage, width and MCSE
#'
#' @description Calculates confidence interval coverage and width. The function also calculates the associated
#' Monte Carlo standard errors. The confidence interval percentage is based on how you calculated the lower
#' and upper bounds.
#'
#' @param lower_bound vector or name of column from \code{data} containing lower bounds of confidence intervals.
#' @param upper_bound vector or name of column from \code{data} containing upper bounds of confidence intervals.
#' @param criteria character or character vector indicating the performance
#'   criteria to be calculated, with possible options \code{"coverage"} and \code{"width"}.
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance criteria estimate(s)
#' and the associated MCSE.
#'
#' @export
#'
#' @examples
#' calc_coverage(data = t_res, lower_bound = lower_bound,
#'               upper_bound = upper_bound, true_param = true_param)
#'
#'

calc_coverage <- function(
  data,
  lower_bound, upper_bound,
  true_param,
  criteria = c("coverage", "width"),
  winz = Inf
) {

  criteria <- match.arg(criteria, choices = c("coverage", "width"), several.ok = TRUE)

  if (!missing(data)) {
    cl <- match.call()
    true_param <- eval(cl$true_param, envir = data, enclos = parent.frame())
    lower_bound <- eval(cl$lower_bound, envir = data, enclos = parent.frame())
    upper_bound <- eval(cl$upper_bound, envir = data, enclos = parent.frame())
  }

  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")

  not_miss <- !is.na(lower_bound) & !is.na(upper_bound)
  lower_bound <- lower_bound[not_miss]
  upper_bound <- upper_bound[not_miss]

  K <- length(lower_bound) # iterations
  width <- upper_bound - lower_bound

  if (winz < Inf) width <- winsorize(width, winz)

  # initialize tibble
  dat <- tibble::tibble(K_coverage = K)

  if (winz < Inf) {
    dat$width_winsor_pct <- attr(width, "winsor_pct")
    dat$width_winsor_pct_mcse <- sqrt(dat$width_winsor_pct * (1 - dat$width_winsor_pct) / K)
  }

  if ("coverage" %in% criteria) {
    coverage <- mean(lower_bound <= true_param & true_param <= upper_bound)
    dat$coverage <- coverage
    dat$coverage_mcse = sqrt(coverage * (1 - coverage) / K)
  }

  if ("width" %in% criteria) {
    dat$width <- mean(width)
    dat$width_mcse <- sqrt(var(width) / K)
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
#' @param CI_subsamples list or name of column from \code{data} containing list
#'   of confidence intervals calculated based on sub-samples with different
#'   numbers of replications.
#' @param criteria character or character vector indicating the performance
#'   criteria to be calculated, with possible options \code{"coverage"} and
#'   \code{"width"}.
#' @param B_target number of bootstrap replications to which the criteria should
#'   be extrapolated, with a default of \code{B = Inf}.
#' @inheritParams calc_absolute
#'
#' @return A tibble containing the number of simulation iterations, performance
#'   criteria estimate(s) and the associated MCSE.
#'
#' @export
#'
#' @references
#' \insertRef{boos2000MonteCarloEvaluation}{simhelpers}
#'
#' @examples
#' extrapolate_coverage(
#'   data = t_boots,
#'   CI_subsamples = CIs,
#'   true_param = true_param
#' )
#'

extrapolate_coverage <- function(
    data,
    CI_subsamples,
    true_param,
    B_target = Inf,
    criteria = c("coverage", "width"),
    winz = Inf
) {

  criteria <- match.arg(criteria, choices = c("coverage", "width"), several.ok = TRUE)

  if (!missing(data)) {
    cl <- match.call()
    true_param <- eval(cl$true_param, envir = data, enclos = parent.frame())
    CI_subsamples <- eval(cl$CI_subsamples, envir = data, enclos = parent.frame())
  }

  true_param <- unique(true_param) # true param
  if (length(true_param) > 1L) stop("`true_param` must have a single unique value.")


  K <- length(CI_subsamples) # iterations

  # check that all replications are same length
  rep_range <- range(sapply(CI_subsamples, nrow))

  if (diff(rep_range) != 0L) {
    msg <- paste0("All replications must have the same number of rows. ",
                  "Smallest: ", rep_range[1], ". ",
                  "Largest: ", rep_range[2], ".")
    stop(msg)
  }

  # check that all replications use same B_vals
  B_reps <-
    sapply(CI_subsamples, \(x) x$bootstraps) |>
    apply(1, \(y) diff(range(y)))
  if (any(B_reps != 0L)) {
    stop("All replications must use the same set of B_vals and same number of subsamples per B_val.")
  }


  # calculate wts for each replication
  B_vals <- unique(CI_subsamples[[1]]$bootstraps)
  p <- length(B_vals)
  Btilde <- mean(1 / B_vals)
  x <- 1 / B_vals - Btilde
  S_B <- as.numeric(crossprod(x))
  B_wts <- 1 / p - x * (Btilde - 1 / B_target) / S_B

  # calculate extrapolated coverage and width per replication
  coverage_proj <- lapply(CI_subsamples, project_coverage, true_param = true_param, B_wts = B_wts, B_target = B_target)
  coverage_proj <- do.call(rbind, coverage_proj)
  coverage_proj <- by(coverage_proj, coverage_proj$bootstraps)
  width_proj <- lapply(CI_subsamples, project_width, B_wts = B_wts, B_target = B_target)
  width_proj <- do.call(rbind, width_proj)

  # initialize tibble
  dat <- tibble::tibble(K_coverage = K)

  # handle winsorization
  if (winz < Inf) {
    width_proj <- lapply(width_proj, winsorize, winz = winz) |> as.data.frame()
    width_winsor_pct <- lapply(width_proj, \(x) attr(x, "winsor_pct")) |> as.data.frame()
    width_winsor_pct_mcse <- sqrt(width_winsor_pct * (1 - width_winsor_pct) / K)
    names(width_winsor_pct) <- paste("width_winsor_pct", names(width_winsor_pct), sep = "_")
    names(width_winsor_pct_mcse) <- paste("width_winsor_pct_mcse", names(width_winsor_pct_mcse), sep = "_")
    dat <- cbind(dat, width_winsor_pct, width_winsor_pct_mcse)
  }

  if ("coverage" %in% criteria) {
    coverage <- lapply(coverage_proj, \(x) mean(x)) |> as.data.frame()
    names(coverage) <- paste("coverage", names(coverage), sep = "_")
    coverage_mcse <- lapply(coverage_proj, \(x) sd(x) / sqrt(K)) |> as.data.frame()
    names(coverage_mcse) <- paste("coverage_mcse", names(coverage_mcse), sep = "_")
    dat <- cbind(dat, coverage, coverage_mcse)
  }

  if ("width" %in% criteria) {
    width <- lapply(width_proj, \(x) mean(x)) |> as.data.frame()
    names(width) <- paste("width", names(width), sep = "_")
    width_mcse <- lapply(width_proj, \(x) sd(x) / sqrt(K)) |> as.data.frame()
    names(width_mcse) <- paste("width_mcse", names(width_mcse), sep = "_")
    dat <- cbind(dat, width, width_mcse)
  }

  return(dat)

}

project_coverage <- function(CI_dat, true_param, B_wts, B_target = B_target) {

  lower_names <- which(grepl("_lower$", names(CI_dat)))
  upper_names <- which(grepl("_upper$", names(CI_dat)))
  CI_names <- substr(names(CI_dat)[lower_names], 1, nchar(names(CI_dat)[lower_names]) - 6L)
  B_vals <- unique(CI_dat$bootstraps)

  cover_by_Bval <- mapply(
    \(lo,up) tapply(lo < true_param & true_param < up, CI_dat$bootstraps, mean),
    lo = CI_dat[lower_names],
    up = CI_dat[upper_names],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  names(cover_by_Bval) <- CI_names

  cover_extrap <- lapply(cover_by_Bval, \(x) sum(x * B_wts))
  cover_extrap$bootstraps <- B_target

  cover_by_Bval$bootstraps <- B_vals
  rbind(
    as.data.frame(cover_by_Bval),
    as.data.frame(cover_extrap)
  )
}

project_width <- function(CI_dat, B_wts, B_target) {

  lower_names <- which(grepl("_lower$", names(CI_dat)))
  upper_names <- which(grepl("_upper$", names(CI_dat)))
  CI_names <- substr(names(CI_dat)[lower_names], 1, nchar(names(CI_dat)[lower_names]) - 6L)
  B_vals <- unique(CI_dat$bootstraps)

  width_by_Bval <- mapply(
    \(lo,up) tapply(up - lo, CI_dat$bootstraps, mean),
    lo = CI_dat[lower_names],
    up = CI_dat[upper_names],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  names(width_by_Bval) <- CI_names

  width_extrap <- lapply(width_by_Bval, \(x) sum(x * B_wts))
  width_extrap$bootstraps <- B_target

  width_by_Bval$bootstraps <- B_vals
  rbind(
    as.data.frame(width_by_Bval),
    as.data.frame(width_extrap)
  )

}

