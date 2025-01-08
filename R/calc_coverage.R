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
#' @param width_trim numeric value specifying the trimming percentage to use
#'   when summarizing CI widths across replications from a single set of
#'   bootstraps, with a default of 0.0 (i.e., use the regular arithmetic mean).
#' @param cover_na_val numeric value to use for calculating coverage if bootstrap CI end-points are missing. Default is \code{NA}.
#' @param width_na_val numeric value to use for calculating width if bootstrap CI end-points are missing. Default is \code{NA}.
#' @inheritParams calc_absolute
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
#' dgp <- function(N, mu, nu) {
#'   mu + rt(N, df = nu)
#' }
#'
#' estimator <- function(
#'    dat,
#'     B_vals = c(49,59,89,99),
#'     m = 4,
#'     trim = 0.1
#' ) {
#'
#'
#'   # compute estimate and standard error
#'   N <- length(dat)
#'   est <- mean(dat, trim = trim)
#'   se <- sd(dat) / sqrt(N)
#'
#'   # compute booties
#'   booties <- replicate(max(B_vals), {
#'     x <- sample(dat, size = N, replace = TRUE)
#'     data.frame(
#'       M = mean(x, trim = trim),
#'       SE = sd(x) / sqrt(N)
#'     )
#'   }, simplify = FALSE) |>
#'     dplyr::bind_rows()
#'
#'   # confidence intervals for each B_vals
#'   CIs <- bootstrap_CIs(
#'     boot_est = booties$M,
#'     boot_se = booties$SE,
#'     est = est,
#'     se = se,
#'     CI_type = c("normal","basic","student","percentile"),
#'     B_vals = B_vals,
#'     reps = m,
#'     format = "wide-list"
#'   )
#'
#'   res <- data.frame(
#'     est = est,
#'     se = se
#'   )
#'   res$CIs <- CIs
#'
#'   res
#' }
#'
#' #' build a simulation driver function
#' simulate_bootCIs <- bundle_sim(
#'   f_generate = dgp,
#'   f_analyze = estimator
#' )
#'
#' boot_results <- simulate_bootCIs(
#'   reps = 50, N = 20, mu = 2, nu = 3,
#'   B_vals = seq(49, 199, 50),
#' )
#'
#' extrapolate_coverage(
#'   data = boot_results,
#'   CI_subsamples = CIs,
#'   true_param = 2
#' )
#'
#' extrapolate_coverage(
#'   data = boot_results,
#'   CI_subsamples = CIs,
#'   true_param = 2,
#'   B_target = 999,
#'   format = "long"
#' )
#'

extrapolate_coverage <- function(
    data,
    CI_subsamples,
    true_param,
    B_target = Inf,
    criteria = c("coverage", "width"),
    winz = Inf,
    nested = FALSE,
    format = "wide",
    width_trim = 0.0,
    cover_na_val = NA,
    width_na_val = NA
) {

  criteria <- match.arg(criteria, choices = c("coverage", "width"), several.ok = TRUE)
  format <- match.arg(format, choices = c("wide","long"))

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
  B_wts <- get_B_wts(B_vals, B_target = B_target)

  # initialize results table
  dat <- data.frame(
    K_boot_coverage = K
  )

  if (format == "wide") {
    dat$bootstraps <- list(c(B_vals, B_target))
  } else {
    CI_names <- names(CI_subsamples[[1]])[seq(2, ncol(CI_subsamples[[1]]), 2)]
    CI_names <- substr(CI_names, 1, nchar(CI_names) - 6L)
    dat$bootstraps <- list(data.frame(
      bootstraps = rep(c(B_vals, B_target), length(CI_names)),
      CI_type = rep(CI_names, each = length(B_vals) + 1L)
    ))
  }

  if ("coverage" %in% criteria) {

    # calculate extrapolated coverage per replication
    coverage_proj <- lapply(
      CI_subsamples,
      project_coverage,
      true_param = true_param, B_wts = B_wts, B_target = B_target, cover_na_val = cover_na_val
    )
    coverage_proj <- do.call(rbind, coverage_proj)

    # summarize across replications
    dat$boot_coverage <- summarize_by_boot(coverage_proj, mean, format = format)
    dat$boot_coverage_mcse <- summarize_by_boot(coverage_proj, \(y) sd(y) / sqrt(K), format = format)
  }

  if ("width" %in% criteria) {

    # calculate extrapolated width per replication

    width_proj <- lapply(
      CI_subsamples,
      project_width,
      B_wts = B_wts, B_target = B_target,
      width_trim = width_trim, width_na_val = width_na_val
    )
    width_proj <- do.call(rbind, width_proj)

    # handle winsorization
    if (winz < Inf) {

      boot_name <- which(names(width_proj) == "bootstraps")

      width_proj_winz <- lapply(
        width_proj[-boot_name],
        winsorize_by,
        by = width_proj$bootstraps, winz = winz,
        na_val = width_na_val
      ) |>
        as.data.frame()
      width_proj_winz$bootstraps <- width_proj$bootstraps
      width_proj <- width_proj_winz

      width_winsor_pct <- lapply(
        width_proj[-boot_name],
        \(x) attr(x, "winsor_pct")
      ) |>
        as.data.frame()

      if (format == "long") {
        width_winsor_pct <- as.numeric(unlist(width_winsor_pct))
      }

      width_winsor_pct_mcse <- sqrt(width_winsor_pct * (1 - width_winsor_pct) / K)
      dat$boot_width_winsor_pct <- list(width_winsor_pct)
      dat$boot_width_winsor_pct_mcse <- list(width_winsor_pct_mcse)
    }

    # summarize across replications
    dat$boot_width <- summarize_by_boot(width_proj, mean, format = format, na.rm = TRUE)
    dat$boot_width_mcse <- summarize_by_boot(width_proj, \(y) sd(y, na.rm = TRUE) / sqrt(K), format = format)
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

project_coverage <- function(CI_dat, true_param, B_wts, B_target = B_target, cover_na_val = NA) {

  lower_names <- which(grepl("_lower$", names(CI_dat)))
  upper_names <- which(grepl("_upper$", names(CI_dat)))
  CI_names <- substr(names(CI_dat)[lower_names], 1, nchar(names(CI_dat)[lower_names]) - 6L)
  B_vals <- unique(CI_dat$bootstraps)

  cover_indicators <- mapply(
    \(lo,up) ifelse(is.na(lo) | is.na(up), cover_na_val, lo < true_param & true_param < up),
    lo = CI_dat[lower_names],
    up = CI_dat[upper_names],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  names(cover_indicators) <- CI_names

  cover_by_Bval <- lapply(cover_indicators, \(x) tapply(x, CI_dat$bootstraps, mean))

  cover_extrap <- lapply(cover_by_Bval, \(x) sum(x * B_wts))
  cover_extrap$bootstraps <- B_target

  cover_by_Bval$bootstraps <- B_vals
  rbind(
    as.data.frame(cover_by_Bval),
    as.data.frame(cover_extrap)
  )
}

project_width <- function(CI_dat, B_wts, B_target, width_trim = 0.0, width_na_val = NA) {

  lower_names <- which(grepl("_lower$", names(CI_dat)))
  upper_names <- which(grepl("_upper$", names(CI_dat)))
  CI_names <- substr(names(CI_dat)[lower_names], 1, nchar(names(CI_dat)[lower_names]) - 6L)
  B_vals <- unique(CI_dat$bootstraps)

  widths <- mapply(
    \(lo,up) ifelse(is.na(lo) | is.na(up), width_na_val, up - lo),
    lo = CI_dat[lower_names],
    up = CI_dat[upper_names],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  names(widths) <- CI_names

  width_by_Bval <- lapply(widths,
    \(x) tapply(x, CI_dat$bootstraps, mean, trim = width_trim)
  )

  width_extrap <- lapply(width_by_Bval, \(x) {
    fn <- is.finite(x)
    if (all(!fn)) return(Inf) else sum(x[fn] * B_wts[fn]) / sum(B_wts[fn])
  })
  width_extrap$bootstraps <- B_target

  width_by_Bval$bootstraps <- B_vals
  rbind(
    as.data.frame(width_by_Bval),
    as.data.frame(width_extrap)
  )

}

summarize_by_boot <- function(x, f, format = "wide", ...) {

  not_boots <- which(!(names(x) == "bootstraps"))
  res <- lapply(x[not_boots], \(y) tapply(y, x$bootstraps, f, ...)) |> as.data.frame()

  if (format == "long") {
    res <- as.numeric(unlist(res))
  }

  list(res)
}
