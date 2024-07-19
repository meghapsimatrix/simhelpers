calc_boot_CIs <- function(
    i,
    boot_est,
    boot_se = NULL,
    est = NULL,
    se = NULL,
    CI_type = "percentile",
    probs = c(.025, .975),
    format = "wide"
) {

  if(any(c("basic","percentile") %in% CI_type)) {
    pctls <- quantile(boot_est[i], probs = probs, type = 1)
  }

  CI_dat <- data.frame(bootstraps = length(i))

  if ("normal" %in% CI_type) {
    mid <- 2 * est - mean(boot_est[i])
    len <- qnorm(probs) * sd(boot_est[i])
    CI_dat$normal_lower <- mid + len[1]
    CI_dat$normal_upper <- mid + len[2]
  }

  if ("basic" %in% CI_type) {
    CI_dat$basic_lower <- 2 * est - pctls[2]
    CI_dat$basic_upper <- 2 * est - pctls[1]
  }

  if ("student" %in% CI_type) {
    boot_ts <- (boot_est[i] - est) / boot_se[i]
    t_pctls <- quantile(boot_ts, probs = probs, type = 1)
    CI_dat$student_lower <- est - se * t_pctls[2]
    CI_dat$student_upper <- est - se * t_pctls[1]
  }

  if ("percentile" %in% CI_type) {
    CI_dat$percentile_lower <- pctls[1]
    CI_dat$percentile_upper <- pctls[2]
  }

  if (format == "long") {
    CI_names <- intersect(c("normal","basic","student","percentile"), CI_type)
    lower_vals <- unlist(CI_dat[seq(2L, by = 2L, length.out = length(CI_type))])
    upper_vals <- unlist(CI_dat[seq(3L, by = 2L, length.out = length(CI_type))])
    CI_dat <- data.frame(
      bootstraps = CI_dat$bootstraps,
      type = CI_names,
      lower = lower_vals,
      upper = upper_vals
    )
    row.names(CI_dat) <- NULL
  }

  return(CI_dat)
}


#' @title Calculate one or multiple bootstrap confidence intervals
#'
#' @description Calculate one or multiple bootstrap confidence intervals, given
#'   a sample of bootstrap replications.
#'
#' @param boot_est vector of bootstrap replications of an estimator.
#' @param boot_se vector of estimated standard errors from each bootstrap
#'   replication.
#' @param est numeric value of the estimate based on the original sample.
#'   Required for \code{CI_type = "normal"}, \code{CI_type = "basic"}, and
#'   \code{CI_type = "student"}.
#' @param se numeric value of the estimated standard error based on the original
#'   sample. Required for \code{CI_type = "student"}.
#' @param CI_type Character string or vector of character strings indicating
#'   types of confidence intervals to calculate. Options are \code{"normal"},
#'   \code{"basic"}, \code{"student"}, and \code{"percentile"} (the default).
#' @param level numeric value between 0 and 1 for the desired coverage level,
#'   with a default of \code{0.95}.
#' @param B_vals vector of sub-sample sizes for which to calculate confidence
#'   intervals. Setting \code{B_vals = length(boot_est)} (the default) will
#'   return bootstrap confidence intervals calculated on the full set of
#'   bootstrap replications. For \code{B_vals < length(boot_est)}, confidence
#'   intervals will be calculated after sub-sampling (without replacement) the
#'   bootstrap replications.
#' @param reps integer value for the number of sub-sample confidence intervals
#'   to generate when \code{B_vals < length(boot_est)}, with a default of
#'   \code{reps = 1}.
#' @param format character string controlling the format of the output. If
#'   \code{format = "wide"} (the default), then different types of confidence
#'   intervals will be returned in separate columns. If \code{format = "long"},
#'   then confidence intervals of different types will appear on different rows
#'   of dataset. If \code{format = "wide-list"}, then different types of
#'   confidence intervals will be returned in separate columns and the result
#'   will be wrapped in an unnamed list.
#'
#' @return If \code{format = "wide"}, the function returns a \code{data.frame}
#'   with \code{reps} rows per entry of \code{B_vals}, where each row contains
#'   confidence intervals for one sub-sample replication.
#'
#'   If \code{format = "long"}, the function returns a \code{data.frame} with
#'   one row for each \code{CI_type}, each replication, and each entry of
#'   \code{B_vals}, where each row contains a single confidence interval for one
#'   sub-sample replication.
#'
#'   If \code{format = "wide-list"}, then the output will be structured as in
#'   \code{format = "wide"} but will be wrapped in an unnamed list, which makes
#'   it easier to sore the output in a tibble, and will be assigned the class
#'   \code{"bootstrap_CIs"}.
#'
#' @details Confidence intervals are calculated following the methods described
#'   in Chapter 5 of Davison and Hinkley (1997). For basic non-parametric
#'   bootstraps, the methods are nearly identical to the implementation in
#'   \code{\link[boot]{boot.ci}} from the \code{boot} package.
#'
#' @references Davison, A.C. and Hinkley, D.V. (1997). _Bootstrap Methods and
#'   Their Application_, Chapter 5. Cambridge University Press.
#'
#' @export
#'
#' @examples
#' # generate t-distributed data
#' N <- 50
#' mu <- 2
#' nu <- 5
#' dat <- mu + rt(N, df = nu)
#'
#' # create bootstrap replications
#' f <- \(x) {
#'  c(
#'    M = mean(x, trim = 0.1),
#'    SE = sd(x) / sqrt(length(x))
#'  )
#' }
#'
#' booties <- replicate(399, {
#'   sample(dat, replace = TRUE, size = N) |>
#'   f()
#' })
#'
#' res <- f(dat)
#'
#' # calculate bootstrap CIs from full set of bootstrap replicates
#' bootstrap_CIs(
#'   boot_est = booties[1,],
#'   boot_se = booties[2,],
#'   est = res[1],
#'   se = res[2],
#'   CI_type = c("normal","basic","student","percentile"),
#'   format = "long"
#' )
#'
#' # calculate multiple bootstrap CIs using sub-sampling of replicates
#' bootstrap_CIs(
#'   boot_est = booties[1,],
#'   boot_se = booties[2,],
#'   est = res[1],
#'   se = res[2],
#'   CI_type = c("normal","basic","student","percentile"),
#'   B_vals = 199,
#'   reps = 4L,
#'   format = "long"
#' )
#'
#' # calculate multiple bootstrap CIs using sub-sampling of replicates,
#' # for each of several sub-sample sizes.
#' bootstrap_CIs(
#'   boot_est = booties[1,],
#'   boot_se = booties[2,],
#'   est = res[1],
#'   se = res[2],
#'   CI_type = c("normal","basic","student","percentile"),
#'   B_vals = c(49,99,199),
#'   reps = 4L,
#'   format = "long"
#' )
#'

bootstrap_CIs <- function(
  boot_est,
  boot_se = NULL,
  est = NULL,
  se = NULL,
  CI_type = "percentile",
  level = 0.95,
  B_vals = length(boot_est),
  reps = 1L,
  format = "wide"
) {

  if (level <= 0 | level >= 1) stop("`level` must be between 0 and 1 (e.g., `level = 0.95`).")

  CI_type <- match.arg(CI_type, c("normal","basic","student","percentile"), several.ok = TRUE)
  format <- match.arg(format, c("wide","long","wide-list"))

  if (("normal" %in% CI_type) & is.null(est)) {
    stop("CI_type = 'normal' requires providing a value for `est`.")
  }
  if (("student" %in% CI_type) & (length(boot_se) != length(boot_est))) {
    stop("CI_type = 'student' requires providing values for `boot_se`.")
  }

  probs <- (1 + c(-1, 1) * level) / 2

  N_boots <- length(boot_est)

  CI_list <- lapply(B_vals, \(x) {
    if (x == length(boot_est)) reps <- 1L
    replicate(reps , {
      sample(1:N_boots, size = x) |>
        calc_boot_CIs(
          boot_est = boot_est,
          boot_se = boot_se,
          est = est,
          se = se,
          CI_type = CI_type,
          probs = probs,
          format = format
        )
    }, simplify = FALSE)
  })

  CI_df <- do.call(rbind, unlist(CI_list, recursive = FALSE))
  if (format == "wide-list") {
    CI_df <- list(CI_df)
    class(CI_df) <- c("bootstrap_CIs", class(CI_df))
  }

  return(CI_df)
}
