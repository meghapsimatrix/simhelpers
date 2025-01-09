#' @importFrom stats quantile
#' @importFrom stats qnorm

calc_boot_CIs <- function(
    i,
    boot_est,
    boot_se = NULL,
    est = NULL,
    se = NULL,
    accel = 0,
    CI_type = "percentile",
    probs = c(.025, .975),
    format = "wide"
) {

  if (any(c("basic","percentile") %in% CI_type)) {
    pctls <- stats::quantile(boot_est[i], probs = probs, type = 1)
  }

  CI_dat <- data.frame(bootstraps = length(i))

  if ("normal" %in% CI_type) {
    mid <- 2 * est - mean(boot_est[i])
    len <- stats::qnorm(probs) * sd(boot_est[i])
    CI_dat$normal_lower <- mid + len[1]
    CI_dat$normal_upper <- mid + len[2]
  }

  if ("basic" %in% CI_type) {
    CI_dat$basic_lower <- 2 * est - pctls[2]
    CI_dat$basic_upper <- 2 * est - pctls[1]
  }

  if ("student" %in% CI_type) {
    boot_ts <- (boot_est[i] - est) / boot_se[i]
    t_pctls <- stats::quantile(boot_ts, probs = probs, type = 1)
    CI_dat$student_lower <- est - se * t_pctls[2]
    CI_dat$student_upper <- est - se * t_pctls[1]
  }

  if ("percentile" %in% CI_type) {
    CI_dat$percentile_lower <- pctls[1]
    CI_dat$percentile_upper <- pctls[2]
  }

  if (any(c("bias-corrected","BCa") %in% CI_type)) {
    w <- stats::qnorm(mean(boot_est[i] < est))
  }

  if ("bias-corrected" %in% CI_type) {
    bc_probs <- stats::pnorm(2 * w + stats::qnorm(probs))
    bc_pctls <- stats::quantile(boot_est[i], probs = bc_probs, type = 1)
    CI_dat$biascorrected_lower <- bc_pctls[1]
    CI_dat$biascorrected_upper <- bc_pctls[2]
  }

  if ("BCa" %in% CI_type) {
    wz <- w + stats::qnorm(probs)
    bca_probs <- stats::pnorm(w + wz / (1 - accel * wz))
    bca_pctls <- stats::quantile(boot_est[i], probs = bca_probs, type = 1)
    CI_dat$BCa_lower <- bca_pctls[1]
    CI_dat$BCa_upper <- bca_pctls[2]
  }

  if (format == "long") {
    CI_names <- intersect(c("normal","basic","student","percentile","bias-corrected","BCa"), CI_type)
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
#'   Required for \code{CI_type = "normal"}, \code{CI_type = "basic"},
#'   \code{CI_type = "student"}, and \code{CI_type = "bias-corrected"}.
#' @param se numeric value of the estimated standard error based on the original
#'   sample. Required for \code{CI_type = "student"}.
#' @param influence vector of empirical influence values for the estimator. Required for \code{CI_type = "BCa"}.
#' @param CI_type Character string or vector of character strings indicating
#'   types of confidence intervals to calculate. Options are \code{"normal"},
#'   \code{"basic"}, \code{"student"}, \code{"percentile"} (the default), \code{"bias-corrected"}, or \code{"BCa"}.
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
#' @param seed Single numeric value to which the random number generator seed
#'   will be set. Default is \code{NULL}, which does not set a seed.
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
#'   CI_type = c("normal","basic","student","percentile","bias-corrected"),
#'   format = "long"
#' )
#'
#' # Calculate bias-corrected-and-accelerated CIs
#' inf_vals <- res[1] - sapply(seq_along(dat), \(i) f(dat[-i])[1])
#' bootstrap_CIs(
#'   boot_est = booties[1,],
#'   est = res[1],
#'   influence = inf_vals,
#'   CI_type = c("percentile","bias-corrected","BCa"),
#'   format = "long"
#' )
#'
#' # calculate multiple bootstrap CIs using sub-sampling of replicates
#' bootstrap_CIs(
#'   boot_est = booties[1,],
#'   boot_se = booties[2,],
#'   est = res[1],
#'   se = res[2],
#'   CI_type = c("normal","basic","student","percentile","bias-corrected"),
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
  influence = NULL,
  CI_type = "percentile",
  level = 0.95,
  B_vals = length(boot_est),
  reps = 1L,
  format = "wide",
  seed = NULL
) {

  if (!is.null(seed)) set.seed(seed)

  if (level <= 0 | level >= 1) stop("`level` must be between 0 and 1 (e.g., `level = 0.95`).")

  CI_type <- match.arg(CI_type, c("normal","basic","student","percentile","bias-corrected","BCa"), several.ok = TRUE)
  format <- match.arg(format, c("wide","long","wide-list"))

  if (is.null(est)) {
    CI_type_reqs <- c("normal","basic","student","bias-corrected","BCa")
    if (any(CI_type_reqs %in% CI_type)) {
      CI_type_paste <- paste("'", intersect(CI_type_reqs, CI_type), "'", collapse = ", ")
      stop(paste0("CI_type %in% c(",CI_type_paste,"requires providing a value for `est`."))
    }
  }
  if ("student" %in% CI_type) {
    if (is.null(se)) {
      stop("CI_type = 'student' requires providing a value for `se`.")
    }
    if ((length(boot_se) != length(boot_est))) {
      stop("CI_type = 'student' requires providing values for `boot_se`.")
    }
  }
  if ("BCa" %in% CI_type) {
    if (is.null(influence)) stop("CI_type = 'BCa' requires providing a value for `influence`.")
    accel <- sum(influence^3) / (6 * sum(influence^2)^1.5)
  } else {
    accel <- 0
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
          accel = accel,
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



#' @title Calculate one or multiple bootstrap p-values
#'
#' @description Calculate one or multiple bootstrap p-values, given a bootstrap
#'   sample of test statistics.
#'
#' @param boot_stat vector of bootstrap replications of a test statistic.
#' @param stat numeric value of the test statistic based on the original sample.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"two-sided"} (the default), \code{"greater"} or
#'   \code{"less"}.
#' @param B_vals vector of sub-sample sizes for which to calculate p-values.
#'   Setting \code{B_vals = length(boot_stat)} (the default) will return a
#'   single p-value calculated on the full set of bootstrap replications. For
#'   \code{B_vals < length(boot_stat)}, p-values will be calculated after
#'   sub-sampling (without replacement) the bootstrap replications.
#' @param reps integer value for the number of sub-sample p-values to generate
#'   when \code{B_vals < length(boot_stat)}, with a default of \code{reps = 1}.
#' @param enlist logical indicating whether to wrap the returned values in an
#'   unnamed list, with a default of \code{FALSE}. Setting \code{enlist = TRUE}
#'   makes it easier to store the output as a single entry in a \code{tibble}.
#' @param seed Single numeric value to which the random number generator seed
#'   will be set. Default is \code{NULL}, which does not set a seed.
#'
#' @return The format of the output depends on several contingencies. If only a
#'   single value of \code{B_vals} is specified and \code{reps = 1}, then the
#'   function returns a vector with a single p-value. If only a single value of
#'   \code{B_vals} is specified but \code{B_vals < length(boot_stat)} and
#'   \code{reps > 1}, then the function returns a vector p-values, with an entry
#'   for each sub-sample replication. If \code{B_vals} is a vector of multiple
#'   values, then the function returns a list with one entry per entry of
#'   \code{B_vals}, where each entry is a vector of length \code{reps} with
#'   entries for each sub-sample replication.
#'
#'   If \code{enlist = TRUE}, then results will be wrapped in an unnamed list,
#'   which makes it easier to sore the output in a tibble.
#'
#' @details p-values are calculated by comparing \code{stat} to the distribution
#'   of \code{boot_stat}, which is taken to represent the null distribution of
#'   the test statistic. If \code{alternative = "two-sided"} (the default), then
#'   the p-value is the proportion of the bootstrap sample where the absolute
#'   value of the bootstrapped statistic exceeds the absolute value of the
#'   original statistic. If \code{alternative = "greater"}, then the p-value is
#'   the proportion of the bootstrap sample where the value of the bootstrapped
#'   statistic is larger than the original statistic. If \code{alternative =
#'   "less"}, then the p-value is the proportion of the bootstrap sample where
#'   the value of the bootstrapped statistic is less than the original
#'   statistic.
#'
#' @references Davison, A.C. and Hinkley, D.V. (1997). _Bootstrap Methods and
#'   Their Application_, Chapter 4. Cambridge University Press.
#'
#' @export
#'
#' @examples
#' # generate data from two distinct populations
#' dat <- data.frame(
#'   group = rep(c("A","B"), c(40, 50)),
#'   y = c(
#'     rgamma(40, shape = 7, scale = 2),
#'     rgamma(50, shape = 3, scale = 4)
#'   )
#' )
#' stat <- t.test(y ~ group, data = dat)$statistic
#'
#' # create bootstrap replications under the null of no difference
#' boot_dat <- dat
#' booties <- replicate(399, {
#'   boot_dat$group <- sample(dat$group)
#'   t.test(y ~ group, data = boot_dat)$statistic
#' })
#'
#' # calculate bootstrap p-values from full set of bootstrap replicates
#' bootstrap_pvals(boot_stat = booties, stat = stat)
#'
#' # calculate multiple bootstrap p-values using sub-sampling of replicates
#' bootstrap_pvals(
#'   boot_stat = booties, stat = stat,
#'   B_vals = 199,
#'   reps = 4L
#' )
#'
#' # calculate multiple bootstrap p-values using sub-sampling of replicates,
#' # for each of several sub-sample sizes.
#' bootstrap_pvals(
#'   boot_stat = booties, stat = stat,
#'   B_vals = c(49,99,199),
#'   reps = 4L
#' )
#'

bootstrap_pvals <- function(
    boot_stat,
    stat,
    alternative = "two-sided",
    B_vals = length(boot_stat),
    reps = 1L,
    enlist = FALSE,
    seed = NULL
) {

  if (!is.null(seed)) set.seed(seed)

  alternative <- match.arg(alternative, c("two-sided","greater","less"))
  if (alternative == "two-sided") {
    boot_stat <- abs(boot_stat)
    stat <- abs(stat)
  } else if (alternative == "less") {
    boot_stat <- -boot_stat
    stat <- -stat
  }

  N_boots <- length(boot_stat)

  pval_list <- lapply(B_vals, \(x) {
    if (x == length(boot_stat)) reps <- 1L
    replicate(reps , {
      boot_sub <- sample(boot_stat, size = x)
      mean(boot_sub > stat)
    }, simplify = TRUE)
  })

  pval_dat <- data.frame(
    bootstraps = B_vals
  )
  pval_dat$pval <- pval_list

  if (enlist) {
    return(list(pval_dat))
  } else {
    return(pval_dat)
  }

}



get_B_wts <- function(B_vals, B_target) {
  p <- length(B_vals)
  Btilde <- mean(1 / B_vals)
  x <- 1 / B_vals - Btilde
  S_B <- as.numeric(crossprod(x))
  B_wts <- 1 / p - x * (Btilde - 1 / B_target) / S_B
  return(B_wts)
}
