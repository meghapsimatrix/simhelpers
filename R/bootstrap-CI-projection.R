calc_boot_CIs <- function(
    i,
    boot_est,
    boot_se = NULL,
    est = NULL,
    se = NULL,
    CI_type = "percentile",
    probs = c(.025, .975)
) {

  if(any(c("basic","percentile") %in% CI_type)) {
    pctls <- quantile(boot_est[i], probs = probs, type = 1)
  }

  CI_dat <- data.frame(
    type = CI_type,
    lo = NA_real_,
    hi = NA_real_
  )

  if ("normal" %in% CI_type) {
    p <- which(CI_type == "normal")
    mid <- 2 * est - mean(boot_est[i])
    len <- qnorm(probs) * sd(boot_est[i])
    CI_dat$lo[p] <- mid + len[1]
    CI_dat$hi[p] <- mid + len[2]
  }

  if ("basic" %in% CI_type) {
    p <- which(CI_type == "basic")
    CI_dat$lo[p] <- 2 * est - pctls[2]
    CI_dat$hi[p] <- 2 * est - pctls[1]
  }

  if ("student" %in% CI_type) {
    p <- which(CI_type == "student")
    boot_ts <- (boot_est[i] - est) / boot_se[i]
    t_pctls <- quantile(boot_ts, probs = probs, type = 1)
    CI_dat$lo[p] <- est - se * t_pctls[2]
    CI_dat$hi[p] <- est - se * t_pctls[1]
  }

  if ("percentile" %in% CI_type) {
    p <- which(CI_type == "percentile")
    CI_dat$lo[p] <- pctls[1]
    CI_dat$hi[p] <- pctls[2]
  }

  CI_dat
}

rep_boot_CIs <- function(
  boot_est,
  boot_se = NULL,
  est = NULL,
  se = NULL,
  CI_type = "percentile",
  level = 0.95,
  B_vals = length(est),
  reps = 1L
) {

  if (level <= 0 | level >= 1) stop("`level` must be between 0 and 1 (e.g., `level = 0.95`).")

  CI_type <- match.arg(CI_type, c("normal","basic","student","percentile"), several.ok = TRUE)

  if (("normal" %in% CI_type) & is.null(est)) {
    stop("CI_type = 'normal' requires providing a value for `est`.")
  }
  if (("student" %in% CI_type) & (length(boot_se) != length(boot_est))) {
    stop("CI_type = 'student' requires providing values for `boot_se`.")
  }

  qtls <- 1 + c(-1, 1) * level / 2

  N_boots <- length(boot_est)
  boots <- Map(B_vals, \(x) {
    if (x == length(boot_est)) reps <- 1L
    replicate(reps , {
      sample(1:N_boots, size = x) |>
        calc_boot_CI(
          boot_est = boot_est,
          boot_se = boot_se,
          est = est,
          se = se,
          CI_type = CI_type,
          probs = qtls
        )
    }, simplify = FALSE)
  })

}
