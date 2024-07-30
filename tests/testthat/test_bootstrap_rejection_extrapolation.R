skip_if_not_installed("dplyr")
skip_if_not_installed("tidyr")
skip_if_not_installed("tibble")

library(tidyr)
library(dplyr)

# generate data from two distinct populations
dgp <- function(N_A, N_B, shape_A, scale_A, shape_B, scale_B) {
  data.frame(
    group = rep(c("A","B"), c(N_A, N_B)),
    y = c(
      rgamma(N_A, shape = shape_A, scale = scale_A),
      rgamma(N_B, shape = shape_B, scale = scale_B)
    )
  )
}

estimator <- function(
  dat,
  B_vals = c(49,59,89,99), # number of booties to evaluate
  pval_reps = 4L
) {
  stat <- t.test(y ~ group, data = dat)$statistic

  # create bootstrap replications under the null of no difference
  boot_dat <- dat
  booties <- replicate(max(B_vals), {
    boot_dat$group <- sample(dat$group)
    t.test(y ~ group, data = boot_dat)$statistic
  })

  # calculate multiple bootstrap p-values using sub-sampling of replicates
  res <- data.frame(stat = stat)

  res$pvalue_subsamples <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    B_vals = B_vals,
    reps = pval_reps,
    enlist = TRUE
  )

  res
}

simulate_boot_pvals <- bundle_sim(
  f_generate = dgp,
  f_analyze = estimator
)


x <- simulate_boot_pvals(
  reps = 10L,
  N_A = 40, N_B = 50,
  shape_A = 7, scale_A = 2,
  shape_B = 4, scale_B = 3,
  B_vals = c(49, 99, 149, 199),
  pval_reps = 3L
)

debug(extrapolate_rejection)
extrapolate_rejection(
  data = x,
  pvalue_subsamples = pvalue_subsamples,
  B_target = 999,
  alpha = c(.01, .05, .10)
)
