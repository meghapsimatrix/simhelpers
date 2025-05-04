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
  reps = 120L,
  N_A = 40, N_B = 50,
  shape_A = 7, scale_A = 2,
  shape_B = 4, scale_B = 3,
  B_vals = c(49, 99, 149, 199),
  pval_reps = 3L
)

test_that("extrapolate_rejection options work with a single alpha.", {

  alpha_vals <- .07
  B_target <- 1999

  unnested_wide <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals
  ) %>%
    as_tibble()

  nested_wide <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals,
    nested = TRUE
  )

  nested_wide %>%
    unnest(c(bootstraps, boot_rej_rate, boot_rej_rate_mcse), names_sep = "_") %>%
    expect_identical(unnested_wide)

  unnested_long <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals,
    format = "long"
  ) %>%
    as_tibble()

  nested_long <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals,
    format = "long",
    nested = TRUE
  )

  nested_long %>%
    unnest(c(bootstraps, boot_rej_rate, boot_rej_rate_mcse)) %>%
    expect_identical(unnested_long)


  by_hand <-
    x %>%
    select(pvalue_subsamples) %>%
    mutate(row = row_number()) %>%
    unnest(pvalue_subsamples) %>%
    unnest(pval) %>%
    rowwise() %>%
    mutate(
      alpha = list(alpha_vals),
      rej = list(sapply(alpha_vals, \(x) pval < x))
    ) %>%
    unnest(c(alpha, rej)) %>%
    group_by(alpha, row, bootstraps) %>%
    summarize(
      rej = mean(rej),
      .groups = "drop_last"
    )

  by_hand <-
    by_hand %>%
    mutate(
      B_wt = get_B_wts(bootstraps, B_target)
    ) %>%
    summarize(
      rej = sum(rej * B_wt),
      bootstraps = B_target,
      .groups = "keep"
    ) %>%
    bind_rows(by_hand) %>%
    group_by(bootstraps, alpha) %>%
    summarize(
      K_boot_rejection = n(),
      boot_rej_rate = mean(rej),
      boot_rej_rate_mcse = sd(rej) / sqrt(n()),
      .groups = "drop"
    ) %>%
    select(K_boot_rejection, bootstraps, alpha, boot_rej_rate, boot_rej_rate_mcse) %>%
    arrange(alpha, bootstraps)

  unnested_long %>%
    expect_identical(by_hand)

})

test_that("extrapolate_rejection options work with a multiple alphas.", {

  alpha_vals <- c(.01, .05, .10)
  B_target <- 999

  unnested_wide <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals
  ) %>%
    as_tibble()

  nested_wide <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals,
    nested = TRUE
  )

  nested_wide %>%
    unnest(c(bootstraps, boot_rej_rate, boot_rej_rate_mcse), names_sep = "_") %>%
    expect_identical(unnested_wide)

  unnested_long <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals,
    format = "long"
  ) %>%
    as_tibble()

  nested_long <- extrapolate_rejection(
    data = x,
    pvalue_subsamples = pvalue_subsamples,
    B_target = B_target,
    alpha = alpha_vals,
    format = "long",
    nested = TRUE
  )

  nested_long %>%
    unnest(c(bootstraps, boot_rej_rate, boot_rej_rate_mcse)) %>%
    expect_identical(unnested_long)


  by_hand <-
    x %>%
    select(pvalue_subsamples) %>%
    mutate(row = row_number()) %>%
    unnest(pvalue_subsamples) %>%
    unnest(pval) %>%
    rowwise() %>%
    mutate(
      alpha = list(alpha_vals),
      rej = list(sapply(alpha_vals, \(x) pval < x))
    ) %>%
    unnest(c(alpha, rej)) %>%
    group_by(alpha, row, bootstraps) %>%
    summarize(
      rej = mean(rej),
      .groups = "drop_last"
    )

  by_hand <-
    by_hand %>%
    mutate(
      B_wt = get_B_wts(bootstraps, B_target)
    ) %>%
    summarize(
      rej = sum(rej * B_wt),
      bootstraps = B_target,
      .groups = "keep"
    ) %>%
    bind_rows(by_hand) %>%
    group_by(bootstraps, alpha) %>%
    summarize(
      K_boot_rejection = n(),
      boot_rej_rate = mean(rej),
      boot_rej_rate_mcse = sd(rej) / sqrt(n()),
      .groups = "drop"
    ) %>%
    select(K_boot_rejection, bootstraps, alpha, boot_rej_rate, boot_rej_rate_mcse) %>%
    arrange(alpha, bootstraps)

  unnested_long %>%
    expect_identical(by_hand)

})

