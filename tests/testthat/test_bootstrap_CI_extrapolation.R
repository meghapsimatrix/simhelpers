skip_if_not_installed("dplyr")
skip_if_not_installed("tidyr")
skip_if_not_installed("tibble")

library(tidyr)
library(dplyr)

dgp <- function(N, mu, nu) {
  mu + rt(N, df = nu)
}

estimator <- function(
    dat, # data
    B_vals = c(49,59,89,99), # number of booties to evaluate
    m = 4, # CIs to replicate per sub-sample size
    trim = 0.1, # trimming percentage,
    CI_type = c("normal","basic","student","percentile")
) {


  # compute estimate and standard error
  N <- length(dat)
  est <- mean(dat, trim = trim)
  se <- sd(dat) / sqrt(N)

  # compute booties
  booties <- replicate(max(B_vals), {
    x <- sample(dat, size = N, replace = TRUE)
    data.frame(
      M = mean(x, trim = trim),
      SE = sd(x) / sqrt(N)
    )
  }, simplify = FALSE) |>
    dplyr::bind_rows()

  # confidence intervals for each B_vals
  CIs <- bootstrap_CIs(
    boot_est = booties$M,
    boot_se = booties$SE,
    est = est,
    se = se,
    CI_type = CI_type,
    B_vals = B_vals,
    reps = m,
    format = "wide-list"
  )

  tibble::tibble(
    est = est,
    se = se,
    CI = CIs
  )
}

# build a simulation driver function
simulate_bootCIs <- bundle_sim(
  f_generate = dgp,
  f_analyze = estimator
)

test_that("extrapolate_coverage options work with a single CI type.", {

  res <- simulate_bootCIs(reps = 80, N = 20, mu = 2, nu = 3, B_vals = seq(49, 149, 20), CI_type = "percentile")

  nested_wide <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, nest = TRUE)
  unnested_wide <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, nest = FALSE) %>%
    as_tibble()

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width","width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, nest = TRUE, format = "long")
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, nest = FALSE, format = "long") %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width","width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width","width_mcse"), names_sep = ".") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)\\.(.+)")
    ) %>%
    arrange(CI_type, bootstraps) %>%
    expect_equal(arrange(unnested_long, CI_type))
})

test_that("extrapolate_coverage options work with a single CI type and winsorization.", {

  res <- simulate_bootCIs(reps = 80, N = 20, mu = 2, nu = 3, B_vals = seq(49, 149, 20), CI_type = "percentile")

  nested_wide <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, winz = 1, nest = TRUE)
  unnested_wide <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, winz = 1, nest = FALSE) %>%
    as_tibble()

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width_winsor_pct","width_winsor_pct_mcse", "width","width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, winz = 1, nest = TRUE, format = "long")
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, winz = 1, nest = FALSE, format = "long") %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width_winsor_pct","width_winsor_pct_mcse","width","width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width_winsor_pct","width_winsor_pct_mcse","width","width_mcse"), names_sep = ".") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)\\.(.+)")
    ) %>%
    arrange(CI_type, bootstraps) %>%
    expect_equal(arrange(unnested_long, CI_type))
})

test_that("extrapolate_coverage options work with multiple CI types.", {

  res <- simulate_bootCIs(reps = 50, N = 40, mu = 3, nu = 5, B_vals = seq(49, 149, 20), CI_type = c("normal","basic"))

  nested_wide <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 3, B_target = 1000L, nest = TRUE)
  unnested_wide <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 3, nest = FALSE, B_target = 1000L) %>%
    as_tibble()

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width","width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 3, nest = TRUE, format = "long", B_target = 1000L)
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 3, nest = FALSE, format = "long", B_target = 1000L) %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width","width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width","width_mcse"), names_sep = ".") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)\\.(.+)")
    ) %>%
    arrange(CI_type, bootstraps) %>%
    expect_equal(arrange(unnested_long, CI_type))

  by_hand <-
    res %>%
    select(CI) %>%
    mutate(row = row_number()) %>%
    unnest(CI) %>%
    pivot_longer(
      c(-bootstraps,-row),
      names_to = c("CI_type",".value"),
      names_pattern = c("(.+)_(.+)")
    ) %>%
    mutate(
      cover = lower < 3 & 3 < upper,
      width = upper - lower,
    ) %>%
    group_by(bootstraps, CI_type, row) %>%
    summarize(
      cover = mean(cover),
      width = mean(width),
      .groups = "drop_last"
    ) %>%
    summarize(
      K_coverage = n(),
      coverage = mean(cover),
      coverage_mcse = sd(cover) / sqrt(n()),
      avg_width = mean(width),
      width_mcse = sd(width) / sqrt(n()),
      .groups = "drop"
    )

  unnested_long %>%
    filter(bootstraps < 1000L) %>%
    arrange(bootstraps, CI_type) %>%
    select(bootstraps, CI_type, K_coverage, coverage, coverage_mcse, avg_width = width, width_mcse) %>%
    expect_equal(by_hand)


})


test_that("extrapolate_coverage options work with multiple CI types and winsorization.", {

  res <- simulate_bootCIs(reps = 100, N = 50, mu = 0, nu = 5, B_vals = seq(49, 99, 10))

  nested_wide <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = TRUE)
  unnested_wide <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = FALSE) %>%
    as_tibble()

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width_winsor_pct","width_winsor_pct_mcse","width","width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = TRUE, format = "long")
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = FALSE, format = "long") %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width_winsor_pct","width_winsor_pct_mcse","width","width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","coverage", "coverage_mcse","width_winsor_pct","width_winsor_pct_mcse","width","width_mcse"), names_sep = ".") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)\\.(.+)")
    ) %>%
    arrange(CI_type, bootstraps) %>%
    expect_equal(arrange(unnested_long, CI_type))

  by_hand <-
    res %>%
    select(CI) %>%
    mutate(row = row_number()) %>%
    unnest(CI) %>%
    pivot_longer(
      c(-bootstraps,-row),
      names_to = c("CI_type",".value"),
      names_pattern = c("(.+)_(.+)")
    ) %>%
    mutate(
      cover = lower < 0 & 0 < upper,
      width = upper - lower,
    ) %>%
    group_by(bootstraps, CI_type, row) %>%
    summarize(
      cover = mean(cover),
      width = mean(width),
      .groups = "drop_last"
    ) %>%
    mutate(
      width_winz = winsorize(width, winz = 1.5),
      width_winsor_pct = attr(winsorize(width, winz = 1.5), "winsor_pct")
    ) %>%
    summarize(
      K_coverage = n(),
      coverage = mean(cover),
      coverage_mcse = sd(cover) / sqrt(n()),
      width_winsor_pct = mean(width_winsor_pct),
      width = mean(width_winz),
      width_mcse = sd(width_winz) / sqrt(n()),
      .groups = "drop"
    )

  unnested_long %>%
    filter(bootstraps < Inf) %>%
    arrange(bootstraps, CI_type) %>%
    select(bootstraps, CI_type, K_coverage, coverage, coverage_mcse, width_winsor_pct, width, width_mcse) %>%
    expect_equal(by_hand)
})



