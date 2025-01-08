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
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width","boot_width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, nest = TRUE, format = "long")
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, nest = FALSE, format = "long") %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width","boot_width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width","boot_width_mcse"), names_sep = ".") %>%
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
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width_winsor_pct","boot_width_winsor_pct_mcse", "boot_width","boot_width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, winz = 1, nest = TRUE, format = "long")
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 2, winz = 1, nest = FALSE, format = "long") %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width_winsor_pct","boot_width_winsor_pct_mcse", "boot_width","boot_width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width_winsor_pct","boot_width_winsor_pct_mcse", "boot_width","boot_width_mcse"), names_sep = "-") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)-(.+)")
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
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width","boot_width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 3, nest = TRUE, format = "long", B_target = 1000L)
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 3, nest = FALSE, format = "long", B_target = 1000L) %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width","boot_width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width","boot_width_mcse"), names_sep = "-") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)-(.+)")
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
      K_boot_coverage = n(),
      boot_coverage = mean(cover),
      boot_coverage_mcse = sd(cover) / sqrt(n()),
      boot_width = mean(width),
      boot_width_mcse = sd(width) / sqrt(n()),
      .groups = "drop"
    )

  unnested_long %>%
    filter(bootstraps < 1000L) %>%
    arrange(bootstraps, CI_type) %>%
    select(bootstraps, CI_type, K_boot_coverage, boot_coverage, boot_coverage_mcse, boot_width, boot_width_mcse) %>%
    expect_equal(by_hand)


})


test_that("extrapolate_coverage options work with multiple CI types and winsorization.", {

  res <- simulate_bootCIs(reps = 100, N = 50, mu = 0, nu = 5, B_vals = seq(49, 99, 10))

  nested_wide <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = TRUE)
  unnested_wide <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = FALSE) %>%
    as_tibble()

  nested_wide %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width_winsor_pct","boot_width_winsor_pct_mcse", "boot_width","boot_width_mcse"), names_sep = "_") %>%
    expect_equal(unnested_wide)

  nested_long <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = TRUE, format = "long")
  unnested_long <-
    extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, nest = FALSE, format = "long") %>%
    as_tibble()

  nested_long %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width_winsor_pct","boot_width_winsor_pct_mcse", "boot_width","boot_width_mcse"), names_sep = "_") %>%
    rename(bootstraps = bootstraps_bootstraps, CI_type = bootstraps_CI_type) %>%
    expect_equal(unnested_long)

  nested_wide %>%
    unnest(cols = c("bootstraps","boot_coverage", "boot_coverage_mcse","boot_width_winsor_pct","boot_width_winsor_pct_mcse", "boot_width","boot_width_mcse"), names_sep = "-") %>%
    pivot_longer(
      c(ends_with("normal"), ends_with("basic"), ends_with("student"), ends_with("percentile")),
      names_to = c(".value","CI_type"),
      names_pattern = c("(.+)-(.+)")
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
      K_boot_coverage = n(),
      boot_coverage = mean(cover),
      boot_coverage_mcse = sd(cover) / sqrt(n()),
      boot_width_winsor_pct = mean(width_winsor_pct),
      boot_width = mean(width_winz),
      boot_width_mcse = sd(width_winz) / sqrt(n()),
      .groups = "drop"
    )

  unnested_long %>%
    filter(bootstraps < Inf) %>%
    arrange(bootstraps, CI_type) %>%
    select(bootstraps, CI_type, K_boot_coverage, boot_coverage, boot_coverage_mcse, boot_width_winsor_pct, boot_width, boot_width_mcse) %>%
    expect_equal(by_hand)
})


test_that("extrapolate_coverage options work with infinite widths.", {

  res <- simulate_bootCIs(reps = 100, N = 50, mu = 0, nu = 5, B_vals = seq(49, 99, 10))
  res$CI[[1]]$normal_upper[1] <- Inf
  res$CI[[1]]$basic_upper[1:4] <- Inf
  res$CI[[1]]$student_upper[1:8] <- Inf
  res$CI[[1]]$percentile_upper <- Inf

  res_raw <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0)
  expect_true(is.infinite(res_raw$boot_width_normal[1]))
  expect_true(all(is.finite(res_raw$boot_width_normal[-1])))
  expect_true(is.infinite(res_raw$boot_width_basic[1]))
  expect_true(all(is.finite(res_raw$boot_width_basic[-1])))
  expect_true(all(is.infinite(res_raw$boot_width_student[1:2])))
  expect_true(all(is.finite(res_raw$boot_width_student[-(1:2)])))
  expect_true(all(is.infinite(res_raw$boot_width_percentile)))

  res_raw_trim <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, width_trim = 0.3)
  expect_true(all(is.finite(res_raw_trim$boot_width_normal)))
  expect_true(is.infinite(res_raw_trim$boot_width_basic[1]))
  expect_true(all(is.finite(res_raw_trim$boot_width_basic[-1])))
  expect_true(all(is.infinite(res_raw_trim$boot_width_student[1:2])))
  expect_true(all(is.finite(res_raw_trim$boot_width_student[-(1:2)])))
  expect_true(all(is.infinite(res_raw_trim$boot_width_percentile)))

  res_winz <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5)
  expect_true(all(is.finite(res_winz$boot_width_normal)))
  expect_true(all(is.finite(res_winz$boot_width_basic)))
  expect_true(all(is.finite(res_winz$boot_width_student)))
  expect_true(all(is.finite(res_winz$boot_width_percentile)))

  res$CI[[2]]$normal_upper[1] <- NaN
  res$CI[[2]]$basic_upper[1:4] <- NaN
  res$CI[[2]]$student_upper[1:8] <- NaN
  res$CI[[2]]$percentile_upper <- NaN

  res_raw <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, width_na_val = Inf)
  expect_true(is.infinite(res_raw$boot_width_normal[1]))
  expect_true(all(is.finite(res_raw$boot_width_normal[-1])))
  expect_true(is.infinite(res_raw$boot_width_basic[1]))
  expect_true(all(is.finite(res_raw$boot_width_basic[-1])))
  expect_true(all(is.infinite(res_raw$boot_width_student[1:2])))
  expect_true(all(is.finite(res_raw$boot_width_student[-(1:2)])))
  expect_true(all(is.infinite(res_raw$boot_width_percentile)))

  res_raw_trim <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, width_trim = 0.3, width_na_val = Inf)
  expect_true(all(is.finite(res_raw_trim$boot_width_normal)))
  expect_true(is.infinite(res_raw_trim$boot_width_basic[1]))
  expect_true(all(is.finite(res_raw_trim$boot_width_basic[-1])))
  expect_true(all(is.infinite(res_raw_trim$boot_width_student[1:2])))
  expect_true(all(is.finite(res_raw_trim$boot_width_student[-(1:2)])))
  expect_true(all(is.infinite(res_raw_trim$boot_width_percentile[1:6])))
  expect_true(all(is.infinite(res_raw_trim$boot_width_percentile[7])))

  res_winz <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5)
  expect_true(all(is.finite(res_winz$boot_width_normal)))
  expect_true(all(is.finite(res_winz$boot_width_basic)))
  expect_true(all(is.finite(res_winz$boot_width_student)))
  expect_true(all(is.finite(res_winz$boot_width_percentile)))

  res_winz_imp <- extrapolate_coverage(res, CI_subsamples = CI, true_param = 0, winz = 1.5, cover_na_val = 0, width_na_val = Inf)
  expect_true(all(is.finite(res_winz_imp$boot_width_normal)))
  expect_true(all(is.finite(res_winz_imp$boot_width_basic)))
  expect_true(all(is.finite(res_winz_imp$boot_width_student)))
  expect_true(all(is.finite(res_winz_imp$boot_width_percentile)))

  res_winz_diff <- abs(res_winz - res_winz_imp)
  expect_gt(res_winz_diff$boot_width_normal[1], 0)
  expect_equal(max(res_winz_diff$boot_width_normal[-1]), 0)
  expect_gt(res_winz_diff$boot_width_basic[1], 0)
  expect_equal(max(res_winz_diff$boot_width_basic[-1]), 0)
  expect_gt(min(res_winz_diff$boot_width_student[1:2]), 0)
  expect_equal(max(res_winz_diff$boot_width_normal[-(1:2)]), 0)
  expect_gt(min(res_winz_diff$boot_width_percentile[-7]), 0)
  expect_equal(res_winz_diff$boot_width_percentile[7], 0)

})
