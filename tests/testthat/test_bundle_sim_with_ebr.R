skip_if_not_installed("dplyr")
skip_if_not_installed("tidyr")
skip_if_not_installed("purrr")

library(dplyr)
library(tidyr)
library(purrr)

generate_chisq_samples <- function(nA, nB, mu_A, mu_B) {

  YA <- rchisq(n = nA, df = mu_A)
  YB <- rchisq(n = nB, df = mu_B)

  sample_data <- data.frame(
    group = rep(c("A","B"), times = c(nA, nB)),
    Y = c(YA, YB)
  )
  return(sample_data)
}

t_test <- function(sample_data) {

  # calculate raw summary statistics
  Ns <- table(sample_data$group)
  means <- tapply(sample_data$Y, sample_data$group, mean)
  sds <- tapply(sample_data$Y, sample_data$group, sd)

  # t-test
  tstat <- (means[[2]] - means[[1]]) / sqrt(sum(sds^2 / Ns))
  df <- sum(sds^2 / Ns)^2 / sum(sds^4 / (Ns^2 * (Ns - 1)))
  pval <- 2 * pt(q = abs(tstat), df = df, lower.tail = FALSE)

  return(data.frame(tstat, df, pval))
}

run_t_tests <- function(sample_data) {
  t_raw <- t_test(sample_data)
  t_raw$transform <- "raw"

  log_data <- data.frame(
    group = sample_data$group,
    Y = log(sample_data$Y)
  )
  t_log <- t_test(log_data)
  t_log$transform <- "log"

  return(rbind(t_raw, t_log))
}

eval_t_tests <- function(replications) {
  replications %>%
    group_by(transform) %>%
    summarize(
      rate_05 = mean(pval < .05),
      rate_10 = mean(pval < .10)
    )
}

simmer_A <- bundle_sim(
  f_generate = generate_chisq_samples,
  f_analyze = run_t_tests
)

simmer_B <- bundle_sim(
  f_generate = generate_chisq_samples,
  f_analyze = run_t_tests,
  f_summarize = eval_t_tests,
)

run_sim <- function(reps, nA, nB, mu_A, mu_B, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  replications <-
    map_dfr(1:reps, ~ {
      dat <- generate_chisq_samples(nA = nA, nB = nB, mu_A = mu_A, mu_B = mu_B)
      run_t_tests(dat)
    }) %>%
    bind_rows()

  replications %>%
    eval_t_tests()
}

design_factors <- list(
  mu_A = c(10, 15, 20),
  nA = c(8, 16),
  nB = c(8, 16)
)

params <-
  expand_grid(!!!design_factors) %>%
  mutate(
    mu_B = mu_A,
    reps = 15,
  )

test_that("bundle_sim functions work with pmap().", {

  set.seed(20240221)
  res_run_sim <- pmap_dfr(params, run_sim)

  set.seed(20240221)
  res_simmer_A <-
    params %>%
    mutate(
      res = pmap(., .f = simmer_A),
      res = map(res, eval_t_tests)
    ) %>%
    unnest(res)

  expect_identical(
    res_run_sim,
    select(res_simmer_A, transform, rate_05, rate_10)
  )

  set.seed(20240221)
  res_simmer_B <- pmap_dfr(params, .f = simmer_B)
  expect_identical(res_run_sim, res_simmer_B)

})

test_that("bundle_sim functions work with evaluate_by_row().", {

  params$seed <- 20240222 + 1:nrow(params)

  res_run_sim <-
    evaluate_by_row(
      params, run_sim,
      .options = furrr::furrr_options(seed = NULL)
    )

  res_simmer_A <-
    evaluate_by_row(
      params, simmer_A,
      .options = furrr::furrr_options(seed = NULL)
    ) %>%
    group_by(mu_A, nA, nB, mu_B, reps, seed) %>%
    group_modify(~ eval_t_tests(.)) %>%
    ungroup()
  expect_identical(res_run_sim, res_simmer_A)

  res_simmer_B <-
    evaluate_by_row(
      params, simmer_B,
      .options = furrr::furrr_options(seed = NULL)
    )
  expect_identical(res_run_sim, res_simmer_B)

})


