skip_if_not_installed("dplyr")
skip_if_not_installed("tidyr")
skip_if_not_installed("purrr")

library(dplyr)
library(tidyr)
library(purrr)

data("welch_res")
data("Tipton_Pusto")

test_that("calc_*() functions can be used in combination inside of dplyr::summarize.", {

  alpha <- c(.01, .05)

  performance <-
    welch_res %>%
    group_by(n1, n2, mean_diff, method) %>%
    summarize(
      calc_absolute(estimates = est, true_param = mean_diff),
      calc_relative(estimates = est, true_param = mean_diff),
      calc_relative_var(estimates = est, var_estimates = var),
      calc_coverage(lower_bound = lower_bound, upper_bound = upper_bound, true_param = mean_diff),
      calc_rejection(p_values = p_val, alpha = alpha),
      .groups = "drop"
    )

  perf_vars <- names(performance)

  abs_vars <- c("K_absolute", eval(formals(calc_absolute)$criteria))
  abs_vars[3] <- "var"
  abs_mcse <- paste(abs_vars[-1], "mcse", sep = "_")

  rel_vars <- c("K_relative", eval(formals(calc_relative)$criteria))
  rel_vars <- sub("relative ", "rel_", rel_vars)
  rel_mcse <- paste(rel_vars[-1], "mcse", sep = "_")

  relvar_vars <- eval(formals(calc_relative_var)$criteria)
  relvar_vars <- c("K_relvar", paste(sub("relative ", "rel_", relvar_vars), "var", sep = "_"))
  relvar_mcse <- paste(relvar_vars[-1], "mcse", sep = "_")

  cov_vars <- c("K_coverage", eval(formals(calc_coverage)$criteria))
  cov_mcse <- paste(cov_vars[-1], "mcse", sep = "_")

  rej_vars <- c("K_rejection", paste("rej_rate", substr(as.character(alpha),3,5), sep = "_"))
  rej_mcse <- paste("rej_rate_mcse", substr(as.character(alpha),3,5), sep = "_")

  all_vars <- c(abs_vars, abs_mcse, rel_vars, rel_mcse, relvar_vars, relvar_mcse, cov_vars, cov_mcse, rej_vars, rej_mcse)
  expect_true(all(all_vars %in% perf_vars))
  expect_identical(setdiff(perf_vars, all_vars), c('n1', 'n2', 'mean_diff', 'method'))
})

test_that("calc_*() functions can be used in combination inside of dplyr::summarize.", {

  # multiple calc_absolute calls

  est_names <- c("hom","Welch")
  performance <-
    welch_res %>%
    select(n1, n2, mean_diff, method, est, var) %>%
    mutate(
      method = recode(method, "t-test" = est_names[1], "Welch t-test" = est_names[2])
    ) %>%
    group_by(n1, n2, mean_diff, method) %>%
    mutate(
      id = row_number(),
    ) %>%
    group_by(n1, n2, mean_diff) %>%
    pivot_wider(names_from = "method", values_from = c(est, var)) %>%
    summarize(
      hom = calc_absolute(estimates = est_hom, true_param = mean_diff),
      Welch = calc_absolute(estimates = est_Welch, true_param = mean_diff),
      .groups = "drop"
    ) %>%
    unnest(cols = c(hom, Welch), names_sep = "_")

  perf_vars <- names(performance)

  abs_vars <- c("K_absolute", eval(formals(calc_absolute)$criteria))
  abs_vars[3] <- "var"
  abs_mcse <- paste(abs_vars[-1], "mcse", sep = "_")

  all_vars <-
    expand_grid(est = est_names, perf = c(abs_vars, abs_mcse)) %>%
    pmap_chr(paste, sep = "_")

  expect_true(all(all_vars %in% perf_vars))
  expect_identical(setdiff(perf_vars, all_vars), c('n1', 'n2', 'mean_diff'))

  # summarize across() multiple estimators

  test_names <- c("AHA","AHB","AHZ","EDF","EDT")
  performance <-
    Tipton_Pusto %>%
    select(num_studies, r, Isq, test, contrast, q, pval = rej_rate) %>%
    pivot_wider(names_from = "test", values_from = pval) %>%
    group_by(num_studies, Isq, q) %>%
    summarise(
      across(all_of(test_names), ~ calc_rejection(p_values = .)),
      .groups = "drop"
    ) %>%
    unnest(cols = all_of(test_names), names_sep = "_")

  perf_vars <- names(performance)

  rej_vars <- c("K_rejection", "rej_rate","rej_rate_mcse")

  all_vars <-
    expand_grid(test_names, rej_vars) %>%
    pmap_chr(paste, sep = "_")

  expect_true(all(all_vars %in% perf_vars))
  expect_identical(setdiff(perf_vars, all_vars), c('num_studies', 'Isq', 'q'))

})
