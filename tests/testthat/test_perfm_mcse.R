library(testthat)
library(simhelpers)
library(dplyr)
library(tibble)
library(future)
library(furrr)
library(tidyr)


set.seed(54321)
dat <- tibble(x = rnorm(10000, 1, 1), true_param = rep(1, 10000),
              p_value = runif(10000))

K <- nrow(dat)
t_bar <- mean(dat$x)
s_t <- sd(dat$x)
k_t <- (1/(K * s_t^4)) * sum((dat$x - mean(dat$x))^4)
g_t <- (1/(K * s_t^3)) * sum((dat$x - mean(dat$x))^3)

t_bar_j <- (1/(K-1)) * (K * t_bar - dat$x)
s_sq_t_j <- (1/(K-2)) * ((K - 1) * var(dat$x) - (K/(K-1)) * (dat$x - t_bar)^2)
rmse <- sqrt(mean((dat$x - 1)^2))
rmse_j <- sqrt((t_bar_j - 1)^2 + s_sq_t_j)




test_that("check the performance measures", {
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "bias") %>% pull(bias), mean(dat$x) - 1)
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "variance") %>% pull(var), var(dat$x))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "mse") %>% pull(mse), mean((dat$x - 1)^2))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "rmse") %>% pull(rmse), sqrt(mean((dat$x - 1)^2)))
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative bias") %>% pull(rel_bias), mean(dat$x)/1)
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative mse") %>% pull(rel_mse), (mean(dat$x - 1)^2 + var(dat$x))/ 1^2)
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative rmse") %>% pull(rel_rmse), sqrt((mean(dat$x - 1)^2 + var(dat$x))/ 1^2))
  expect_equal(calc_rejection(dat, p_values = p_value) %>% pull(rej_rate), mean(dat$p_value < .05))
  expect_equal(calc_rejection(dat, p_values = p_value, alpha = .10) %>% pull(rej_rate), mean(dat$p_value < .10))
  expect_equal(calc_rejection(dat, p_values = p_value, alpha = .01) %>% pull(rej_rate), mean(dat$p_value < .01))
  expect_equal(calc_coverage(t_res, lower_bound, upper_bound, true_param, perfm_criteria = "coverage") %>% pull(coverage), mean(t_res$lower_bound < t_res$true_param & t_res$true_param < t_res$upper_bound))
})

test_that("check the mcse", {
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "bias") %>% pull(bias_mcse), sqrt(var(dat$x)/nrow(dat)))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "variance") %>% pull(var_mcse), (var(dat$x) * sqrt((k_t - 1)/K)))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "mse") %>% pull(mse_mcse), sqrt((1/K) * (s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * (mean(dat$x) - 1) + 4 * s_t^2 * (mean(dat$x - 1)^2))))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "rmse") %>% pull(rmse_mcse), sqrt((1/K) * sum((rmse_j - rmse)^2)))
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative bias") %>% pull(rel_bias_mcse), sqrt(var(dat$x)/(nrow(dat) * 1^2)))
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative mse") %>% pull(rel_mse_mcse), sqrt((1/(K * 1^2)) * (s_t^4 * (k_t  - 1) + 4 * s_t^3 * g_t * (mean(dat$x) - 1) + 4 * s_t^2 * (mean(dat$x) - 1)^2)))
  expect_equal(calc_rejection(dat, p_values = p_value) %>% pull(rej_rate_mcse), sqrt((mean(dat$p_value < .05) * (1 - mean(dat$p_value < .05)))/K))
})



