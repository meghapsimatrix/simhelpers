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
s_t <- sd(dat$x)
k_t <- (1/(K * s_t^4)) * sum((dat$x - mean(dat$x))^4)
g_t <- (1/(K * s_t^3)) * sum((dat$x - mean(dat$x))^3)



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
})

test_that("check the mcse", {
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "bias") %>% pull(bias_mcse), sqrt(var(dat$x)/nrow(dat)))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "variance") %>% pull(var_mcse), (var(dat$x) * sqrt((k_t - 1)/K)))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "mse") %>% pull(mse_mcse), sqrt((1/K) * (s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * (mean(dat$x) - 1) + 4 * s_t^2 * (mean(dat$x - 1)^2))))
  expect_equal(calc_rejection(dat, p_values = p_value) %>% pull(rej_rate_mcse), sqrt((mean(dat$p_value < .05) * (1 - mean(dat$p_value < .05)))/K))
})


