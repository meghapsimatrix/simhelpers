library(testthat)
library(simhelpers)
library(dplyr)
library(tibble)
library(future)
library(furrr)
library(tidyr)

# test_check("simhelpers")

set.seed(54321)
dat <- tibble(x = rnorm(10000, 1, 1), true_param = rep(1, 10000),
              p_value = runif(10000))

df <- data.frame(
  a = 3:5,
  b= seq(8, 16, 4)
)



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

test_that("evaluate_by_row works", {
  expect_equal(evaluate_by_row(df, function(a, b) a * b) %>% rename(res = .results), df %>% mutate(res = future_pmap(., .f = function(a, b) a * b)) %>% unnest(cols = res))
})
