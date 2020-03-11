library(testthat)
library(simhelpers)
library(tidyverse)

test_check("simhelpers")

set.seed(54321)
dat <- tibble(x = rnorm(10000, 1, 1), true_param = rep(1, 10000))


test_that("check the performance measures", {
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "bias") %>% pull(bias), mean(dat$x) - 1)
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "variance") %>% pull(var), var(dat$x))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "mse") %>% pull(mse), mean((dat$x - 1)^2))
  expect_equal(calc_absolute(dat, x, true_param, perfm_criteria = "rmse") %>% pull(rmse), sqrt(mean((dat$x - 1)^2)))
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative bias") %>% pull(rel_bias), mean(dat$x)/1)
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative mse") %>% pull(rel_mse), (mean(dat$x - 1)^2 + var(dat$x))/ 1^2)
  expect_equal(calc_relative(dat, x, true_param, perfm_criteria = "relative rmse") %>% pull(rel_rmse), sqrt((mean(dat$x - 1)^2 + var(dat$x))/ 1^2))
})
