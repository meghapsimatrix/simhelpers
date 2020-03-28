library(testthat)
library(simhelpers)
library(dplyr)
library(tibble)
library(future)
library(furrr)
library(tidyr)


# absolute criteria
set.seed(54321)
dat <- tibble(x = rnorm(10000, 2, 1), true_param = rep(2, 10000),
              p_value = runif(10000))

dat_abs <- dat %>%
  select(x, true_param) %>%
  mutate(x = if_else(x > 4, as.double(NA), x)) %>%
  filter(!is.na(x))

dat_rej <- dat %>%
  select(p_value, true_param) %>%
  mutate(p_value = if_else(p_value > .95, as.double(NA), p_value)) %>%
  filter(!is.na(p_value))


t_p <- 2
K_abs <- nrow(dat_abs)
K_rej <- nrow(dat_rej)
t_bar <- mean(dat_abs$x)
s_t <- sd(dat_abs$x)
k_t <- (1/(K_abs * s_t^4)) * sum((dat_abs$x - mean(dat_abs$x))^4)
g_t <- (1/(K_abs * s_t^3)) * sum((dat_abs$x - mean(dat_abs$x))^3)

g_t <- (1/(K_abs * s_t^3)) * sum((dat_abs$x - mean(dat_abs$x))^3)

t_bar_j <- (1/(K_abs - 1)) * (K_abs * t_bar - dat_abs$x)
s_sq_t_j <- (1/(K_abs - 2)) * ((K_abs - 1) * var(dat_abs$x, na.rm = TRUE) - (K_abs/(K_abs - 1)) * (dat_abs$x - t_bar)^2)
rmse <- sqrt(mean((dat_abs$x - t_p)^2))
rmse_j <- sqrt((t_bar_j - t_p)^2 + s_sq_t_j)


# coverage
t_res <- t_res %>%
  mutate(lower_bound = if_else(lower_bound > .5, as.double(NA), lower_bound),
         upper_bound = ifelse(is.na(lower_bound), as.double(NA), upper_bound)) %>%
  filter(complete.cases(lower_bound, upper_bound))

cov <- mean(t_res$lower_bound <= t_res$true_param & t_res$true_param <= t_res$upper_bound)

# variance related
alpha_res <- alpha_res %>%
  mutate(Var_A = if_else(Var_A > .007, as.double(NA), Var_A)) %>%
  filter(!is.na(Var_A))

v_bar <- mean(alpha_res$Var_A)
s_sq_t <- var(alpha_res$A)
s_sq_v <- var(alpha_res$Var_A)
K_alpha <- nrow(alpha_res)

rb_var <- v_bar/s_sq_t

v_bar_j <- (1/(K_alpha-1)) * (K_alpha * v_bar - alpha_res$Var_A)
s_sq_t_j_alpha <- (1/(K_alpha - 2)) * ((K_alpha - 1) * s_sq_t - (K_alpha/(K_alpha - 1)) * (alpha_res$A - mean(alpha_res$A))^2)
s_sq_v_j_alpha <- (1/(K_alpha - 2)) * ((K_alpha - 1) * s_sq_v - (K_alpha/(K_alpha - 1)) * (alpha_res$Var_A - mean(alpha_res$Var_A))^2)


calc_rejection(t_res, p_values = p_val)

test_that("check the performance measures", {
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "bias") %>% pull(bias), mean(dat_abs$x) - t_p)
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "variance") %>% pull(var), var(dat_abs$x))
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "mse") %>% pull(mse), mean((dat_abs$x - t_p)^2))
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "rmse") %>% pull(rmse), sqrt(mean((dat_abs$x - t_p)^2)))
  expect_equal(calc_relative(dat_abs, x, true_param, perfm_criteria = "relative bias") %>% pull(rel_bias), mean(dat_abs$x)/t_p)
  expect_equal(calc_relative(dat_abs, x, true_param, perfm_criteria = "relative mse") %>% pull(rel_mse), (mean(dat_abs$x - t_p)^2 + var(dat_abs$x))/ t_p^2)
  expect_equal(calc_relative(dat_abs, x, true_param, perfm_criteria = "relative rmse") %>% pull(rel_rmse), sqrt((mean(dat_abs$x - t_p)^2 + var(dat_abs$x))/ t_p^2))
  expect_equal(calc_rejection(dat_rej, p_values = p_value) %>% pull(rej_rate), mean(dat_rej$p_value < .05))
  expect_equal(calc_rejection(dat_rej, p_values = p_value, alpha = .10) %>% pull(rej_rate), mean(dat_rej$p_value < .10))
  expect_equal(calc_rejection(dat_rej, p_values = p_value, alpha = .01) %>% pull(rej_rate), mean(dat_rej$p_value < .01))
  expect_equal(calc_coverage(t_res, lower_bound, upper_bound, true_param, perfm_criteria = "coverage") %>% pull(coverage), cov)
  expect_equal(calc_coverage(t_res, lower_bound, upper_bound, true_param, perfm_criteria = "width") %>% pull(width), mean(t_res$upper_bound) - mean(t_res$lower_bound))

})

test_that("check the mcse", {
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "bias") %>% pull(bias_mcse), sqrt(var(dat_abs$x)/nrow(dat_abs)))
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "variance") %>% pull(var_mcse), (var(dat_abs$x) * sqrt((k_t - 1)/K_abs)))
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "mse") %>% pull(mse_mcse), sqrt((1/K_abs) * (s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * (mean(dat_abs$x) - t_p) + 4 * s_t^2 * (mean(dat_abs$x - t_p)^2))))
  expect_equal(calc_absolute(dat_abs, x, true_param, perfm_criteria = "rmse") %>% pull(rmse_mcse), sqrt((1/K_abs) * sum((rmse_j - rmse)^2)))
  expect_equal(calc_relative(dat_abs, x, true_param, perfm_criteria = "relative bias") %>% pull(rel_bias_mcse), sqrt(var(dat_abs$x)/(nrow(dat_abs) * t_p^2)))
  expect_equal(calc_relative(dat_abs, x, true_param, perfm_criteria = "relative mse") %>% pull(rel_mse_mcse), sqrt((1/(K_abs * t_p^2)) * (s_t^4 * (k_t  - 1) + 4 * s_t^3 * g_t * (mean(dat_abs$x) - t_p) + 4 * s_t^2 * (mean(dat_abs$x) - t_p)^2)))
  expect_equal(calc_rejection(dat_rej, p_values = p_value) %>% pull(rej_rate_mcse), sqrt((mean(dat_rej$p_value < .05) * (1 - mean(dat_rej$p_value < .05)))/K_rej))
  expect_equal(calc_coverage(t_res, lower_bound, upper_bound, true_param, perfm_criteria = "coverage") %>% pull(coverage_mcse), sqrt((cov * (1 - cov))/nrow(t_res)))
  expect_equal(calc_coverage(t_res, lower_bound, upper_bound, true_param, perfm_criteria = "width") %>% pull(width_mcse), sqrt(var(t_res$upper_bound - t_res$lower_bound)/nrow(t_res)))
})

test_that("check perfm var jk", {
  expect_equal(calc_relative_var(alpha_res, A, Var_A, perfm_criteria = "relative bias") %>% pull(rel_bias_var), v_bar/s_sq_t)
  expect_equal(calc_relative_var(alpha_res, A, Var_A, perfm_criteria = "relative mse") %>% pull(rel_mse_var), ((v_bar - s_sq_t)^2 + s_sq_v)/ s_sq_t^2)
  expect_equal(calc_relative_var(alpha_res, A, Var_A, perfm_criteria = "relative rmse") %>% pull(rel_rmse_var), sqrt(((v_bar - s_sq_t)^2 + s_sq_v)/ s_sq_t^2))
})


test_that("check mcse var jk", {
  expect_equal(calc_relative_var(alpha_res, A, Var_A, perfm_criteria = "relative bias") %>% pull(rel_bias_var_mcse), sqrt((1/K_alpha)  * sum((v_bar_j/s_sq_t_j_alpha - v_bar/s_sq_t)^2)))
  expect_equal(calc_relative_var(alpha_res, A, Var_A, perfm_criteria = "relative mse") %>% pull(rel_mse_var_mcse), sqrt((1/(K_alpha)) * sum((((v_bar_j - s_sq_t_j_alpha)^2 + s_sq_v_j_alpha)/ s_sq_t_j_alpha^2 - ((v_bar - s_sq_t)^2 + s_sq_v)/ s_sq_t^2)^2)))
  expect_equal(calc_relative_var(alpha_res, A, Var_A, perfm_criteria = "relative rmse") %>% pull(rel_rmse_var_mcse), sqrt((1/(K_alpha)) * sum((sqrt(((v_bar_j - s_sq_t_j_alpha)^2 + s_sq_v_j_alpha)/ s_sq_t_j_alpha^2) - sqrt(((v_bar - s_sq_t)^2 + s_sq_v)/ s_sq_t^2))^2)))
})
