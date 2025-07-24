skip_if_not_installed("dplyr")
skip_if_not_installed("tidyr")

library(dplyr)
library(tidyr)

df <- data.frame(
  a = 3:5,
  b= seq(8, 16, 4)
)

test_that("evaluate_by_row() works with generic functions.", {

  f <- function(a, b) a * b
  res_f <- evaluate_by_row(df, f, results_name = "res", system_time = FALSE, verbose = FALSE)
  hand_f <-
    df %>%
    mutate(res = pmap(., .f = f)) %>%
    unnest(cols = res)

  expect_equal(res_f, hand_f)

  fprod <- function(...) prod(...)
  res_prod <- evaluate_by_row(df, fprod, results_name = "res", system_time = FALSE, verbose = FALSE)
  expect_equal(res_f, res_prod)

  g <- function(a, b) data.frame(x = a * b, y = a + b)
  res_g <- evaluate_by_row(df, g, results_name = "res", system_time = FALSE, verbose = FALSE)
  nest_g <- evaluate_by_row(df, g, nest_results = TRUE, results_name = "res", system_time = FALSE, verbose = FALSE)
  hand_g <- df %>% mutate(res = pmap(., .f = g))

  expect_equal(nest_g, hand_g)
  expect_equal(res_g, unnest(hand_g, cols = res))

})


test_that("evaluate_by_row() works with extra variables.", {

  df$c <- LETTERS[1:nrow(df)]
  f <- function(a, b) a * b
  fdot <- function(a, b, ...) a * b

  expect_message(res_f <- evaluate_by_row(df, f, results_name = "res", system_time = FALSE), "Evaluating f\\(\\) using the following variables: a, b")
  expect_message(res_fdot <- evaluate_by_row(df, fdot, results_name = "res", system_time = FALSE), "Evaluating fdot\\(\\) using the following variables: a, b, c")
  expect_message(nest_fdot <- evaluate_by_row(df, fdot, nest_results = TRUE, results_name = "res", system_time = FALSE), "Evaluating fdot\\(\\) using the following variables: a, b, c")

  hand_fdot <- df %>% mutate(res = pmap(., .f = fdot))

  expect_equal(res_f, res_fdot)
  expect_equal(nest_fdot, hand_fdot)
  expect_equal(res_fdot, unnest(hand_fdot, cols = res))

  g <- function(a, b, d) data.frame(x = a * b * d, y = a + b + d)
  gdot <- function(a, b, d, ...) g(a, b, d)

  expect_error(evaluate_by_row(df, g, results_name = "res", system_time = FALSE, verbose = FALSE), 'argument "d" is missing, with no default')

  df$d <- 17:15

  expect_message(res_g <- evaluate_by_row(df, g, results_name = "res", system_time = FALSE), "Evaluating g\\(\\) using the following variables: a, b, d")
  expect_message(res_gdot <- evaluate_by_row(df, gdot, results_name = "res", system_time = FALSE), "Evaluating gdot\\(\\) using the following variables: a, b, c, d")
  expect_message(nest_gdot <- evaluate_by_row(df, gdot, nest_results = TRUE, results_name = "res", system_time = FALSE), "Evaluating gdot\\(\\) using the following variables: a, b, c, d")

  hand_gdot <- df %>% mutate(res = pmap(., .f = gdot))

  expect_equal(res_g, res_gdot)
  expect_equal(nest_gdot, hand_gdot)
  expect_equal(res_gdot, unnest(hand_gdot, cols = res))

})
