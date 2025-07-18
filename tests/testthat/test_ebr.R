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

  expect_equal(
    res_f,
    df %>%
      mutate(res = pmap(., .f = f)) %>%
      unnest(cols = res)
  )

  fprod <- function(...) prod(...)
  res_prod <- evaluate_by_row(df, fprod, results_name = "res", system_time = FALSE, verbose = FALSE)
  expect_equal(res_f, res_prod)

  g <- function(a, b) data.frame(x = a * b, y = a + b)
  expect_equal(
    evaluate_by_row(df, g, results_name = "res", system_time = FALSE, verbose = FALSE),
    df %>%
      mutate(res = pmap(., .f = g)) %>%
      unnest(cols = res)
  )

})


test_that("evaluate_by_row() works with extra variables.", {

  df$c <- LETTERS[1:nrow(df)]
  f <- function(a, b) a * b
  fdot <- function(a, b, ...) a * b

  expect_message(res_f <- evaluate_by_row(df, f, results_name = "res", system_time = FALSE), "Evaluating f\\(\\) using the following variables: a, b")
  expect_message(res_fdot <- evaluate_by_row(df, fdot, results_name = "res", system_time = FALSE), "Evaluating fdot\\(\\) using the following variables: a, b, c")

  expect_equal(res_f, res_fdot)
  expect_equal(
    res_fdot,
    df %>%
      mutate(res = pmap(., .f = fdot)) %>%
      unnest(cols = res)
  )

  g <- function(a, b, d) data.frame(x = a * b * d, y = a + b + d)
  gdot <- function(a, b, d, ...) g(a, b, d)

  expect_error(evaluate_by_row(df, g, results_name = "res", system_time = FALSE, verbose = FALSE), 'argument "d" is missing, with no default')

  df$d <- 17:15

  expect_message(res_g <- evaluate_by_row(df, g, results_name = "res", system_time = FALSE), "Evaluating g\\(\\) using the following variables: a, b, d")
  expect_message(res_gdot <- evaluate_by_row(df, gdot, results_name = "res", system_time = FALSE), "Evaluating gdot\\(\\) using the following variables: a, b, c, d")

  expect_equal(res_g, res_gdot)
  expect_equal(
    res_gdot,
    df %>%
      mutate(res = pmap(., .f = gdot)) %>%
      unnest(cols = res)
  )

})
