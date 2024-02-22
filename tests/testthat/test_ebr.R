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
  expect_equal(
    evaluate_by_row(df, f, results_name = "res"),
    df %>%
      mutate(res = pmap(., .f = f)) %>%
      unnest(cols = res)
  )

  g <- function(a, b) data.frame(x = a * b, y = a + b)
  expect_equal(
    evaluate_by_row(df, g, results_name = "res"),
    df %>%
      mutate(res = pmap(., .f = g)) %>%
      unnest(cols = res)
  )

})


