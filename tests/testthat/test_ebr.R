library(testthat)
library(simhelpers)
library(dplyr)
library(tibble)
library(future)
library(furrr)
library(tidyr)

df <- data.frame(
  a = 3:5,
  b= seq(8, 16, 4)
)

test_that("evaluate_by_row works", {
  expect_equal(evaluate_by_row(df, function(a, b) a * b) %>% rename(res = .results), df %>% mutate(res = future_pmap(., .f = function(a, b) a * b)) %>% unnest(cols = res))
})

