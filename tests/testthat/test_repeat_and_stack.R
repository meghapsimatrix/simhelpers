test_that("repeat_and_stack() is equivalent to replicate().", {

  N <- rpois(1, lambda = 8)
  p <- rpois(1, lambda = 4)

  set.seed(20240730)
  base_res <- replicate(N, data.frame(x = rexp(p)), simplify = FALSE)

  set.seed(20240730)
  list_res <- repeat_and_stack(N, data.frame(x = rexp(p)), stack = FALSE)

  expect_identical(base_res, list_res)
  expect_identical(length(list_res), N)
  expect_identical(sapply(list_res, nrow), rep(p, N))

  set.seed(20240730)
  stack_res <- repeat_and_stack(N, data.frame(x = rexp(p)))

  do.call(rbind, base_res) |>
  expect_identical(stack_res)
  expect_identical(nrow(stack_res), N * p)
})
