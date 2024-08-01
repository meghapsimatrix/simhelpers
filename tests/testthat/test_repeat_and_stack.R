test_that("repeat_and_stack() is equivalent to replicate() for simple expressions.", {

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


test_that("repeat_and_stack() is equivalent to replicate() for multi-line expressions.", {

  N <- rpois(1, lambda = 8)
  p <- rpois(1, lambda = 4)

  set.seed(20240730)
  base_res <- replicate(
    n = N,
    expr = {
      x <- rexp(p)
      data.frame(M = mean(x), SE = sd(x) / sqrt(p))
    },
    simplify = FALSE
  )

  set.seed(20240730)
  list_res <- repeat_and_stack(
    N,
    expr = {
      x <- rexp(p)
      data.frame(M = mean(x), SE = sd(x) / sqrt(p))
    },
    stack = FALSE
  )

  expect_identical(base_res, list_res)
  expect_identical(length(list_res), N)
  expect_identical(sapply(list_res, nrow), rep(1L, N))
  expect_identical(lapply(list_res, names), rep(list(c("M","SE")), N))

  set.seed(20240730)
  stack_res <- repeat_and_stack(
    N,
    expr = {
      x <- rexp(p)
      data.frame(M = mean(x), SE = sd(x) / sqrt(p))
    }
  )

  do.call(rbind, base_res) |>
    expect_identical(stack_res)
  expect_identical(nrow(stack_res), N)
  expect_identical(names(stack_res), c("M","SE"))

})
