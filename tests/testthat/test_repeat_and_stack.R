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

test_that("repeat_and_stack()'s ID argument works.", {

  N <- 4
  p <- 3
  idvar <- "ID"

  set.seed(20250504)
  list_anon <- repeat_and_stack(N, data.frame(x = rpois(p,lambda = 10)), stack = FALSE)
  set.seed(20250504)
  list_ID <- repeat_and_stack(N, data.frame(x = rpois(p,lambda = 10)), id = idvar, stack = FALSE)

  expect_false(identical(list_anon, list_ID))
  expect_equivalent(list_anon, list_ID)
  expect_identical(names(list_ID), as.character(1:N))

  set.seed(20250504)
  df_anon <- repeat_and_stack(N, data.frame(x = rpois(p,lambda = 10)))
  set.seed(20250504)
  df_ID <- repeat_and_stack(N, data.frame(x = rpois(p,lambda = 10)), id = idvar, stack = TRUE)

  expect_false(identical(df_anon, df_ID))
  expect_identical(df_anon$x, df_ID$x)
  expect_identical(df_ID$ID, rep(1:N, each = p))

  expect_identical(do.call(rbind, list_anon), df_anon)

  df_from_list <- data.frame(
    ID = rep(1:N, each = p),
    x = do.call(rbind, c(list_ID, make.row.names = FALSE))
  )
  expect_identical(df_from_list, df_ID)

})
