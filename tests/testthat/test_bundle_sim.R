f_G1 <- rnorm

f_A1 <- function(x, trim = 0) data.frame(y_bar = mean(x, trim = trim))

f_S1 <- function(x, calc_sd = FALSE) {
  if (calc_sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

test_that("bundle_sim() works for a simple DGP.", {

  skip_on_cran()
  sim1 <- bundle_sim(f_generate = f_G1, f_analyze = f_A1)
  expect_identical(formalArgs(sim1), c("reps","n","mean","sd","trim","seed"))

  res1 <- sim1(24, n = 7, seed = 20240107)
  set.seed(20240107)
  dat1 <- data.frame(rep = rep(1:24, each = 7), y = rnorm(24 * 7))
  ybar1 <- with(dat1, tapply(y, rep, mean)) |> as.double()
  expect_identical(res1$y_bar, ybar1)

  res2 <- sim1(24, n = 7, mean = 0, sd = 1, seed = 20240107)
  expect_identical(res1, res2)

  set.seed(20240107)
  res3 <- sim1(24, n = 7)
  expect_identical(res1, res3)

  res4 <- sim1(14, n = 78, sd = 25, trim = 0.1, seed = 20240107)
  set.seed(20240107)
  dat4 <- data.frame(rep = rep(1:14, each = 78), y = rnorm(14 * 78, sd = 25))
  ybar4 <- with(dat4, tapply(y, rep, mean, trim = 0.1)) |> as.double()
  expect_identical(res4$y_bar, ybar4)

  sim1a <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, row_bind_reps = FALSE)
  expect_identical(formals(sim1), formals(sim1a))
  res1a <-  sim1a(24, n = 7, seed = 20240107)
  expect_is(res1a, "list")
  expect_length(res1a, 24)
  expect_identical(res1, do.call(rbind, res1a))

  sim2 <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                     reps_name = "R", seed_name = ".seed", summarize_opt_name = "Agg")
  expect_identical(formalArgs(sim2), c("R","n","mean","sd","trim","calc_sd",".seed", "Agg"))

  res5 <- sim2(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107, Agg = FALSE)
  res6 <- sim2(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107)
  set.seed(20240107)
  dat5 <- data.frame(rep = rep(1:93, each = 350), y = rnorm(93 * 350, mean = -3, sd = 1))
  ybar5 <- with(dat5, tapply(y, rep, mean)) |> as.double()
  expect_identical(res5$y_bar, ybar5)
  expect_identical(res6$M, mean(dat5$y))
  expect_identical(res6$SD, sd(ybar5))
  expect_identical(f_S1(res5, calc_sd = TRUE), res6)

  res7 <- sim2(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107, Agg = FALSE)
  res8 <- sim2(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107)
  set.seed(20240107)
  dat7 <- data.frame(rep = rep(1:93, each = 350), y = rnorm(93 * 350, mean = -4/3, sd = 1))
  ybar7 <- with(dat7, tapply(y, rep, mean, trim = 0.3)) |> as.double()
  expect_identical(res7$y_bar, ybar7)
  expect_gt(abs(res8$M - mean(dat7$y)), 0)
  expect_identical(res8$M, mean(ybar7))
  expect_identical(res8$SD, sd(ybar7))
  expect_identical(f_S1(res7, calc_sd = TRUE), res8)

  sim2a <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                      reps_name = "R", seed_name = ".seed", summarize_opt_name = "Agg", row_bind_reps = FALSE)
  expect_identical(formals(sim2), formals(sim2a))
  res5a <- sim2a(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107, Agg = FALSE)
  expect_is(res5a, "list")
  expect_length(res5a, 93)
  expect_identical(res5, do.call(rbind, res5a))
  expect_error(
    sim2a(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107)
  )

  sim2b <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                      reps_name = "R", seed_name = ".seed", summarize_opt_name = NULL)
  res8b <- sim2b(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107)
  expect_identical(res8, res8b)

  sim2c <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                      reps_name = "R", seed_name = ".seed", summarize_opt_name = NULL, row_bind_reps = FALSE)
  expect_error(
    sim2c(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107)
  )

})

test_that("bundle_sim() works for a simple DGP, tests for CRAN.", {

  sim1 <- bundle_sim(f_generate = f_G1, f_analyze = f_A1)
  expect_equal(formalArgs(sim1), c("reps","n","mean","sd","trim","seed"))

  res1 <- sim1(24, n = 7, seed = 20240107)
  set.seed(20240107)
  dat1 <- data.frame(rep = rep(1:24, each = 7), y = rnorm(24 * 7))
  ybar1 <- with(dat1, tapply(y, rep, mean)) |> as.double()
  expect_equal(res1$y_bar, ybar1)

  res2 <- sim1(24, n = 7, mean = 0, sd = 1, seed = 20240107)
  expect_equal(res1, res2)

  set.seed(20240107)
  res3 <- sim1(24, n = 7)
  expect_equal(res1, res3)

  res4 <- sim1(14, n = 78, sd = 25, trim = 0.1, seed = 20240107)
  set.seed(20240107)
  dat4 <- data.frame(rep = rep(1:14, each = 78), y = rnorm(14 * 78, sd = 25))
  ybar4 <- with(dat4, tapply(y, rep, mean, trim = 0.1)) |> as.double()
  expect_equal(res4$y_bar, ybar4)

  sim1a <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, row_bind_reps = FALSE)
  expect_equal(formals(sim1), formals(sim1a))
  res1a <-  sim1a(24, n = 7, seed = 20240107)
  expect_is(res1a, "list")
  expect_length(res1a, 24)
  expect_equal(res1, do.call(rbind, res1a))

  sim2 <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                     reps_name = "R", seed_name = ".seed", summarize_opt_name = "Agg")
  expect_equal(formalArgs(sim2), c("R","n","mean","sd","trim","calc_sd",".seed", "Agg"))

  res5 <- sim2(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107, Agg = FALSE)
  res6 <- sim2(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107)
  set.seed(20240107)
  dat5 <- data.frame(rep = rep(1:93, each = 350), y = rnorm(93 * 350, mean = -3, sd = 1))
  ybar5 <- with(dat5, tapply(y, rep, mean)) |> as.double()
  expect_equal(res5$y_bar, ybar5)
  expect_equal(res6$M, mean(dat5$y))
  expect_equal(res6$SD, sd(ybar5))
  expect_equal(f_S1(res5, calc_sd = TRUE), res6)

  res7 <- sim2(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107, Agg = FALSE)
  res8 <- sim2(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107)
  set.seed(20240107)
  dat7 <- data.frame(rep = rep(1:93, each = 350), y = rnorm(93 * 350, mean = -4/3, sd = 1))
  ybar7 <- with(dat7, tapply(y, rep, mean, trim = 0.3)) |> as.double()
  expect_equal(res7$y_bar, ybar7)
  expect_gt(abs(res8$M - mean(dat7$y)), 0)
  expect_equal(res8$M, mean(ybar7))
  expect_equal(res8$SD, sd(ybar7))
  expect_equal(f_S1(res7, calc_sd = TRUE), res8)

  sim2a <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                      reps_name = "R", seed_name = ".seed", summarize_opt_name = "Agg", row_bind_reps = FALSE)
  expect_equal(formals(sim2), formals(sim2a))
  res5a <- sim2a(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107, Agg = FALSE)
  expect_is(res5a, "list")
  expect_length(res5a, 93)
  expect_equal(res5, do.call(rbind, res5a))
  expect_error(
    sim2a(93, n = 350, mean = -3, calc_sd = TRUE, .seed = 20240107)
  )

  sim2b <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                      reps_name = "R", seed_name = ".seed", summarize_opt_name = NULL)
  res8b <- sim2b(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107)
  expect_equal(res8, res8b)

  sim2c <- bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1,
                      reps_name = "R", seed_name = ".seed", summarize_opt_name = NULL, row_bind_reps = FALSE)
  expect_error(
    sim2c(93, n = 350, mean = -4/3, trim = 0.3, calc_sd = TRUE, .seed = 20240107)
  )

})

f_G2 <- function(N, p, df, rho, Rsq) {
  X <- matrix(rnorm(N*p), N, p)
  dat <- as.data.frame(X)
  y_var <- var(rowSums(X)) * (1 - Rsq) / Rsq
  dat$Y <- rowSums(X) + sqrt(y_var) * rt(N, df = df)
  dat
}

test_that("bundle_sim() works with identity functions.", {

  sim3 <- bundle_sim(f_generate = f_G2, f_analyze = identity,
                     reps_name = "n_reps", seed_name = NULL)
  expect_identical(
    as.list(formals(sim3)),
    c(alist(n_reps = ), as.list(formals(f_G2)))
  )

  set.seed(20240108)
  dat3 <- sim3(n_reps = 40, N = 8, p = 4, df = 3, rho = 0.6, Rsq = 0.5)
  set.seed(20240108)
  dat3m <- replicate(40, f_G2(N = 8, p = 4, df = 3, rho = 0.6, Rsq = 0.5), simplify = FALSE)
  dat3m <- do.call(rbind, dat3m)
  expect_identical(dat3, dat3m)

  sim3a <- bundle_sim(f_generate = f_G2, f_analyze = identity,
                      reps_name = "n_reps", seed_name = NULL, row_bind_reps = FALSE)
  expect_identical(formals(sim3), formals(sim3a))
  set.seed(20240108)
  dat3a <- sim3a(n_reps = 40, N = 8, p = 4, df = 3, rho = 0.6, Rsq = 0.5)
  expect_is(dat3a, "list")
  expect_length(dat3a, 40)
  expect_identical(dat3, do.call(rbind, dat3a))

  sim4 <- bundle_sim(f_generate = f_G2, f_analyze = identity, f_summarize = identity,
                     reps_name = "n_reps", seed_name = ".s", summarize_opt_name = "evaluate")
  expect_identical(
    as.list(formals(sim4)),
    c(alist(n_reps = ), as.list(formals(f_G2)), .s = NA_integer_, evaluate = TRUE)
  )

  arg_list <- list(
    n_reps = rpois(1, 40) + 1L,
    N = rpois(1, 3) + 8L,
    p = sample(1:5, 1),
    df = sample(4:10, 1),
    rho = 0.3,
    Rsq = 0.8
  )
  set.seed(20240108)
  dat4 <- do.call(sim4, arg_list)
  set.seed(20240108)
  dat5 <- do.call(sim4, c(arg_list, evaluate = FALSE))
  set.seed(20240108)
  dat6 <- replicate(
    arg_list$n_reps,
    do.call(f_G2, arg_list[-1]),
    simplify = FALSE
  )
  dat6 <- do.call(rbind, dat6)
  expect_identical(dat4, dat5)
  expect_identical(dat4, dat6)

  sim4a <- bundle_sim(f_generate = f_G2, f_analyze = identity, f_summarize = identity,
                      reps_name = "n_reps", seed_name = ".s", summarize_opt_name = "evaluate", row_bind_reps = FALSE)
  expect_identical(formals(sim4), formals(sim4a))
  set.seed(20240108)
  dat4a <- do.call(sim4a, arg_list)
  expect_is(dat4a, "list")
  expect_length(dat4a, arg_list$n_reps)
  expect_identical(dat4, do.call(rbind, dat4a))
  dat4b <- do.call(sim4a, c(arg_list, evaluate = FALSE, .s = 20240108))
  expect_is(dat4b, "list")
  expect_length(dat4b, arg_list$n_reps)
  expect_identical(dat4a, dat4b)


  sim4b <- bundle_sim(f_generate = f_G2, f_analyze = identity, f_summarize = identity,
                      reps_name = "n_reps", seed_name = ".s", summarize_opt_name = "evaluate", row_bind_reps = TRUE)
  dat4b <- do.call(sim4b, c(arg_list, .s = 20240108))
  expect_identical(dat4, dat4b)

  sim4c <- bundle_sim(f_generate = f_G2, f_analyze = identity, f_summarize = identity,
                      reps_name = "n_reps", seed_name = ".s", summarize_opt_name = "evaluate", row_bind_reps = FALSE)
  dat4c <- do.call(sim4c, c(arg_list, .s = 20240108))
  expect_identical(dat4a, dat4c)

})


f_A2 <- function(x, trim) data.frame(y_bar = mean(x, trim = trim))

f_S2 <- function(x, calc_sd) {
  if (calc_sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

f_S3 <- function(x, sd = FALSE) {
  if (sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

test_that("bundle_sim throws errors as expected.", {

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A2),
    "All arguments of `f_analyze` except for the first must have default values."
  )
  expect_error(
    bundle_sim(f_generate = f_G2, f_analyze = f_A2),
    "All arguments of `f_analyze` except for the first must have default values."
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S2),
    "All arguments of `f_summarize` except for the first must have default values."
  )
  expect_error(
    bundle_sim(f_generate = f_G2, f_analyze = identity, f_summarize = f_S2),
    "All arguments of `f_summarize` except for the first must have default values."
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_S3),
    "Default arguments of `f_analyze` do not match default arguments of `f_generate`."
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_S3, f_summarize = identity),
    "Default arguments of `f_analyze` do not match default arguments of `f_generate`."
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = identity, f_summarize = f_S3),
    "Default arguments of `f_summarize` do not match default arguments of `f_generate`."
  )

  f_sim <- bundle_sim(f_generate = f_G2, f_analyze = identity, f_summarize = f_S3)
  expect_is(f_sim, "function")

  sim10 <- bundle_sim(f_generate = f_G2, f_analyze = f_S3, reps_name = "n_reps")
  dat10 <- sim10(
    n_reps = 14L,
    N = rpois(1, 3) + 8L,
    p = 7,
    df = sample(4:10, 1),
    rho = 0.3,
    Rsq = 0.8,
    sd = TRUE
  )
  expect_identical(dim(dat10), c(14L * 8L, 2L))

  f_S4 <- f_S3
  formals(f_S4)$sd <- TRUE
  expect_error(
    bundle_sim(f_generate = f_G2, f_analyze = f_S3, f_summarize = f_S4),
    "Default arguments of `f_summarize` do not match default arguments of `f_analyze`."
  )

  # reps_name

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, reps_name = "mean"),
    "cannot be used as an argument name in `f_generate` or `f_analyze`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, reps_name = "trim"),
    "cannot be used as an argument name in `f_generate` or `f_analyze`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, reps_name = "sd"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, reps_name = "trim"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  # seed_name

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, seed_name = "mean"),
    "cannot be used as an argument name in `f_generate` or `f_analyze`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, seed_name = "trim"),
    "cannot be used as an argument name in `f_generate` or `f_analyze`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, seed_name = "sd"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, seed_name = "trim"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  # summarize_opt_name

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, summarize_opt_name = "mean"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, summarize_opt_name = "sd"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, summarize_opt_name = "trim"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

  expect_error(
    bundle_sim(f_generate = f_G1, f_analyze = f_A1, f_summarize = f_S1, summarize_opt_name = "calc_sd"),
    "cannot be used as an argument name in `f_generate`, `f_analyze`, or `f_summarize`"
  )

})


f_G3 <- function(N, p, df, rho, Rsq = NULL) {
  X <- matrix(rnorm(N*p), N, p)
  dat <- as.data.frame(X)
  y_var <- if (!is.null(Rsq)) var(rowSums(X)) * (1 - Rsq) / Rsq else 1L
  dat$Y <- rowSums(X) + sqrt(y_var) * rt(N, df = df)
  dat
}

f_A3 <- function(x, trim = 0, show = NULL) if (!is.null(show)) data.frame(y_bar = mean(x, trim = trim)) else "peekaboo!"

f_A4 <- f_A3
formals(f_A4)$show <- NA

f_S4 <- function(x, calc_sd = FALSE, f = NULL) {
  if (calc_sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  if (!is.null(f)) {
    res$f <- apply(x, 2, f)
  }
  res
}

test_that("bundle_sim() works with functions that have defaults of NULL.", {

  sim11 <- bundle_sim(f_generate = f_G1, f_analyze = f_A3, seed_name = NULL)
  args_A <- formals(sim11)
  args_B <- c(alist(reps = ), formals(f_G1), formals(f_A3)[-1])
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  sim12 <- bundle_sim(f_generate = f_G1, f_analyze = f_A4, seed_name = NULL)
  args_A <- formals(sim12)
  args_B <- c(alist(reps = ), formals(f_G1), formals(f_A4)[-1])
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  sim13 <- bundle_sim(f_generate = f_G3, f_analyze = f_A3, seed_name = NULL)
  args_A <- formals(sim13)
  args_B <- c(alist(reps = ), formals(f_G3), formals(f_A3)[-1])
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  sim14 <- bundle_sim(f_generate = f_G3, f_analyze = f_A4, seed_name = NULL)
  args_A <- formals(sim14)
  args_B <- c(alist(reps = ), formals(f_G3), formals(f_A4)[-1])
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G1, f_analyze = f_A3, f_summarize = f_S4) |> formals()
  args_B <- c(alist(reps = ), formals(f_G1), formals(f_A3)[-1], formals(f_S4)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G1, f_analyze = f_A3, f_summarize = f_S1) |> formals()
  args_B <- c(alist(reps = ), formals(f_G1), formals(f_A3)[-1], formals(f_S1)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G3, f_analyze = f_A3, f_summarize = f_S4) |> formals()
  args_B <- c(alist(reps = ), formals(f_G3), formals(f_A3)[-1], formals(f_S4)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G3, f_analyze = f_A3, f_summarize = f_S1) |> formals()
  args_B <- c(alist(reps = ), formals(f_G3), formals(f_A3)[-1], formals(f_S1)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G1, f_analyze = f_A4, f_summarize = f_S4) |> formals()
  args_B <- c(alist(reps = ), formals(f_G1), formals(f_A4)[-1], formals(f_S4)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G1, f_analyze = f_A4, f_summarize = f_S1) |> formals()
  args_B <- c(alist(reps = ), formals(f_G1), formals(f_A4)[-1], formals(f_S1)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G3, f_analyze = f_A4, f_summarize = f_S4) |> formals()
  args_B <- c(alist(reps = ), formals(f_G3), formals(f_A4)[-1], formals(f_S4)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

  args_A <- bundle_sim(f_generate = f_G3, f_analyze = f_A4, f_summarize = f_S1) |> formals()
  args_B <- c(alist(reps = ), formals(f_G3), formals(f_A4)[-1], formals(f_S1)[-1], alist(seed = NA_integer_, summarize = TRUE))
  expect_identical(as.list(args_A), args_B)
  setdiff(names(args_B), names(args_A))

})
