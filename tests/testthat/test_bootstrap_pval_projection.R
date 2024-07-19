skip_if_not_installed("dplyr")

# generate data from two distinct populations
dat <- data.frame(
  group = rep(c("A","B"), c(40, 50)),
  y = c(
    rgamma(40, shape = 7, scale = 2),
    rgamma(50, shape = 3, scale = 4)
  )
)
stat <- t.test(y ~ group, data = dat)$statistic

# create bootstrap replications under the null of no difference
boot_dat <- dat
booties <- replicate(1999, {
  boot_dat$group <- sample(dat$group)
  t.test(y ~ group, data = boot_dat)$statistic
})

alts <- c("greater","less","two-sided")

test_that("bootstrap_pvals returns results of expected length.", {

  A_long <- c(
    bootstrap_pvals(
      boot_stat = booties,
      stat = stat
    ),
    sapply(alts, \(x) bootstrap_pvals(
      boot_stat = booties,
      stat = stat,
      alternative = x
    ))
  )

  expect_equal(A_long[[1]], A_long[["two-sided"]])
  expect_equal(
    A_long[["greater"]],
    1 - A_long[["less"]]
  )

  B_long <- sapply(alts, \(x) bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = x,
    B_vals = 499,
    seed = 20240719
  ))
  set.seed(20240719)
  expect_equal(
    B_long[["two-sided"]],
    mean(abs(sample(booties, size = 499)) > abs(stat))
  )
  expect_equal(
    B_long[["greater"]],
    1 - B_long[["less"]]
  )

  E_long <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = "greater",
    B_vals = c(49,59,89)
  )

  expect_true(is.list(E_long))
  expect_identical(length(E_long), 3L)
  expect_identical(length(unlist(E_long)), 3L)

  F_long <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = "two-sided",
    B_vals = c(49,59,89),
    reps = 7L
  )

  expect_true(is.list(F_long))
  expect_identical(length(F_long), 3L)
  expect_true(all(sapply(F_long, is.vector)))
  expect_identical(lengths(F_long), rep(7L, 3L))

  G_long <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = "two-sided",
    B_vals = c(49,59,89),
    reps = 7L,
    enlist = TRUE
  )

  expect_true(is.list(G_long))
  expect_identical(length(G_long), 1L)
  expect_true(all(sapply(G_long, is.list)))
  expect_identical(lengths(G_long), 3L)
  expect_identical(lengths(G_long[[1]]), rep(7L, 3L))

})

