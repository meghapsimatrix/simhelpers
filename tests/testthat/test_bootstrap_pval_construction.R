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
names(alts) <- alts

test_that("bootstrap_pvals returns results of expected length.", {

  A_long <- c(
    list(bootstrap_pvals(
      boot_stat = booties,
      stat = stat
    )),
    lapply(alts, \(x) bootstrap_pvals(
      boot_stat = booties,
      stat = stat,
      alternative = x
    ))
  )

  expect_equal(A_long[[1]], A_long[["two-sided"]])
  expect_equal(
    A_long[["greater"]]$pval[[1]],
    1 - A_long[["less"]]$pval[[1]]
  )

  B_long <- lapply(alts, \(x) bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = x,
    B_vals = 499,
    seed = 20240719
  ))
  set.seed(20240719)
  expect_equal(
    B_long[["two-sided"]]$pval[[1]],
    mean(abs(sample(booties, size = 499)) > abs(stat))
  )
  expect_equal(
    B_long[["greater"]]$pval[[1]],
    1 - B_long[["less"]]$pval[[1]]
  )

  E_long <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = "greater",
    B_vals = c(49,59,89)
  )

  expect_true(is.data.frame(E_long))
  expect_identical(nrow(E_long), 3L)

  F_long <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    alternative = "two-sided",
    B_vals = c(49,59,89),
    reps = 7L
  )

  expect_true(is.data.frame(F_long))
  expect_identical(nrow(F_long), 3L)
  expect_true(all(sapply(F_long$pval, is.vector)))
  expect_identical(lengths(F_long$pval), rep(7L, 3L))

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
  expect_true(all(sapply(G_long, is.data.frame)))
  expect_identical(lengths(G_long), 2L)
  expect_identical(lengths(G_long[[1]]$pval), rep(7L, 3L))

})

