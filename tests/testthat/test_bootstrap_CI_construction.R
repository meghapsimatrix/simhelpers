skip_if_not_installed("dplyr")
skip_if_not_installed("tibble")
skip_if_not_installed("boot")

library(boot)

format_boot_CIs <- function(boot_CIs) {
  CI_dat <- rbind(
    normal = boot_CIs$normal[2:3],
    basic = boot_CIs$basic[4:5],
    student = boot_CIs$student[4:5],
    percentile = boot_CIs$percent[4:5]
  ) |>
    as.data.frame()
  names(CI_dat) <- c("lower","upper")

  tibble::rownames_to_column(CI_dat, var = "type")
}


f <- \(x,i) {
  c(
    M = mean(x[i], trim = 0.1),
    SE = sd(x[i]) / sqrt(length(x[i]))
  )
}

# generate t-distributed data

set.seed(20240629)
N <- 50
mu <- 2
nu <- 5
dat <- mu + rt(N, df = nu)


test_that("bootstrap_CIs() agrees with boot::boot.ci using fake data.", {

  # basic pairs bootstrap
  B <- 399L
  booties <- boot(dat, f, R = B)

  # benchmark bootstrap CIs
  norm_boot_CIs <- boot.ci(
    booties,
    type = c("norm"),
  ) |>
    format_boot_CIs()

  more_boot_CIs <- boot.ci(
    booties,
    type = c("basic", "stud", "perc"),
    var.t0 = booties$t0[2]^2,
    var.t = booties$t[,2]^2
  ) |>
    format_boot_CIs()

  # package bootstrap CIs
  my_CIs <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    format = "long"
  )

  dplyr::bind_rows(norm_boot_CIs, more_boot_CIs) |>
    expect_equal(my_CIs[,-1])

  # now do same comparison, but with sub-sampling
  B_val <- 199L
  reps <- 10L
  set.seed(20240717)
  boot_CI_reps <- replicate(reps, {
    i <- sample(1:B, size = B_val)
    mini_booties <- booties
    mini_booties$t <- booties$t[i,]
    mini_booties$R <- B_val
    boot.ci(
      mini_booties,
      type = c("basic", "stud", "perc"),
      var.t0 = mini_booties$t0[2]^2,
      var.t = mini_booties$t[,2]^2
    ) |>
      format_boot_CIs()
  }, simplify = FALSE)

  set.seed(20240717)
  my_CI_reps <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("basic","student","percentile"),
    B_vals = B_val,
    reps = reps,
    format = "long"
  )

  expect_equal(
    dplyr::bind_rows(boot_CI_reps),
    my_CI_reps[,-1]
  )
})


test_that("bootstrap_CIs() agrees with boot::boot.ci using real data.", {

  data(city)
  ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)

  # basic pairs bootstrap
  B <- 999L
  booties <- boot(city, ratio, R = 999, stype = "w")

  # benchmark bootstrap CIs
  boot_CIs <- boot.ci(
    booties,
    type = c("norm","basic", "perc"),
  ) |>
    format_boot_CIs()

  # package bootstrap CIs
  my_CIs <- bootstrap_CIs(
    boot_est = booties$t[,1],
    est = booties$t0[1],
    CI_type = c("normal","basic","percentile"),
    format = "long"
  )

  expect_equal(boot_CIs, my_CIs[,-1])

  # now do same comparison, but with sub-sampling
  B_val <- 199L
  reps <- 12L
  set.seed(20240717)
  boot_CI_reps <- replicate(reps, {
    i <- sample(1:B, size = B_val)
    mini_booties <- booties
    mini_booties$t <- booties$t[i,,drop=FALSE]
    mini_booties$R <- B_val
    boot.ci(
      mini_booties,
      type = c("norm", "basic", "perc"),
    ) |>
      format_boot_CIs()
  }, simplify = FALSE)

  set.seed(20240717)
  my_CI_reps <- bootstrap_CIs(
    boot_est = booties$t[,1],
    est = booties$t0[1],
    CI_type = c("norm","basic","percentile"),
    B_vals = B_val,
    reps = reps,
    format = "long"
  )

  expect_equal(
    dplyr::bind_rows(boot_CI_reps),
    my_CI_reps[,-1]
  )
})

test_that("bootstrap_CIs returns results of expected length.", {

  booties <- boot(dat, f, R = 199)

  A_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    format = "wide"
  )

  expect_s3_class(A_wide, "data.frame")
  expect_identical(ncol(A_wide), 9L)
  expect_identical(nrow(A_wide), 1L)

  A_long <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    format = "long"
  )

  expect_s3_class(A_long, "data.frame")
  expect_identical(ncol(A_long), 4L)
  expect_identical(nrow(A_long), 4L)


  B_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("basic","student"),
    B_vals = 19
  )

  expect_s3_class(B_wide, "data.frame")
  expect_identical(ncol(B_wide), 5L)
  expect_identical(nrow(B_wide), 1L)

  B_long <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("basic","student"),
    B_vals = 19,
    format = "long"
  )

  expect_s3_class(B_long, "data.frame")
  expect_identical(ncol(B_long), 4L)
  expect_identical(nrow(B_long), 2L)

  C_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    reps = 11L,
    format = "wide"
  )

  expect_identical(A_wide, C_wide)

  C_long <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    reps = 13L,
    format = "long"
  )

  expect_equal(A_long, C_long)

  D_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    B_vals = 59,
    reps = 11L,
    format = "wide"
  )

  expect_s3_class(D_wide, "data.frame")
  expect_identical(nrow(D_wide), 11L)
  expect_identical(ncol(D_wide), 9L)

  D_long <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student","percentile"),
    B_vals = 59,
    reps = 13L,
    format = "long"
  )

  expect_s3_class(D_long, "data.frame")
  expect_identical(nrow(D_long), 13L * 4L)
  expect_identical(ncol(D_long), 4L)


  E_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student"),
    B_vals = c(49,59,89,99),
    reps = 1L,
    format = "wide"
  )

  expect_s3_class(E_wide, "data.frame")
  expect_identical(nrow(E_wide), 4L)
  expect_identical(ncol(E_wide), 7L)

  E_long <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("basic","student","percentile"),
    B_vals = c(49,59,89),
    format = "long"
  )

  expect_s3_class(E_long, "data.frame")
  expect_identical(nrow(E_long), 3L * 3L)
  expect_identical(ncol(E_long), 4L)

  F_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student"),
    B_vals = c(49,59,89,99),
    reps = 5L,
    format = "wide"
  )

  expect_s3_class(F_wide, "data.frame")
  expect_identical(nrow(F_wide), 4L * 5L)
  expect_identical(ncol(F_wide), 7L)

  F_long <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("basic","student","percentile"),
    B_vals = c(49,59,89),
    reps = 7L,
    format = "long"
  )

  expect_s3_class(F_long, "data.frame")
  expect_identical(nrow(F_long), 3L * 3L * 7L)
  expect_identical(ncol(F_long), 4L)


  G_wide <- bootstrap_CIs(
    boot_est = booties$t[,1],
    boot_se = booties$t[,2],
    est = booties$t0[1],
    se = booties$t0[2],
    CI_type = c("normal","basic","student"),
    B_vals = c(49,59,89,99),
    reps = 5L,
    format = "wide-list"
  )
  expect_true(is.list(G_wide))
  expect_identical(length(G_wide), 1L)
  expect_s3_class(G_wide[[1]], "data.frame")
  expect_identical(nrow(G_wide[[1]]), 4L * 5L)
  expect_identical(ncol(G_wide[[1]]), 7L)

})
