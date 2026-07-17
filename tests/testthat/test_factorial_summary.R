data("Chen_Pusto")

dat <- subset(Chen_Pusto, method == "3PSM")
dat_imbalanced <- subset(Chen_Pusto, method == "WILS")
dat_imbalanced$k <- sample(dat_imbalanced$k, replace = FALSE)

res <- factorial_summary(dat, "rmse", c("k","mu","tau","cor_mu","wts"))

test_that("error messages trigger as expected.", {
  # data validation
  expect_error(
    factorial_summary(as.matrix(dat), "bias",c("k","mu","tau","cor_mu","wts")),
    regexp = "`data` must be a data.frame."
  )

  # y validation
  expect_error(
    factorial_summary(dat, dat$bias, c("k","mu","tau","cor_mu","wts")),
    regexp = "`y` must be a character vector of length 1."
  )

  # factors validation
  expect_error(
    factorial_summary(dat, "bias", "k"),
    regexp = "`factors` must be a character vector listing two or more factors"
  )
  expect_error(
    factorial_summary(dat, "bias", factors = 3:4),
    regexp = "`factors` must be a character vector listing two or more factors"
  )
  expect_error(
    factorial_summary(dat, "bias", c("J","mu","tau","cor_mu","wts")),
    regexp = "The following variables are not in `data`"
  )

})

test_that("sum_orders works.", {
  expect_error(
    factorial_summary(dat, "bias", c("k","mu","tau","cor_mu","wts"), sum_orders = "yes"),
    regexp = "`sum_orders` must be a logical value."
  )

  a <- factorial_summary(dat, "rmse", c("k","mu","tau","cor_mu","wts"), sum_orders = FALSE)
  expect_identical(rowSums(a[,-(1:2)]), res$Sum)

  p <- factorial_summary(dat, "rmse", c("k","cor_mu","wts"), sum_orders = FALSE)
  q <- factorial_summary(dat, "rmse", c("k","cor_mu","wts"), sum_orders = TRUE)
  expect_identical(rowSums(p[,-(1:2)]), q$Sum)


})


test_that("include_total works.", {

  expect_error(
    factorial_summary(dat, "bias", c("k","mu","tau","cor_mu","wts"), include_total = 1L),
    regexp = "`include_total` must be a logical value."
  )

  a <- factorial_summary(dat, "rmse", c("k","mu","tau","cor_mu","wts"), include_total = FALSE, sum_orders = FALSE)

  expect_equal(a, res[-6, -8])
  expect_equal(
    as.numeric(colSums(a[,-(1:2)]) / 1:5),
    as.numeric(res[res$factor == "Total",3:7])
  )

  p <- factorial_summary(dat, "rmse", c("k","mu","cor_mu","wts"), include_total = FALSE, sum_orders = FALSE)
  q <- factorial_summary(dat, "rmse", c("k","mu","cor_mu","wts"), include_total = TRUE, sum_orders = FALSE)
  expect_identical(
    colSums(p[,-(1:2)]) / 1:4,
    unlist(q[q$factor=="Total",-(1:2)])
  )


})

test_that("check_balance works.", {

  expect_error(factorial_summary(dat_imbalanced, "bias", c("k","mu","tau","cor_mu","wts")))

  expect_s3_class(
    factorial_summary(dat, "bias", c("k","mu","tau","cor_mu","wts"), check_balance = FALSE),
    "data.frame"
  )

  expect_s3_class(
    factorial_summary(dat_imbalanced, "bias", c("k","mu","tau","cor_mu","wts"), check_balance = FALSE),
    "data.frame"
  )
})

