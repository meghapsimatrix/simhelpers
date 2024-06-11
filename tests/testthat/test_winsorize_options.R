K <- 10000
ncp <- 2
df <- 3
mu <- ncp * sqrt(df/ 2) * gamma((df - 1) / 2) / gamma(df / 2)
sigma_sq <- (1 + ncp^2) * df / (df - 2) - mu^2

dat <- data.frame(
  x = rt(K, df = df, ncp = ncp),
  SE_sq = rchisq(K, df = sigma_sq)
)


winz <- 1.5
qrtls <- quantile(dat$x, c(.25, .75))
IQR <- qrtls[2] - qrtls[1]
dat$x_trim <- pmax(pmin(dat$x, qrtls[2] + winz * IQR), qrtls[1] - winz * IQR)

var_winz <- 0.8
var_qrtls <- quantile(dat$SE_sq, c(.25, .75))
var_IQR <- var_qrtls[2] - var_qrtls[1]
dat$SE_sq_trim <- pmax(pmin(dat$SE_sq, var_qrtls[2] + var_winz * var_IQR), var_qrtls[1] - var_winz * var_IQR)

test_that("winsorization works for absolute performance measures", {

  vbls <- c("K_absolute","bias","bias_mcse","var","var_mcse","stddev","stddev_mcse","mse","mse_mcse","rmse","rmse_mcse")

  res_wins <- calc_absolute(dat, x, true_param = mu, winz = winz)
  res_hand <- calc_absolute(dat, x_trim, true_param = mu)
  expect_equal(
    res_wins[vbls],
    res_hand[vbls]
  )
  expect_equal(res_wins$winsor_pct, mean(dat$x != dat$x_trim))

  res_wins <- calc_absolute(dat, SE_sq, true_param = sigma_sq, winz = var_winz)
  res_hand <- calc_absolute(dat, SE_sq_trim, true_param = sigma_sq)
  expect_equal(
    res_wins[vbls],
    res_hand[vbls]
  )
  expect_equal(res_wins$winsor_pct, mean(dat$SE_sq != dat$SE_sq_trim))

})

test_that("winsorization works for relative performance measures", {
  vbls <- c("bias_mcse","rmse","rmse_mcse")

  res_wins <- calc_relative(dat, x, true_param = mu, winz = winz)
  res_hand <- calc_absolute(dat, x_trim, true_param = mu, criteria = c("bias","rmse"))
  expect_equal(res_wins$K_relative, res_hand$K_absolute)
  expect_equal(res_wins$rel_bias, res_hand$bias / mu + 1)
  expect_equal(res_wins[paste("rel",vbls, sep = "_")], res_hand[vbls]  / mu, check.attributes = FALSE)
  expect_equal(res_wins$winsor_pct, mean(dat$x != dat$x_trim))

  res_wins <- calc_relative(dat, SE_sq, true_param = sigma_sq, winz = var_winz)
  res_hand <- calc_absolute(dat, SE_sq_trim, true_param = sigma_sq, criteria = c("bias","rmse"))
  expect_equal(res_wins$K_relative, res_hand$K_absolute)
  expect_equal(res_wins$rel_bias, res_hand$bias / sigma_sq + 1)
  expect_equal(res_wins[paste("rel",vbls, sep = "_")], res_hand[vbls]  / sigma_sq, check.attributes = FALSE)
  expect_equal(res_wins$winsor_pct, mean(dat$SE_sq != dat$SE_sq_trim))

})


test_that("winsorization works for coverage measures", {

  dat$lo <- runif(nrow(dat), min = 0, max = 3)
  dat$hi <- dat$lo + dat$x
  res_wins <- calc_coverage(dat, lower_bound = lo, upper_bound = hi, true_param = mu, winz = winz)
  res_hand <- calc_coverage(dat, lower_bound = rep(0, nrow(dat)), upper_bound = x_trim, true_param = mu, criteria = c("width"))

  vbls <- c("K_coverage","width","width_mcse")
  expect_equal(res_wins[vbls], res_hand[vbls])
  expect_equal(res_wins$width_winsor_pct, mean(dat$x != dat$x_trim))
})

test_that("winsorization works for relative variance measures", {

  vbls <- c("K_relvar","rel_bias_var","rel_bias_var_mcse","rel_mse_var","rel_mse_var_mcse", "rel_rmse_var", "rel_rmse_var_mcse")
  res_wins <- calc_relative_var(dat, estimates = x, var_estimates = SE_sq, winz = winz, var_winz = var_winz)
  res_hand <- calc_relative_var(dat, estimates = x_trim, var_estimates = SE_sq_trim)

  expect_equal(res_wins[vbls], res_hand[vbls])
  expect_equal(res_wins$est_winsor_pct, mean(dat$x != dat$x_trim))
  expect_equal(res_wins$var_winsor_pct, mean(dat$SE_sq != dat$SE_sq_trim))

})
