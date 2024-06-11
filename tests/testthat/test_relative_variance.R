K <- 10000
ncp <- 2
df <- 3
mu <- ncp * sqrt(df/ 2) * gamma((df - 1) / 2) / gamma(df / 2)
sigma_sq <- (1 + ncp^2) * df / (df - 2) - mu^2

dat <- data.frame(
  x = rt(K, df = df, ncp = ncp),
  SE_sq = rchisq(K, df = sigma_sq)
)

test_that("Relative variance calculations work in simple cases.", {
  x_res <- calc_absolute(dat, x, true_param = mu, criteria = c("bias","var"))
  v_res <- calc_absolute(dat, SE_sq, true_param = x_res$var, criteria = c("bias","var","mse","rmse"))
  rel_res <- calc_relative(dat, SE_sq, true_param = x_res$var)
  rv_res <- calc_relative_var(dat, estimates = x, var_estimates = SE_sq)

  expect_equal(rv_res$rel_bias_var, 1 + v_res$bias / x_res$var)
  expect_equal(rv_res$rel_mse_var, (v_res$bias^2 + v_res$var) / x_res$var^2)
  expect_equal(rv_res$rel_rmse_var, sqrt(v_res$bias^2 + v_res$var) / x_res$var)

  expect_equal(rv_res$rel_bias_var, rel_res$rel_bias)
  expect_true(rv_res$rel_bias_var_mcse != rel_res$rel_bias_mcse)
  expect_equal(rv_res$rel_mse_var, rel_res$rel_mse, tolerance = 5e-5)
  expect_true(rv_res$rel_mse_var_mcse != rel_res$rel_mse_mcse)
  expect_equal(rv_res$rel_rmse_var, rel_res$rel_rmse, tolerance = 5e-5)
  expect_true(rv_res$rel_rmse_var_mcse != rel_res$rel_rmse_mcse)

  expect_equal(rel_res$rel_mse, v_res$mse / x_res$var^2)
  expect_equal(rel_res$rel_mse_mcse, v_res$mse_mcse / x_res$var^2)
  expect_equal(rel_res$rel_rmse, v_res$rmse / x_res$var)
  expect_equal(rel_res$rel_rmse_mcse, v_res$rmse_mcse / x_res$var)

})


test_that("Relative variance calculations work with winsorization.", {
  winz <- 1.5
  var_winz <- 1
  x_res <- calc_absolute(dat, x, true_param = mu, criteria = c("bias","var"), winz = winz)
  v_res <- calc_absolute(dat, SE_sq, true_param = x_res$var, criteria = c("bias","var","mse","rmse"), winz = var_winz)
  rel_res <- calc_relative(dat, SE_sq, true_param = x_res$var, winz = var_winz)
  rv_res <- calc_relative_var(dat, estimates = x, var_estimates = SE_sq, winz = winz, var_winz = var_winz)

  expect_equal(rv_res$rel_bias_var, 1 + v_res$bias / x_res$var)
  expect_equal(rv_res$rel_mse_var, (v_res$bias^2 + v_res$var) / x_res$var^2)
  expect_equal(rv_res$rel_rmse_var, sqrt(v_res$bias^2 + v_res$var) / x_res$var)

  expect_equal(rv_res$rel_bias_var, rel_res$rel_bias)
  expect_true(rv_res$rel_bias_var_mcse != rel_res$rel_bias_mcse)
  expect_equal(rv_res$rel_mse_var, rel_res$rel_mse, tolerance = 5e-5)
  expect_true(rv_res$rel_mse_var_mcse != rel_res$rel_mse_mcse)
  expect_equal(rv_res$rel_rmse_var, rel_res$rel_rmse, tolerance = 5e-5)
  expect_true(rv_res$rel_rmse_var_mcse != rel_res$rel_rmse_mcse)

  expect_equal(rel_res$rel_mse, v_res$mse / x_res$var^2)
  expect_equal(rel_res$rel_mse_mcse, v_res$mse_mcse / x_res$var^2)
  expect_equal(rel_res$rel_rmse, v_res$rmse / x_res$var)
  expect_equal(rel_res$rel_rmse_mcse, v_res$rmse_mcse / x_res$var)

})
