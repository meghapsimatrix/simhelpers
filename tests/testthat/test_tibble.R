
test_that("functions return a tibble", {

  abs_res <- calc_absolute(data = t_res, estimates = est, true_param = true_param)
    
  cov_res <- calc_coverage(data = t_res, lower_bound = lower_bound,
                upper_bound = upper_bound, true_param = true_param)
    
  rej_res <- calc_rejection(data = t_res, p_values = p_val)
    
  rel_var_res <- calc_relative_var(data = alpha_res, estimates = A, var_estimates = Var_A)
    
  rel_res <- calc_relative(data = t_res, estimates = est, true_param = true_param)

  expect_s3_class(abs_res, "tbl")
  expect_s3_class(cov_res, "tbl")
  expect_s3_class(rej_res, "tbl")
  expect_s3_class(rel_var_res, "tbl")
  expect_s3_class(rel_res, "tbl")
  


})


