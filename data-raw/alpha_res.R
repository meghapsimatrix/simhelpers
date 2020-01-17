## code to prepare `DATASET` dataset goes here


# Generate data -----------------------------------------------------------

library(mvtnorm)
library(dplyr)

r_mvt_items <- function(n, p, icc, df) {
  V_mat <- icc + diag(1 - icc, nrow = p)
  X <- rmvt(n = n, sigma = V_mat, df = df)
  colnames(X) <- LETTERS[1:p]
  X
}


# Estimate Alpha ----------------------------------------------------------

estimate_alpha <- function(dat, alpha) {
    V <- cov(dat)
    p <- ncol(dat)
    n <- nrow(dat)
    A <- p / (p - 1) * (1 - sum(diag(V)) / sum(V))
    Var_A <- 2 * p * (1 - A)^2 / ((p - 1) * n)
    data.frame(A, Var_A, true_param = alpha)
}


# Replicates --------------------------------------------------------------

simulate_alpha <- function(reps, n, p, alpha, df) {
  icc <- alpha / (p - alpha * (p - 1))
  replicate(reps, {
    dat <- r_mvt_items(n = n, p = p, icc = icc, df = df)
    estimate_alpha(dat, alpha)
  }, simplify = FALSE) %>%
    bind_rows()
}

reps <- 1000
alpha_true <- 0.8
alpha_res <- simulate_alpha(reps = reps, n = 40, p = 6, alpha = alpha_true, df = 5)


usethis::use_data(alpha_res, overwrite = TRUE)
