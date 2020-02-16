## code to prepare `results` dataset goes here
library(tidyverse)
library(broom)


# Data generation ---------------------------------------------------------

# function to create normally distributed data for each group to run t test

generate_dat <- function(n, mean_diff){

  dat <- tibble(group_1 = rnorm(n = n, mean_diff, 1),
                group_2 = rnorm(n = n, 0, 2))

  return(dat)

}



# Estimation Procedures ---------------------------------------------------

# function to calculate t-test, pextracts estimate of the mean difference, p val and ci

estimate <- function(dat){

  # calculate summary stats
  est <- mean(dat$group_1) - mean(dat$group_2)
  n1 <- n2 <- nrow(dat)
  var_1 <- var(dat$group_1)
  var_2 <- var(dat$group_2)

  # normal t-test
  df <- n1 + n2 - 2
  sp_sq <- ((n1-1) * var_1 + (n2 - 1) * var_2) / df
  vd <- sp_sq * (1 / n1 + 1 / n2)



  # welch t-test
  dfw <- (var_1 / n1 + var_2 / n2)^2 / (((1 / (n1 - 1)) * (var_1 / n1)^2) + ((1 / (n2 - 1)) * (var_2 / n2)^2))
  vdw <- var_1 / n1 + var_2 / n2


  #t and pvalue
  calc_t <- function(est, vd, df, method){

    se <- sqrt(vd)
    t <- est / se
    p_val <-  2 * pt(-abs(t), df = df)
    ci <- est + c(-1, 1) * qt(.975, df = df) * se


    return(tibble(method = method, est = est, p_val = p_val, lower_bound = ci[1], upper_bound = ci[2]))
  }

  results <- bind_rows(calc_t(est = est, vd = vd, df = df, method = "t-test"),
                   calc_t(est = est, vd = vdw, df = dfw, method = "Welch t-test"))


  return(results)

}


# Simulation Driver -------------------------------------------------------

run_sim <- function(iterations, n, mean_diff, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  results <-
    rerun(iterations, {
      dat <- generate_dat(n, mean_diff)
      estimate(dat)
    }) %>%
    bind_rows()

}


# Experimental Design -----------------------------------------------------

# generating 1000 iterations

set.seed(20200110)

# now express the simulation parameters as vectors/lists

design_factors <- list(
  n = c(50, 100, 200),
  mean_diff = c(0, .5, 1, 2)
)

params <-
  cross_df(design_factors) %>%
  mutate(
    iterations = 1000,
    seed = round(runif(1) * 2^30) + 1:n()
  )




# Running Sim -------------------------------------------------------------

system.time(
  results <-
    params %>%
    mutate(
      res = pmap(., .f = run_sim)
    ) %>%
    unnest(cols = c(res))
)

welch_res <- results

usethis::use_data(welch_res, overwrite = TRUE)
