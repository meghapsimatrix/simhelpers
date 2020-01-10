## code to prepare `results` dataset goes here
library(tidyverse)
library(broom)


# Data generation ---------------------------------------------------------

# function to create normally distributed data for each group to run t test

generate_dat <- function(n = 50, mean_diff){

  dat <- tibble(group_1 = rnorm(n, 0, 1),
                group_2 = rnorm(n, mean_diff, 2))

  return(dat)

}



# Estimation Procedures ---------------------------------------------------

# function to calculate t-test, pulls out estimate of the mean difference, p val and ci

estimate_t <- function(sim_dat, var_equal = FALSE){

  res <- tidy(t.test(sim_dat$group_2, sim_dat$group_1, var.equal = var_equal)) %>%
    mutate(est = estimate1 - estimate2) %>%
    dplyr::select(method, est, p_val = p.value, lower_bound = conf.low, upper_bound = conf.high)

  return(res)

}



# Simulation Driver -------------------------------------------------------

run_sim <- function(iterations, mean_diff, var_equal, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  results <-
    rerun(iterations, {
      dat <- generate_dat(mean_diff = mean_diff)
      estimate_t(sim_dat = dat, var_equal = var_equal)
    }) %>%
    bind_rows()

  return(results)
}



# Experimental Design -----------------------------------------------------

# generating 1000 iterations

set.seed(20200110)

# now express the simulation parameters as vectors/lists

design_factors <- list(
  mean_diff = c(0, .5, 1, 2),
  var_equal = c(TRUE, FALSE)
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
