## code to prepare `results` dataset goes here
library(tidyverse)
library(broom)

# function to create normally distributed data for each group to run t test

generate_dat <- function(n = 50, effect_x){

  dat <- tibble(group_1 = rnorm(n, 0, 1),
                group_2 = rnorm(n, effect_x, 2))

  return(dat)

}


# function to calculate t-test, pulls out estimate of the mean difference, p val and ci

estimate_t <- function(sim_dat, var_equal = FALSE){

  res <- tidy(t.test(sim_dat$group_2, sim_dat$group_1, var.equal = var_equal)) %>%
    mutate(est = estimate1 - estimate2) %>%
    dplyr::select(method, est, p_val = p.value, ci_low = conf.low, ci_high = conf.high)

  return(res)

}

run_sim <- function(iterations,effect_x, var_equal, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  results <-
    rerun(iterations, {
      dat <- generate_dat(effect_x = effect_x)
      estimate_t(sim_dat = dat, var_equal = var_equal)
    }) %>%
    bind_rows()

  return(results)
}



# generating 1000 iterations



set.seed(20200110)

# now express the simulation parameters as vectors/lists

design_factors <- list(
  effect_x = c(0, .5, 1, 2),
  var_equal = c(TRUE, FALSE)
)

params <-
  cross_df(design_factors) %>%
  mutate(
    iterations = 1000,
    seed = round(runif(1) * 2^30) + 1:n()
  )


system.time(
  results <-
    params %>%
    mutate(
      res = pmap(., .f = run_sim)
    ) %>%
    unnest(cols = c(res))
)


usethis::use_data(results, overwrite = TRUE)
