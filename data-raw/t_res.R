## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(broom)
library(kableExtra)

set.seed(20191228)

# function to create normally distributed data for each group to run t test

generate_dat <- function(n = 50, effect_x){

  dat <- tibble(group_1 = rnorm(n, 0, 1),
                group_2 = rnorm(n, effect_x, 1))

  return(dat)

}


# function to calculate t-test, pulls out estimate of the mean difference, p val and ci

estimate_t <- function(sim_dat, effect_x){

  res <- tidy(t.test(sim_dat$group_2, sim_dat$group_1)) %>%
    select(est = estimate, p_val = p.value, lower_bound = conf.low, upper_bound = conf.high) %>%
    mutate(true_param = effect_x)

  return(res)

}


# generating 1000 iterations

t_res <-
  rerun(1000, {
    dat <- generate_dat(effect_x = .5)
    estimate_t(dat, effect_x = .5)
  }) %>%
  bind_rows()

usethis::use_data(t_res, overwrite = TRUE)
