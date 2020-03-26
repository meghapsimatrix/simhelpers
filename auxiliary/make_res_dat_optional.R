library(tidyverse)


calc_rejection <- function(res_dat, p_values, alpha = .05){

  env <- list2env(res_dat, parent = parent.frame())
  p_vals <- eval(res_dat %>% dplyr::pull({{p_values}}), env) # p values
  K <- eval(nrow(res_dat), env) # number of iterations

  dat <- tibble::tibble(rej_rate = mean(p_vals < alpha),
                        rej_rate_mcse = sqrt((rej_rate * (1 - rej_rate)) / K))

  return(dat)

}


# Try to run --------------------------------------------------------------

load("data/t_res.rda")
load("data/welch_res.rda")

t_res %>%
  calc_rejection(p_values = p_val)

welch_res %>%
  calc_rejection(p_values = p_val)

welch_res %>%
  group_by(method) %>%
  calc_rejection(p_values = p_val)


welch_res %>%
  group_by(method) %>%
  mutate(calc_rejection(p_values = p_val))
