library(tidyverse)


calc_rejection <- function(res_dat, p_values, alpha = .05){

  if(missing(res_dat)){

    p_vals <- p_values
    K <- length(p_values)

  } else{

    p_vals <- res_dat %>% dplyr::pull({{p_values}})
    K <- nrow(res_dat)

  }

  #dat1 <- tibble::tibble(rej_rate = mean(p_vals < alpha))
  rej_rate <- mean(p_vals < alpha)
  rej_rate_mcse <- sqrt((rej_rate * (1 - rej_rate)) / K)


  dat <- nest(data.frame(rej_rate =rej_rate,
              rej_rate_mcse = rej_rate_mcse))

  return(dat)

}


# Try to run --------------------------------------------------------------

load("data/t_res.rda")
load("data/welch_res.rda")

res <- t_res %>%
  calc_rejection(t_res, p_values = p_val)

unnest(res)

# runs but doesn't group
welch_res %>%
  group_by(method) %>%
  calc_rejection(p_values = p_val)


# the output is weird
welch_res %>%
  group_by(method) %>%
  summarize(rmse = calc_rejection(p_values = p_val))

welch_res %>%
  group_by(method) %>%
  do(calc_rejection(., p_values = p_val))
