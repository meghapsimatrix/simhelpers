library(tidyverse)

load("auxiliary/sim-res.Rdata")

Chen_Pusto <-
  res_one_step %>%
  mutate(
    omega_ratio = 0.5,
    k_stop = if_else(k_stop == "NA", 5L, as.integer(k_stop))
  ) %>%
  filter(
    method %in% c("3PSM","4PSM","CHE-ISCW","MV_EK","MV_PET-PEESE","MV_WILS","MV_WAAP","TF","p-uniform*"),
    k_stop == 5
  ) %>%
  mutate(
    method = str_remove(method, "^MV_")
  ) %>%
  select(
    k, mu, tau, cor_mu, wts, iterations, method,
    n_converged:rej_rate_mcse
  )

Chen_Pusto %>%
  count(method)

Chen_Pusto %>%
  summarize(
    n = n(),
    .by = c(k, mu, tau, cor_mu, wts)
  ) %>%
  count(n)

factor_levels <-
  Chen_Pusto %>%
  summarize(
    across(
      c(k, mu, tau, cor_mu, wts),
      n_distinct
    )
  ) %>%
  unlist()

factor_levels
prod(factor_levels)

usethis::use_data(Chen_Pusto, overwrite = TRUE)
