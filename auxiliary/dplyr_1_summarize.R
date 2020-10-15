library(simhelpers)
library(dplyr)

welch_res %>%
  group_by(n1, n2, mean_diff, method) %>%
  summarize(calc_rejection(., p_val))


welch_res %>%
  group_by(n1, n2, mean_diff, method) %>%
  do(calc_rejection(., p_val))
