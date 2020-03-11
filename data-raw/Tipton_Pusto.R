## code to prepare `DATASET` dataset goes here
library(tidyverse)

tp_dat <- read_csv("https://raw.githubusercontent.com/meghapsimatrix/datasets/master/sim_data/Simulation_results.csv") %>%
  rename(num_studies = 1) %>%
  mutate(q = str_sub(contrast, 3, 3))

Tipton_Pusto <- tp_dat %>%
  filter(num_studies %in% c(10, 20, 40, 80), test != "Naive F") %>%
  mutate(test = factor(test, levels = c("EDF", "EDT", "T^2 A", "T^2 B","T^2 Z"),
                       labels = c("EDF", "EDT", "AHA", "AHB", "AHZ"))) %>%
  select(num_studies, r, Isq, contrast, test, q, rej_rate = p05) %>%
  mutate(mcse = sqrt((rej_rate * (1 - rej_rate)) / 10000))

usethis::use_data(Tipton_Pusto, overwrite = TRUE)
