library(tidyverse)
library(simhelpers)
# ...

#-------------------------------------------------------------------------------
# Source the functions

# source("1_data_gen.R")
# source("2_estimation.R")
# source("3_performance_criteria.R")
# source("4_sim_driver.R")


#-------------------------------------
# Experimental Design
#-------------------------------------
set.seed(20150316) # change this seed value!

# now express the simulation parameters as vectors/lists

design_factors <- list(factor1 = , factor2 = , ...) # combine into a design set

params <-
  expand_grid(!!!design_factors) %>%
  mutate(
    iterations = 1000, # change this to how many ever iterations
    seed = round(runif(1) * 2^30) + 1:n()
  )

# All look right?
lengths(design_factors)
nrow(params)
head(params)



#--------------------------------------------------------
# run simulations in serial - purrr workflow
#--------------------------------------------------------
library(purrr)

system.time(
  results <-
    params %>%
    mutate(res = pmap(., .f = run_sim)) %>%
    unnest(cols = res)
)


#--------------------------------------------------------
# run simulations in parallel - future + furrr workflow
#--------------------------------------------------------

library(future)
library(furrr)

plan(multisession) # choose an appropriate plan from the future package
evaluate_by_row(params, run_sim)

# OR
plan(multisession)
system.time(
  results <-
    params %>%
    mutate(res = future_pmap(., .f = run_sim)) %>%
    unnest(cols = res)
)

#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------

session_info <- sessionInfo()
run_date <- date()

save(params, results, session_info, run_date, file = "simulation_results.Rdata")
