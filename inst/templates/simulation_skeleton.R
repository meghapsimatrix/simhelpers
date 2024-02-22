#------------------------------------------------------
# Set development values for simulation parameters
#------------------------------------------------------

# What are your model parameters?
# What are your design parameters?


#------------------------------------------------------
# Data Generating Model
#------------------------------------------------------

generate_dat <- function(model_params) {

  return(dat)
}

# Test the data-generating model - How can you verify that it is correct?


#------------------------------------------------------
# Model-fitting/estimation/testing functions
#------------------------------------------------------


estimate <- function(dat, design_params) {

  return(results)
}

# Test the estimation function

#------------------------------------------------------
# Calculate performance measures
# (For some simulations, it may make more sense
# to do this as part of the simulation driver.)
#------------------------------------------------------

calc_performance <- function(results, model_params) {

  return(performance_measures)
}

# Check performance calculations

#-----------------------------------------------------------
# Simulation Driver - should return a data.frame or tibble
#-----------------------------------------------------------

run_sim <- function(iterations, model_params, design_params, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  results <-
    map_dfr(1:iterations, ~ {
      dat <- generate_dat(model_params)
      estimate(dat, design_params)
    })

  calc_performance(results, model_params)
}

# demonstrate the simulation driver

#-------------------------------------
# Experimental Design
#-------------------------------------
library(tidyverse)
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
