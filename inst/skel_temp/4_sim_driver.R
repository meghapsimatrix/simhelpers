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
