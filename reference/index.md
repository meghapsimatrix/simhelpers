# Package index

## Performance Criteria and MCSE

Functions for calculating performance critieria and MCSE

- [`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md)
  : Calculate absolute performance criteria and MCSE
- [`calc_relative()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative.md)
  : Calculate relative performance criteria and MCSE
- [`calc_relative_var()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative_var.md)
  : Calculate jack-knife Monte Carlo SE for variance estimators
- [`calc_rejection()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_rejection.md)
  : Calculate rejection rate and MCSE
- [`calc_coverage()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_coverage.md)
  : Calculate confidence interval coverage, width and MCSE

## Simulating bootstrap processes

Specialized functions for simulations involving bootstrap hypothesis
tests or bootstrap confidence intervals

- [`bootstrap_pvals()`](https://meghapsimatrix.github.io/simhelpers/reference/bootstrap_pvals.md)
  : Calculate one or multiple bootstrap p-values
- [`bootstrap_CIs()`](https://meghapsimatrix.github.io/simhelpers/reference/bootstrap_CIs.md)
  : Calculate one or multiple bootstrap confidence intervals
- [`extrapolate_rejection()`](https://meghapsimatrix.github.io/simhelpers/reference/extrapolate_rejection.md)
  : Extrapolate coverage and width using sub-sampled bootstrap
  confidence intervals.
- [`extrapolate_coverage()`](https://meghapsimatrix.github.io/simhelpers/reference/extrapolate_coverage.md)
  : Extrapolate coverage and width using sub-sampled bootstrap
  confidence intervals.

## Simulation Workflow

Functions for facilitating simulation workflows

- [`create_skeleton()`](https://meghapsimatrix.github.io/simhelpers/reference/create_skeleton.md)
  : Open a simulation skeleton
- [`repeat_and_stack()`](https://meghapsimatrix.github.io/simhelpers/reference/repeat_and_stack.md)
  : Repeat an expression multiple times and (optionally) stack the
  results.
- [`bundle_sim()`](https://meghapsimatrix.github.io/simhelpers/reference/bundle_sim.md)
  : Bundle functions into a simulation driver function
- [`evaluate_by_row()`](https://meghapsimatrix.github.io/simhelpers/reference/evaluate_by_row.md)
  : Evaluate a simulation function on each row of a data frame or tibble

## Example Datasets

Example datasets from simulation studies

- [`Tipton_Pusto`](https://meghapsimatrix.github.io/simhelpers/reference/Tipton_Pusto.md)
  : Results for Figure 2 of Tipton & Pustejovsky (2015)
- [`alpha_res`](https://meghapsimatrix.github.io/simhelpers/reference/alpha_res.md)
  : Cronbach's alpha simulation results
- [`t_res`](https://meghapsimatrix.github.io/simhelpers/reference/t_res.md)
  : t-test simulation results
- [`welch_res`](https://meghapsimatrix.github.io/simhelpers/reference/welch_res.md)
  : Welch t-test simulation results
