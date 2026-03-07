# Changelog

## simhelpers 0.3.1.9999

- Added option to specify a unique ID variable for every repetition in
  [`repeat_and_stack()`](https://meghapsimatrix.github.io/simhelpers/reference/repeat_and_stack.md)
  and
  [`bundle_sim()`](https://meghapsimatrix.github.io/simhelpers/reference/bundle_sim.md).
- Refactored
  [`bundle_sim()`](https://meghapsimatrix.github.io/simhelpers/reference/bundle_sim.md)
  so that it uses
  [`repeat_and_stack()`](https://meghapsimatrix.github.io/simhelpers/reference/repeat_and_stack.md)
  internally.
- Refined
  [`evaluate_by_row()`](https://meghapsimatrix.github.io/simhelpers/reference/evaluate_by_row.md):
  - Refactored so that it uses only variables that match argument names
    of the function to be evaluated.
  - Added an option `nest_results` to return a nested column of results
    (for more compact storage).
- [`extrapolate_coverage()`](https://meghapsimatrix.github.io/simhelpers/reference/extrapolate_coverage.md)
  and
  [`extrapolate_rejection()`](https://meghapsimatrix.github.io/simhelpers/reference/extrapolate_rejection.md)
  gain an `exclude_above` argument to exclude results based on more than
  a certain number of bootstraps from being included in the
  extrapolation calculations.

## simhelpers 0.3.1

CRAN release: 2025-01-10

- Added support for bias-corrected and bias-corrected-and-accelerated
  (BCa) bootstrap confidence intervals.
- Corrected an error in the documentation of
  [`bundle_sim()`](https://meghapsimatrix.github.io/simhelpers/reference/bundle_sim.md).

## simhelpers 0.3.0

CRAN release: 2024-09-04

- Added functions for calculating bootstrap p-values and confidence
  intervals and for estimating rejection rates, coverage rates, and
  interval widths by extrapolating across bootstrap subsamples.
- Added
  [`repeat_and_stack()`](https://meghapsimatrix.github.io/simhelpers/reference/repeat_and_stack.md)
  function, which is similar to the base R
  [`replicate()`](https://rdrr.io/r/base/lapply.html) and to the now
  deprecated
  [`purrr::rerun()`](https://purrr.tidyverse.org/reference/rerun.html),
  with the option to stack the output into a single `data.frame`.
- Added `"stddev"` as a performance criterion in
  [`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md).
- Added `winsorize` options in
  [`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md),
  [`calc_relative()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative.md),
  [`calc_relative_var()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative_var.md),
  and
  [`calc_coverage()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_coverage.md).

## simhelpers 0.2.1

CRAN release: 2024-02-29

- Fixed issues in unit tests on Mac OS, M1Mac, and NoLD.

## simhelpers 0.2.0

CRAN release: 2024-02-23

- Added new, experimental function
  [`bundle_sim()`](https://meghapsimatrix.github.io/simhelpers/reference/bundle_sim.md)
  to compose a set of functions into a simulation driver.
- Added an argument to
  [`evaluate_by_row()`](https://meghapsimatrix.github.io/simhelpers/reference/evaluate_by_row.md)
  to control the name of the variable where simulation results are
  stored.
- Revised the `calc_*()` functions so that they can take vectors or
  variable names from a specified dataset.
- [`calc_rejection()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_rejection.md)
  can now compute rejection rates for multiple `alpha` levels.
- Renamed the `K` variable computed in the `calc_*()` functions to avoid
  over-writing variables when using multiple performance calculations
  inside of
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html).
- Updated syntax in vignettes and examples to use current tidyverse
  conventions.

## simhelpers 0.1.2

CRAN release: 2022-05-03

- Removed import of a defunct function from the furrr package.

## simhelpers 0.1.1

CRAN release: 2021-02-14

- Fixed formula for jacknife MCSE

## simhelpers 0.1.0

CRAN release: 2020-03-31

- First version
