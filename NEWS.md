simhelpers 0.3.1
=======================
* Added support for bias-corrected and bias-corrected-and-accelerated (BCa) bootstrap confidence intervals.
* Corrected an error in the documentation of `bundle_sim()`.

simhelpers 0.3.0
=======================
* Added functions for calculating bootstrap p-values and confidence intervals and for estimating rejection rates, coverage rates, and interval widths by extrapolating across bootstrap subsamples. 
* Added `repeat_and_stack()` function, which is similar to the base R `replicate()` and to the now deprecated `purrr::rerun()`, with the option to stack the output into a single `data.frame`.
* Added `"stddev"` as a performance criterion in `calc_absolute()`.
* Added `winsorize` options in `calc_absolute()`, `calc_relative()`, `calc_relative_var()`, and `calc_coverage()`.

simhelpers 0.2.1
=======================
* Fixed issues in unit tests on Mac OS, M1Mac, and NoLD.

simhelpers 0.2.0
=======================

* Added new, experimental function `bundle_sim()` to compose a set of functions into a simulation driver.
* Added an argument to `evaluate_by_row()` to control the name of the variable where simulation results are stored.
* Revised the `calc_*()` functions so that they can take vectors or variable names from a specified dataset.
* `calc_rejection()` can now compute rejection rates for multiple `alpha` levels. 
* Renamed the `K` variable computed in the `calc_*()` functions to avoid over-writing variables when using multiple performance calculations inside of `dplyr::summarize()`. 
* Updated syntax in vignettes and examples to use current tidyverse conventions.

simhelpers 0.1.2
=======================

* Removed import of a defunct function from the furrr package. 

simhelpers 0.1.1
=======================

* Fixed formula for jacknife MCSE

simhelpers 0.1.0
=======================

* First version
