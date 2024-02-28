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
