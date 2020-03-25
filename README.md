
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simhelpers

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/meghapsimatrix/simhelpers.svg?branch=master)](https://travis-ci.org/meghapsimatrix/simhelpers)
[![Codecov test
coverage](https://codecov.io/gh/meghapsimatrix/simhelpers/branch/master/graph/badge.svg)](https://codecov.io/gh/meghapsimatrix/simhelpers?branch=master)
<!-- badges: end -->

Monte Carlo Simulations are computer experiments designed to study the
performance of statistical methods under known data-generating
conditions (Morris, White & Crowther, 2018). Methodologists use
simulations to examine questions such as: (1) how does ordinary least
squares regression perform if errors are heteroskedastic? (2) how does
the presence of missing data affect treatment effect estimates from a
propensity score analysis? (3) how does cluster robust variance
estimation perform when the number of clusters is small? To answer such
questions, we conduct experiments by simulating thousands of datasets
based on pseudo-random sampling, applying statistical methods, and
evaluating how well those statistical methods recover the true
data-generating conditions (Morris et al., 2018).

The goal of `simhelpers` is to assist in running simulation studies. The
main tools in the package consist of functions to calculate measures of
estimator performance like bias, root mean squared error, rejection
rates. The functions also calculate the associated Monte Carlo standard
errors (MCSE) of the performance measures. These functions are divided
into three major categories of performance criteria: absolute criteria,
relative criteria, and criteria to evaluate hypothesis testing. The
functions use the
[`tidyeval`](https://tidyeval.tidyverse.org/index.html) principles, so
that they play well with `dplyr` and fit easily into a `%>%`-centric
workflow.

In addition to the set of functions that calculates performance measures
and MCSE, the package also includes a function, `create_skeleton()`,
that generates a skeleton outline for a simulation study. Another
function, `evaluate_by_row()`, runs the simulation for each combination
of conditions row by row. This function uses
[`future_pmap()`](https://davisvaughan.github.io/furrr/reference/future_map2.html)
from the [`furrr`](https://davisvaughan.github.io/furrr/) package,
making it easy to run the simulation in parallel. The package also
includes several datasets that contain results from example simulation
studies.

<img src="man/figures/workflow.png" />

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("meghapsimatrix/simhelpers")
```

## Related Work

Our explanation of MCSE formulas and our general simulation workflow is
closely aligned with the approach described by Morris et al. (2018). We
want to recognize several other R packages that offer functionality for
conducting Monte Carlo simulation studies. In particular, the
[`rsimsum`](https://cran.r-project.org/web/packages/rsimsum/index.html)
package (which has a lovely name that makes me hungry) also calculates
Monte Carlo standard errors. The
[`SimDesign`](https://cran.r-project.org/web/packages/SimDesign/index.html)
package implements a generate-analyze-summarize model for writing
simulations; it also includes tools for error handling and parallel
computing.

In contrast to the two packages mentioned above, our package works uses
[`tidyeval`](https://tidyeval.tidyverse.org/index.html) and outputs
tibbles, which can then easily be used with
[`dplyr`](https://dplyr.tidyverse.org/index.html),
[`tidyr`](https://tidyr.tidyverse.org/) and
[`purrr`](https://purrr.tidyverse.org/) syntax. The functions that
calulate MCSEs are easy to run on grouped data. For parallel computing,
we use the [`furrr`](https://davisvaughan.github.io/furrr/) and
[`future`](https://rstudio.github.io/promises/articles/futures.html)
packages. Moreover, in contrast to the `rsimsum` and `SimDesign`
packages, `simhelpers` provides jacknife MCSE for variance estimators.
It also provides jacknife MCSE estimates for root mean squared error.

Another related project is
[`DeclareDesign`](https://declaredesign.org/), a suite of packages that
allow users to declare and diagnose research designs, fabricate mock
data, and explore tradeoffs between different designs. This project
follows a similar model for how simulation studies are instantiated, but
it uses a higher-level API, which is tailored for simulating certain
specific types of research designs. In contrast, our package is a
simpler set of general-purpose utility functions.

Other packages that have similar aims to `simhelpers` include:
[MonteCarlo](https://cran.r-project.org/web/packages/MonteCarlo/index.html),
[parSim](https://cran.r-project.org/web/packages/parSim/index.html),
[simsalapar](https://cran.r-project.org/web/packages/simsalapar/index.html),
[simulator](https://cran.r-project.org/web/packages/simulator/index.html),
[simstudy](https://cran.r-project.org/web/packages/simstudy/index.html),
[simTool](https://cran.r-project.org/web/packages/simTool/index.html),
[simSummary](https://cran.r-project.org/web/packages/simSummary/index.html),
and [ezsim](https://cran.r-project.org/web/packages/ezsim/index.html).

## References

Bengtsson, H. (2020). future: Unified Parallel and Distributed
Processing in R for Everyone. R package version 1.16.0.
<https://CRAN.R-project.org/package=future>

Chalmers, P. (2019). SimDesign: Structure for Organizing Monte Carlo
Simulation Designs. R package version 1.14.
<https://CRAN.R-project.org/package=SimDesign>

Gasparini, A. (2018). rsimsum: Summarise results from Monte Carlo
simulation studies. Journal of Open Source Software, 3(26), 739,
<https://doi.org/10.21105/joss.00739> CRAN:
<https://cran.r-project.org/web/packages/rsimsum/index.html>

Graeme B., Jasper C., Alexander C., & Macartan H. (2019). Declaring and
Diagnosing Research Designs. American Poliitcal Science Review 113(3):
838-859. URL <http://declaredesign.org/paper.pdf> CRAN:
<https://cran.r-project.org/web/packages/DeclareDesign/index.html>

Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. Statistics in medicine, 38(11),
2074-2102. <http://doi.org/10.1002/sim.8086>

Vaughan, D., & Dancho, M. (2018). furrr: Apply Mapping Functions in
Parallel using Futures. R package version 0.1.0.
<https://CRAN.R-project.org/package=furrr>

Wickham, H., et al., (2019). Welcome to the tidyverse. Journal of Open
Source Software, 4(43), 1686, <https://doi.org/10.21105/joss.01686>

## Acknowledgments

We are grateful for the feedback provided by [Sangdon
Lim](https://sdlim.com/) and Man Chen.
