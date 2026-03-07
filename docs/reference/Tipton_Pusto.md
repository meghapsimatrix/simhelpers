# Results for Figure 2 of Tipton & Pustejovsky (2015)

A dataset containing simulation results comparing small sample
correction methods for cluster robust variance estimation in
meta-analysis.

## Usage

``` r
Tipton_Pusto
```

## Format

A tibble with 15,300 rows and 8 variables:

- num_studies:

  the number of studies included in the meta-analysis.

- r:

  correlation between outcomes.

- Isq:

  measure of heterogeneity of true effects.

- contrast:

  type of contrast that was tested.

- test:

  small sample method used.

- q:

  the number of parameters in the hypothesis test.

- rej_rate:

  the Type 1 error rate.

- mcse:

  the Monte Carlo standard error for the estimate of the Type 1 error
  rate.

## Source

Tipton E, Pustejovsky JE (2015). “Small-sample adjustments for tests of
moderators and model fit using robust variance estimation in
meta-regression.” *Journal of Educational and Behavioral Statistics*,
**40**(6), 604–634.
[doi:10.3102/1076998615606099](https://doi.org/10.3102/1076998615606099)
.
