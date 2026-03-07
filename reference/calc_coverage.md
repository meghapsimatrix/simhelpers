# Calculate confidence interval coverage, width and MCSE

Calculates confidence interval coverage and width. The function also
calculates the associated Monte Carlo standard errors. The confidence
interval percentage is based on how you calculated the lower and upper
bounds.

## Usage

``` r
calc_coverage(
  data,
  lower_bound,
  upper_bound,
  true_param,
  criteria = c("coverage", "width"),
  winz = Inf
)
```

## Arguments

- data:

  data frame or tibble containing the simulation results.

- lower_bound:

  vector or name of column from `data` containing lower bounds of
  confidence intervals.

- upper_bound:

  vector or name of column from `data` containing upper bounds of
  confidence intervals.

- true_param:

  vector or name of column from `data` containing corresponding true
  parameters.

- criteria:

  character or character vector indicating the performance criteria to
  be calculated, with possible options `"coverage"` and `"width"`.

- winz:

  numeric value for winsorization constant. If set to a finite value,
  estimates will be winsorized at the constant multiple of the
  inter-quartile range below the 25th percentile or above the 75th
  percentile of the distribution. For instance, setting `winz = 3` will
  truncate estimates that fall below P25 - 3 \* IQR or above P75 + 3 \*
  IQR.

## Value

A tibble containing the number of simulation iterations, performance
criteria estimate(s) and the associated MCSE.

## Examples

``` r
calc_coverage(data = t_res, lower_bound = lower_bound,
              upper_bound = upper_bound, true_param = true_param)
#> # A tibble: 1 × 5
#>   K_coverage coverage coverage_mcse width width_mcse
#>        <int>    <dbl>         <dbl> <dbl>      <dbl>
#> 1       1000    0.951       0.00683 0.791    0.00179

```
