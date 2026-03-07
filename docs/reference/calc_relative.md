# Calculate relative performance criteria and MCSE

Calculates relative bias, mean squared error (relative mse), and root
mean squared error (relative rmse). The function also calculates the
associated Monte Carlo standard errors.

## Usage

``` r
calc_relative(
  data,
  estimates,
  true_param,
  criteria = c("relative bias", "relative mse", "relative rmse"),
  winz = Inf
)
```

## Arguments

- data:

  data frame or tibble containing the simulation results.

- estimates:

  vector or name of column from `data` containing point estimates.

- true_param:

  vector or name of column from `data` containing corresponding true
  parameters.

- criteria:

  character or character vector indicating the performance criteria to
  be calculated, with possible options `"relative bias"`,
  `"relative mse"`, and `"relative rmse"`.

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
calc_relative(data = t_res, estimates = est, true_param = true_param)
#> # A tibble: 1 × 7
#>   K_relative rel_bias rel_bias_mcse rel_mse rel_mse_mcse rel_rmse rel_rmse_mcse
#>        <int>    <dbl>         <dbl>   <dbl>        <dbl>    <dbl>         <dbl>
#> 1       1000     1.00        0.0128   0.163      0.00733    0.403        0.0111

```
