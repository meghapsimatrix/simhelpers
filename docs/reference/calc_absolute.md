# Calculate absolute performance criteria and MCSE

Calculates absolute bias, variance, mean squared error (mse) and root
mean squared error (rmse). The function also calculates the associated
Monte Carlo standard errors.

## Usage

``` r
calc_absolute(
  data,
  estimates,
  true_param,
  criteria = c("bias", "variance", "stddev", "mse", "rmse"),
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
  be calculated, with possible options `"bias"`, `"variance"`,
  `"stddev"`, `"mse"`, and `"rmse"`.

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
calc_absolute(data = t_res, estimates = est, true_param = true_param)
#> # A tibble: 1 × 11
#>   K_absolute    bias bias_mcse    var var_mcse stddev stddev_mcse    mse
#>        <int>   <dbl>     <dbl>  <dbl>    <dbl>  <dbl>       <dbl>  <dbl>
#> 1       1000 0.00233   0.00638 0.0407  0.00183  0.202     0.00457 0.0407
#> # ℹ 3 more variables: mse_mcse <dbl>, rmse <dbl>, rmse_mcse <dbl>
```
