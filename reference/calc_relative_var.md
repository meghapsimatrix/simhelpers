# Calculate jack-knife Monte Carlo SE for variance estimators

Calculates relative bias, mean squared error (relative mse), and root
mean squared error (relative rmse) of variance estimators. The function
also calculates the associated jack-knife Monte Carlo standard errors.

## Usage

``` r
calc_relative_var(
  data,
  estimates,
  var_estimates,
  criteria = c("relative bias", "relative mse", "relative rmse"),
  winz = Inf,
  var_winz = winz
)
```

## Arguments

- data:

  data frame or tibble containing the simulation results.

- estimates:

  vector or name of column from `data` containing point estimates.

- var_estimates:

  vector or name of column from `data` containing variance estimates for
  point estimator in `estimates`.

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

- var_winz:

  numeric value for winsorization constant for the variance estimates.
  If set to a finite value, variance estimates will be winsorized at the
  constant multiple of the inter-quartile range below the 25th
  percentile or above the 75th percentile of the distribution. For
  instance, setting `var_winz = 3` will truncate variance estimates that
  fall below P25 - 3 \* IQR or above P75 + 3 \* IQR. By default
  `var_winz` is set to the same constant as `winsorize`.

## Value

A tibble containing the number of simulation iterations, performance
criteria estimate(s) and the associated MCSE.

## Examples

``` r
calc_relative_var(data = alpha_res, estimates = A, var_estimates = Var_A)
#> # A tibble: 1 × 7
#>   K_relvar rel_bias_var rel_bias_var_mcse rel_mse_var rel_mse_var_mcse
#>      <int>        <dbl>             <dbl>       <dbl>            <dbl>
#> 1     1000        0.440             0.101       0.726            0.337
#> # ℹ 2 more variables: rel_rmse_var <dbl>, rel_rmse_var_mcse <dbl>
```
