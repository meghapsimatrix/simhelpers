# Calculate rejection rate and MCSE

Calculates rejection rate. The function also calculates the associated
Monte Carlo standard error.

## Usage

``` r
calc_rejection(data, p_values, alpha = 0.05, format = "wide")
```

## Arguments

- data:

  data frame or tibble containing the simulation results.

- p_values:

  vector or name of column from `data` containing p-values.

- alpha:

  scalar or vector indicating the nominal alpha level(s). Default value
  is set to the conventional .05.

- format:

  option `"wide"` (the default) will produce a tibble with one row, with
  separate variables for each specified `alpha`. Option `"long"` will
  produce a tibble with one row per specified `alpha`.

## Value

A tibble containing the number of simulation iterations, performance
criteria estimate and the associated MCSE.

## Examples

``` r
calc_rejection(data = t_res, p_values = p_val)
#> # A tibble: 1 × 3
#>   K_rejection rej_rate rej_rate_mcse
#>         <int>    <dbl>         <dbl>
#> 1        1000    0.702        0.0145

```
