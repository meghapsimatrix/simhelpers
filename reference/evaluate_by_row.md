# Evaluate a simulation function on each row of a data frame or tibble

Evaluates a simulation function on each row of a data frame or tibble
containing parameter values. Returns a single tibble with parameters and
simulation results. The function uses
[`furrr::future_pmap`](https://furrr.futureverse.org/reference/future_map2.html),
which allows for easy parallelization.

## Usage

``` r
evaluate_by_row(
  params,
  sim_function,
  ...,
  nest_results = FALSE,
  results_name = ".results",
  .progress = FALSE,
  .options = furrr::furrr_options(seed = TRUE),
  system_time = TRUE,
  verbose = TRUE
)
```

## Arguments

- params:

  data frame or tibble containing simulation parameter values. Each row
  should represent a separate set of parameter values. Column names must
  exactly match the argument names of `sim_function`. Non-matching
  columns are ignored.

- sim_function:

  function to be evaluated, with argument names matching the variable
  names in `params`. The function must return a `data.frame`, `tibble`,
  or vector.

- ...:

  additional arguments passed to `sim_function`.

- nest_results:

  logical indicating whether to store the results of evaluating
  `sim_function` in a nested column. Default is `FALSE`.

- results_name:

  character string to set the name of the nested column storing the
  results of the simulation. Default is `".results"`.

- .progress:

  A single logical. Should a progress bar be displayed? Only works with
  multisession, multicore, and multiprocess futures. Note that if a
  multicore/multisession future falls back to sequential, then a
  progress bar will not be displayed.

  **Warning:** The `.progress` argument will be deprecated and removed
  in a future version of furrr in favor of using the more robust
  [progressr](https://CRAN.R-project.org/package=progressr) package.

- .options:

  The `future` specific options to use with the workers. This must be
  the result from a call to
  [`furrr_options()`](https://furrr.futureverse.org/reference/furrr_options.html).

- system_time:

  logical indicating whether to print computation time. `TRUE` by
  default.

- verbose:

  logical indicating whether to display a message about variables used
  in function evaluation. `TRUE` by default.

## Value

A tibble containing parameter values and simulation results.

## Examples

``` r
df <- data.frame(
  n = 3:5,
  lambda = seq(8, 16, 4)
)

evaluate_by_row(df, rpois)
#> Evaluating rpois() using the following variables: n, lambda
#>    user  system elapsed 
#>   0.076   0.000   0.076 
#> # A tibble: 12 × 3
#>        n lambda .results
#>    <int>  <dbl>    <int>
#>  1     3      8        2
#>  2     3      8        7
#>  3     3      8       10
#>  4     4     12       10
#>  5     4     12       17
#>  6     4     12       18
#>  7     4     12        9
#>  8     5     16       14
#>  9     5     16       15
#> 10     5     16       13
#> 11     5     16       14
#> 12     5     16       15
```
