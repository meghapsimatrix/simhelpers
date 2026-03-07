# t-test simulation results

A dataset containing simulation results from a study that just runs a
t-test.

## Usage

``` r
t_res
```

## Format

A tibble with 1,000 rows and 5 variables:

- est:

  estimate of the mean difference.

- p_val:

  p-value from the t-test.

- lower_bound:

  lower bound of the confidence interval.

- upper_bound:

  upper bound of the confidence interval.

- true_param:

  true mean difference used to generate the data.
