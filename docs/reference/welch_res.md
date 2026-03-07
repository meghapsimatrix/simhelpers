# Welch t-test simulation results

A dataset containing simulation results from a study comparing Welch
t-test to the conventional t-test.

## Usage

``` r
welch_res
```

## Format

A tibble with 16,000 rows and 11 variables:

- n1:

  sample size for Group 1.

- n2:

  sample size for Group 2.

- mean_diff:

  true difference in means of two groups used to generate the data.

- iterations:

  number of iterations.

- seed:

  seed used to generate data.

- method:

  indicates whether Welch or conventional t-test was used.

- est:

  estimate of the mean difference.

- var:

  variance of the estimate.

- p_val:

  p-value from the t-test.

- lower_bound:

  lower bound of the confidence interval.

- upper_bound:

  upper bound of the confidence interval.
