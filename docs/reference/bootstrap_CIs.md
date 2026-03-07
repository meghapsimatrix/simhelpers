# Calculate one or multiple bootstrap confidence intervals

Calculate one or multiple bootstrap confidence intervals, given a sample
of bootstrap replications.

## Usage

``` r
bootstrap_CIs(
  boot_est,
  boot_se = NULL,
  est = NULL,
  se = NULL,
  influence = NULL,
  CI_type = "percentile",
  level = 0.95,
  B_vals = length(boot_est),
  reps = 1L,
  format = "wide",
  seed = NULL
)
```

## Arguments

- boot_est:

  vector of bootstrap replications of an estimator.

- boot_se:

  vector of estimated standard errors from each bootstrap replication.

- est:

  numeric value of the estimate based on the original sample. Required
  for `CI_type = "normal"`, `CI_type = "basic"`, `CI_type = "student"`,
  and `CI_type = "bias-corrected"`.

- se:

  numeric value of the estimated standard error based on the original
  sample. Required for `CI_type = "student"`.

- influence:

  vector of empirical influence values for the estimator. Required for
  `CI_type = "BCa"`.

- CI_type:

  Character string or vector of character strings indicating types of
  confidence intervals to calculate. Options are `"normal"`, `"basic"`,
  `"student"`, `"percentile"` (the default), `"bias-corrected"`, or
  `"BCa"`.

- level:

  numeric value between 0 and 1 for the desired coverage level, with a
  default of `0.95`.

- B_vals:

  vector of sub-sample sizes for which to calculate confidence
  intervals. Setting `B_vals = length(boot_est)` (the default) will
  return bootstrap confidence intervals calculated on the full set of
  bootstrap replications. For `B_vals < length(boot_est)`, confidence
  intervals will be calculated after sub-sampling (without replacement)
  the bootstrap replications.

- reps:

  integer value for the number of sub-sample confidence intervals to
  generate when `B_vals < length(boot_est)`, with a default of
  `reps = 1`.

- format:

  character string controlling the format of the output. If
  `format = "wide"` (the default), then different types of confidence
  intervals will be returned in separate columns. If `format = "long"`,
  then confidence intervals of different types will appear on different
  rows of dataset. If `format = "wide-list"`, then different types of
  confidence intervals will be returned in separate columns and the
  result will be wrapped in an unnamed list.

- seed:

  Single numeric value to which the random number generator seed will be
  set. Default is `NULL`, which does not set a seed.

## Value

If `format = "wide"`, the function returns a `data.frame` with `reps`
rows per entry of `B_vals`, where each row contains confidence intervals
for one sub-sample replication.

If `format = "long"`, the function returns a `data.frame` with one row
for each `CI_type`, each replication, and each entry of `B_vals`, where
each row contains a single confidence interval for one sub-sample
replication.

If `format = "wide-list"`, then the output will be structured as in
`format = "wide"` but will be wrapped in an unnamed list, which makes it
easier to sore the output in a tibble, and will be assigned the class
`"bootstrap_CIs"`.

## Details

Confidence intervals are calculated following the methods described in
Chapter 5 of Davison and Hinkley (1997). For basic non-parametric
bootstraps, the methods are nearly identical to the implementation in
[`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html) from the `boot`
package.

## References

Davison, A.C. and Hinkley, D.V. (1997). \_Bootstrap Methods and Their
Application\_, Chapter 5. Cambridge University Press.

## Examples

``` r
# generate t-distributed data
N <- 50
mu <- 2
nu <- 5
dat <- mu + rt(N, df = nu)

# create bootstrap replications
f <- \(x) {
 c(
   M = mean(x, trim = 0.1),
   SE = sd(x) / sqrt(length(x))
 )
}

booties <- replicate(399, {
  sample(dat, replace = TRUE, size = N) |>
  f()
})

res <- f(dat)

# calculate bootstrap CIs from full set of bootstrap replicates
bootstrap_CIs(
  boot_est = booties[1,],
  boot_se = booties[2,],
  est = res[1],
  se = res[2],
  CI_type = c("normal","basic","student","percentile","bias-corrected"),
  format = "long"
)
#>   bootstraps           type    lower    upper
#> 1        399         normal 2.028844 2.816268
#> 2        399          basic 2.040644 2.818643
#> 3        399        student 2.033162 2.870852
#> 4        399     percentile 2.022576 2.800575
#> 5        399 bias-corrected 2.022576 2.797262

# Calculate bias-corrected-and-accelerated CIs
inf_vals <- res[1] - sapply(seq_along(dat), \(i) f(dat[-i])[1])
bootstrap_CIs(
  boot_est = booties[1,],
  est = res[1],
  influence = inf_vals,
  CI_type = c("percentile","bias-corrected","BCa"),
  format = "long"
)
#>   bootstraps           type    lower    upper
#> 1        399     percentile 2.022576 2.800575
#> 2        399 bias-corrected 2.022576 2.797262
#> 3        399            BCa 2.022576 2.797262

# calculate multiple bootstrap CIs using sub-sampling of replicates
bootstrap_CIs(
  boot_est = booties[1,],
  boot_se = booties[2,],
  est = res[1],
  se = res[2],
  CI_type = c("normal","basic","student","percentile","bias-corrected"),
  B_vals = 199,
  reps = 4L,
  format = "long"
)
#>    bootstraps           type    lower    upper
#> 1         199         normal 2.031996 2.859805
#> 2         199          basic 2.044270 2.875748
#> 3         199        student 2.057285 2.957861
#> 4         199     percentile 1.965472 2.796949
#> 5         199 bias-corrected 2.001888 2.804617
#> 6         199         normal 2.021828 2.873832
#> 7         199          basic 1.999816 2.839331
#> 8         199        student 1.976411 2.885610
#> 9         199     percentile 2.001888 2.841403
#> 10        199 bias-corrected 2.051460 2.930256
#> 11        199         normal 2.001092 2.838692
#> 12        199          basic 2.044270 2.874536
#> 13        199        student 2.035858 2.948230
#> 14        199     percentile 1.966683 2.796949
#> 15        199 bias-corrected 1.966683 2.793398
#> 16        199         normal 2.016251 2.780760
#> 17        199          basic 2.043957 2.790233
#> 18        199        student 2.028818 2.876856
#> 19        199     percentile 2.050987 2.797262
#> 20        199 bias-corrected 2.001888 2.774727

# calculate multiple bootstrap CIs using sub-sampling of replicates,
# for each of several sub-sample sizes.
bootstrap_CIs(
  boot_est = booties[1,],
  boot_se = booties[2,],
  est = res[1],
  se = res[2],
  CI_type = c("normal","basic","student","percentile"),
  B_vals = c(49,99,199),
  reps = 4L,
  format = "long"
)
#>    bootstraps       type    lower    upper
#> 1          49     normal 2.000343 2.877043
#> 2          49      basic 2.036602 2.874536
#> 3          49    student 2.033162 2.948230
#> 4          49 percentile 1.966683 2.804617
#> 5          49     normal 2.031352 2.905860
#> 6          49      basic 2.043957 2.791094
#> 7          49    student 2.074542 2.823714
#> 8          49 percentile 2.050126 2.797262
#> 9          49     normal 2.020648 2.720565
#> 10         49      basic 2.050924 2.729620
#> 11         49    student 2.085570 2.699863
#> 12         49 percentile 2.111599 2.790295
#> 13         49     normal 2.043214 2.886938
#> 14         49      basic 2.138788 2.874536
#> 15         49    student 2.140775 2.885610
#> 16         49 percentile 1.966683 2.702431
#> 17         99     normal 2.067627 2.853661
#> 18         99      basic 2.047821 2.791094
#> 19         99    student 2.035858 2.846810
#> 20         99 percentile 2.050126 2.793398
#> 21         99     normal 2.002904 2.856506
#> 22         99      basic 2.031207 2.839331
#> 23         99    student 2.008973 2.957861
#> 24         99 percentile 2.001888 2.810012
#> 25         99     normal 2.032474 2.777077
#> 26         99      basic 2.072614 2.791663
#> 27         99    student 2.096600 2.948230
#> 28         99 percentile 2.049556 2.768605
#> 29         99     normal 2.008817 2.809936
#> 30         99      basic 1.969681 2.789759
#> 31         99    student 1.950793 2.876856
#> 32         99 percentile 2.051460 2.871538
#> 33        199     normal 2.018971 2.826070
#> 34        199      basic 2.044270 2.875748
#> 35        199    student 2.057285 2.885610
#> 36        199 percentile 1.965472 2.796949
#> 37        199     normal 2.026626 2.833965
#> 38        199      basic 2.036602 2.874536
#> 39        199    student 2.033162 2.915899
#> 40        199 percentile 1.966683 2.804617
#> 41        199     normal 2.021649 2.799257
#> 42        199      basic 2.040644 2.791094
#> 43        199    student 2.008973 2.866753
#> 44        199 percentile 2.050126 2.800575
#> 45        199     normal 2.023718 2.828731
#> 46        199      basic 2.040644 2.839331
#> 47        199    student 2.033162 2.885610
#> 48        199 percentile 2.001888 2.800575
```
