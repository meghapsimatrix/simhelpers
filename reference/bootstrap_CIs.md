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
#> 1        399         normal 1.956827 2.713912
#> 2        399          basic 1.932965 2.698004
#> 3        399        student 1.916762 2.720168
#> 4        399     percentile 1.969730 2.734769
#> 5        399 bias-corrected 1.980209 2.737876

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
#> 1        399     percentile 1.969730 2.734769
#> 2        399 bias-corrected 1.980209 2.737876
#> 3        399            BCa 1.980209 2.737876

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
#> 1         199         normal 1.980454 2.694244
#> 2         199          basic 1.950599 2.698004
#> 3         199        student 1.943938 2.704483
#> 4         199     percentile 1.969730 2.717136
#> 5         199 bias-corrected 1.980209 2.717136
#> 6         199         normal 1.971123 2.694702
#> 7         199          basic 1.962497 2.691840
#> 8         199        student 1.948314 2.704483
#> 9         199     percentile 1.975895 2.705237
#> 10        199 bias-corrected 2.002991 2.760457
#> 11        199         normal 1.947918 2.691788
#> 12        199          basic 1.920147 2.691840
#> 13        199        student 1.915225 2.720168
#> 14        199     percentile 1.975895 2.747588
#> 15        199 bias-corrected 1.961454 2.705237
#> 16        199         normal 1.940259 2.719847
#> 17        199          basic 1.920147 2.691840
#> 18        199        student 1.916762 2.704623
#> 19        199     percentile 1.975895 2.747588
#> 20        199 bias-corrected 1.961454 2.661461

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
#> 1          49     normal 1.954279 2.756996
#> 2          49      basic 2.004221 2.686558
#> 3          49    student 1.987738 2.701393
#> 4          49 percentile 1.981176 2.663513
#> 5          49     normal 1.940103 2.700159
#> 6          49      basic 1.901757 2.641634
#> 7          49    student 1.901939 2.672398
#> 8          49 percentile 2.026100 2.765978
#> 9          49     normal 2.009940 2.709993
#> 10         49      basic 2.016382 2.664743
#> 11         49    student 2.023731 2.704483
#> 12         49 percentile 2.002991 2.651352
#> 13         49     normal 2.005915 2.723204
#> 14         49      basic 2.109050 2.691840
#> 15         49    student 2.101210 2.720485
#> 16         49 percentile 1.975895 2.558685
#> 17         99     normal 1.943855 2.668960
#> 18         99      basic 1.907278 2.654617
#> 19         99    student 1.901939 2.650926
#> 20         99 percentile 2.013117 2.760457
#> 21         99     normal 1.959127 2.709051
#> 22         99      basic 1.907278 2.686846
#> 23         99    student 1.926578 2.720168
#> 24         99 percentile 1.980888 2.760457
#> 25         99     normal 1.995844 2.670863
#> 26         99      basic 1.993127 2.655265
#> 27         99    student 1.982072 2.704483
#> 28         99 percentile 2.012469 2.674607
#> 29         99     normal 1.965503 2.729939
#> 30         99      basic 1.920147 2.746083
#> 31         99    student 1.900402 2.756335
#> 32         99 percentile 1.921651 2.747588
#> 33        199     normal 1.954786 2.688914
#> 34        199      basic 1.932965 2.686846
#> 35        199    student 1.926578 2.720168
#> 36        199 percentile 1.980888 2.734769
#> 37        199     normal 1.956729 2.736172
#> 38        199      basic 1.947668 2.746083
#> 39        199    student 1.929162 2.757393
#> 40        199 percentile 1.921651 2.720066
#> 41        199     normal 1.953180 2.728824
#> 42        199      basic 1.920147 2.720873
#> 43        199    student 1.916762 2.749270
#> 44        199 percentile 1.946861 2.747588
#> 45        199     normal 1.963244 2.708885
#> 46        199      basic 1.920147 2.691840
#> 47        199    student 1.901939 2.694993
#> 48        199 percentile 1.975895 2.747588
```
