# Calculate one or multiple bootstrap p-values

Calculate one or multiple bootstrap p-values, given a bootstrap sample
of test statistics.

## Usage

``` r
bootstrap_pvals(
  boot_stat,
  stat,
  alternative = "two-sided",
  B_vals = length(boot_stat),
  reps = 1L,
  enlist = FALSE,
  seed = NULL
)
```

## Arguments

- boot_stat:

  vector of bootstrap replications of a test statistic.

- stat:

  numeric value of the test statistic based on the original sample.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"two-sided"` (the default), `"greater"` or `"less"`.

- B_vals:

  vector of sub-sample sizes for which to calculate p-values. Setting
  `B_vals = length(boot_stat)` (the default) will return a single
  p-value calculated on the full set of bootstrap replications. For
  `B_vals < length(boot_stat)`, p-values will be calculated after
  sub-sampling (without replacement) the bootstrap replications.

- reps:

  integer value for the number of sub-sample p-values to generate when
  `B_vals < length(boot_stat)`, with a default of `reps = 1`.

- enlist:

  logical indicating whether to wrap the returned values in an unnamed
  list, with a default of `FALSE`. Setting `enlist = TRUE` makes it
  easier to store the output as a single entry in a `tibble`.

- seed:

  Single numeric value to which the random number generator seed will be
  set. Default is `NULL`, which does not set a seed.

## Value

The format of the output depends on several contingencies. If only a
single value of `B_vals` is specified and `reps = 1`, then the function
returns a vector with a single p-value. If only a single value of
`B_vals` is specified but `B_vals < length(boot_stat)` and `reps > 1`,
then the function returns a vector p-values, with an entry for each
sub-sample replication. If `B_vals` is a vector of multiple values, then
the function returns a list with one entry per entry of `B_vals`, where
each entry is a vector of length `reps` with entries for each sub-sample
replication.

If `enlist = TRUE`, then results will be wrapped in an unnamed list,
which makes it easier to sore the output in a tibble.

## Details

p-values are calculated by comparing `stat` to the distribution of
`boot_stat`, which is taken to represent the null distribution of the
test statistic. If `alternative = "two-sided"` (the default), then the
p-value is the proportion of the bootstrap sample where the absolute
value of the bootstrapped statistic exceeds the absolute value of the
original statistic. If `alternative = "greater"`, then the p-value is
the proportion of the bootstrap sample where the value of the
bootstrapped statistic is larger than the original statistic. If
`alternative = "less"`, then the p-value is the proportion of the
bootstrap sample where the value of the bootstrapped statistic is less
than the original statistic.

## References

Davison, A.C. and Hinkley, D.V. (1997). \_Bootstrap Methods and Their
Application\_, Chapter 4. Cambridge University Press.

## Examples

``` r
# generate data from two distinct populations
dat <- data.frame(
  group = rep(c("A","B"), c(40, 50)),
  y = c(
    rgamma(40, shape = 7, scale = 2),
    rgamma(50, shape = 3, scale = 4)
  )
)
stat <- t.test(y ~ group, data = dat)$statistic

# create bootstrap replications under the null of no difference
boot_dat <- dat
booties <- replicate(399, {
  boot_dat$group <- sample(dat$group)
  t.test(y ~ group, data = boot_dat)$statistic
})

# calculate bootstrap p-values from full set of bootstrap replicates
bootstrap_pvals(boot_stat = booties, stat = stat)
#>   bootstraps      pval
#> 1        399 0.6917293

# calculate multiple bootstrap p-values using sub-sampling of replicates
bootstrap_pvals(
  boot_stat = booties, stat = stat,
  B_vals = 199,
  reps = 4L
)
#>   bootstraps                                       pval
#> 1        199 0.7085427, 0.7035176, 0.6633166, 0.6783920

# calculate multiple bootstrap p-values using sub-sampling of replicates,
# for each of several sub-sample sizes.
bootstrap_pvals(
  boot_stat = booties, stat = stat,
  B_vals = c(49,99,199),
  reps = 4L
)
#>   bootstraps                                       pval
#> 1         49 0.6530612, 0.7551020, 0.6938776, 0.6734694
#> 2         99 0.6868687, 0.6363636, 0.6666667, 0.7070707
#> 3        199 0.6884422, 0.6884422, 0.6633166, 0.7286432
```
