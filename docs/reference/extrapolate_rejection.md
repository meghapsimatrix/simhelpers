# Extrapolate coverage and width using sub-sampled bootstrap confidence intervals.

Given a set of bootstrap confidence intervals calculated across
sub-samples with different numbers of replications, extrapolates
confidence interval coverage and width of bootstrap confidence intervals
to a specified (larger) number of bootstraps. The function also
calculates the associated Monte Carlo standard errors. The confidence
interval percentage is based on how you calculated the lower and upper
bounds.

## Usage

``` r
extrapolate_rejection(
  data,
  pvalue_subsamples,
  B_target = Inf,
  exclude_above = B_target - 1L,
  alpha = 0.05,
  nested = FALSE,
  format = "wide"
)
```

## Arguments

- data:

  data frame or tibble containing the simulation results.

- pvalue_subsamples:

  list or name of column from `data` containing list of confidence
  intervals calculated based on sub-samples with different numbers of
  replications.

- B_target:

  number of bootstrap replications to which the criteria should be
  extrapolated, with a default of `B = Inf`.

- exclude_above:

  numeric threshold for bootstrap replication sizes that should be
  excluded before calculating extrapolations. By default, set to
  `B_target - 1L` so that p-values based on B_target or greater
  replications will be excluded from the extrapolation calculations.

- alpha:

  scalar or vector indicating the nominal alpha level(s). Default value
  is set to the conventional .05.

- nested:

  logical value controlling the format of the output. If `FALSE` (the
  default), then the results will be returned as a data frame with rows
  for each distinct number of bootstraps. If `TRUE`, then the results
  will be returned as a data frame with a single row, with each
  performance criterion containing a nested data frame.

- format:

  character string controlling the format of the output when
  `CI_subsamples` has results for more than one type of confidence
  interval. If `"wide"` (the default), then each performance criterion
  will have a separate column for each CI type. If `"long"`, then each
  performance criterion will be a single variable, with separate rows
  for each CI type.

## Value

A tibble containing the number of simulation iterations, performance
criteria estimate(s) and the associated MCSE.

## References

Boos DD, Zhang J (2000). “Monte Carlo evaluation of resampling-based
hypothesis tests.” *Journal of the American Statistical Association*,
**95**(450), 486–492.
[doi:10.1080/01621459.2000.10474226](https://doi.org/10.1080/01621459.2000.10474226)
.

## Examples

``` r

# function to generate data from two distinct populations
dgp <- function(N_A, N_B, shape_A, scale_A, shape_B, scale_B) {
  data.frame(
    group = rep(c("A","B"), c(N_A, N_B)),
      y = c(
        rgamma(N_A, shape = shape_A, scale = scale_A),
        rgamma(N_B, shape = shape_B, scale = scale_B)
      )
  )
}

# function to do a bootstrap t-test
estimator <- function(
    dat,
    B_vals = c(49,59,89,99), # number of booties to evaluate
    pval_reps = 4L
) {
  stat <- t.test(y ~ group, data = dat)$statistic

  # create bootstrap replications under the null of no difference
  boot_dat <- dat
  booties <- replicate(max(B_vals), {
    boot_dat$group <- sample(dat$group)
    t.test(y ~ group, data = boot_dat)$statistic
  })

  # calculate multiple bootstrap p-values using sub-sampling of replicates
  res <- data.frame(stat = stat)

  res$pvalue_subsamples <- bootstrap_pvals(
    boot_stat = booties,
    stat = stat,
    B_vals = B_vals,
    reps = pval_reps,
    enlist = TRUE
  )

  res
}

# create simulation driver
simulate_boot_pvals <- bundle_sim(
  f_generate = dgp,
  f_analyze = estimator
)

# replicate the bootstrap process
x <- simulate_boot_pvals(
  reps = 50L,
  N_A = 20, N_B = 25,
  shape_A = 7, scale_A = 2,
  shape_B = 4, scale_B = 3,
  B_vals = c(49, 99, 149, 199),
  pval_reps = 2L
)

extrapolate_rejection(
  data = x,
  pvalue_subsamples = pvalue_subsamples,
  B_target = 1999,
  alpha = c(.01, .05, .10)
)
#>   K_boot_rejection bootstraps extrapolated boot_rej_rate_alpha_01
#> 1               50         49        FALSE             0.11000000
#> 2               50         99        FALSE             0.03000000
#> 3               50        149        FALSE             0.07000000
#> 4               50        199        FALSE             0.04000000
#> 5               50       1999         TRUE             0.02170504
#>   boot_rej_rate_alpha_05 boot_rej_rate_alpha_10 boot_rej_rate_mcse_alpha_01
#> 1              0.2800000              0.3900000                  0.04113194
#> 2              0.2500000              0.4300000                  0.02217739
#> 3              0.2900000              0.4200000                  0.03502186
#> 4              0.3000000              0.4000000                  0.02799417
#> 5              0.2895512              0.4233506                  0.02951073
#>   boot_rej_rate_mcse_alpha_05 boot_rej_rate_mcse_alpha_10
#> 1                  0.05917804                  0.06591584
#> 2                  0.05758756                  0.07000000
#> 3                  0.06076049                  0.07050836
#> 4                  0.06546537                  0.06998542
#> 5                  0.06766279                  0.07363925

extrapolate_rejection(
  data = x,
  pvalue_subsamples = pvalue_subsamples,
  B_target = Inf,
  alpha = c(.01, .05, .10),
  nested = TRUE
)
#>   K_boot_rejection            bootstraps                     extrapolated
#> 1               50 49, 99, 149, 199, Inf FALSE, FALSE, FALSE, FALSE, TRUE
#>                                                                                                                                                                        boot_rej_rate
#> 1 0.11000000, 0.03000000, 0.07000000, 0.04000000, 0.01967668, 0.28000000, 0.25000000, 0.29000000, 0.30000000, 0.29002613, 0.39000000, 0.43000000, 0.42000000, 0.40000000, 0.42401445
#>                                                                                                                                                                   boot_rej_rate_mcse
#> 1 0.04113194, 0.02217739, 0.03502186, 0.02799417, 0.02991865, 0.05917804, 0.05758756, 0.06076049, 0.06546537, 0.06842138, 0.06591584, 0.07000000, 0.07050836, 0.06998542, 0.07402652
```
