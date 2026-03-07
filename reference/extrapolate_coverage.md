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
extrapolate_coverage(
  data,
  CI_subsamples,
  true_param,
  B_target = Inf,
  exclude_above = B_target - 1L,
  criteria = c("coverage", "width"),
  winz = Inf,
  nested = FALSE,
  format = "wide",
  width_trim = 0,
  cover_na_val = NA,
  width_na_val = NA
)
```

## Arguments

- data:

  data frame or tibble containing the simulation results.

- CI_subsamples:

  list or name of column from `data` containing list of confidence
  intervals calculated based on sub-samples with different numbers of
  replications.

- true_param:

  vector or name of column from `data` containing corresponding true
  parameters.

- B_target:

  number of bootstrap replications to which the criteria should be
  extrapolated, with a default of `B = Inf`.

- exclude_above:

  numeric threshold for bootstrap replication sizes that should be
  excluded before calculating extrapolations. By default, set to
  `B_target - 1L` so that CIs based on B_target or greater replications
  will be excluded from the extrapolation calculations.

- criteria:

  character or character vector indicating the performance criteria to
  be calculated, with possible options `"coverage"` and `"width"`.

- winz:

  numeric value for winsorization constant. If set to a finite value,
  estimates will be winsorized at the constant multiple of the
  inter-quartile range below the 25th percentile or above the 75th
  percentile of the distribution. For instance, setting `winz = 3` will
  truncate estimates that fall below P25 - 3 \* IQR or above P75 + 3 \*
  IQR.

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

- width_trim:

  numeric value specifying the trimming percentage to use when
  summarizing CI widths across replications from a single set of
  bootstraps, with a default of 0.0 (i.e., use the regular arithmetic
  mean).

- cover_na_val:

  numeric value to use for calculating coverage if bootstrap CI
  end-points are missing. Default is `NA`.

- width_na_val:

  numeric value to use for calculating width if bootstrap CI end-points
  are missing. Default is `NA`.

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
dgp <- function(N, mu, nu) {
  mu + rt(N, df = nu)
}

estimator <- function(
   dat,
    B_vals = c(49,59,89,99),
    m = 4,
    trim = 0.1
) {


  # compute estimate and standard error
  N <- length(dat)
  est <- mean(dat, trim = trim)
  se <- sd(dat) / sqrt(N)

  # compute booties
  booties <- replicate(max(B_vals), {
    x <- sample(dat, size = N, replace = TRUE)
    data.frame(
      M = mean(x, trim = trim),
      SE = sd(x) / sqrt(N)
    )
  }, simplify = FALSE) |>
    dplyr::bind_rows()

  # confidence intervals for each B_vals
  CIs <- bootstrap_CIs(
    boot_est = booties$M,
    boot_se = booties$SE,
    est = est,
    se = se,
    CI_type = c("normal","basic","student","percentile"),
    B_vals = B_vals,
    reps = m,
    format = "wide-list"
  )

  res <- data.frame(
    est = est,
    se = se
  )
  res$CIs <- CIs

  res
}

#' build a simulation driver function
simulate_bootCIs <- bundle_sim(
  f_generate = dgp,
  f_analyze = estimator
)

boot_results <- simulate_bootCIs(
  reps = 50, N = 20, mu = 2, nu = 3,
  B_vals = seq(49, 199, 50),
)

extrapolate_coverage(
  data = boot_results,
  CI_subsamples = CIs,
  true_param = 2
)
#>      K_boot_coverage bootstraps extrapolated boot_coverage_normal
#> 49                50         49        FALSE            0.9400000
#> 99                50         99        FALSE            0.9650000
#> 149               50        149        FALSE            0.9650000
#> 199               50        199        FALSE            0.9600000
#> Inf*              50        Inf         TRUE            0.9736769
#>      boot_coverage_basic boot_coverage_student boot_coverage_percentile
#> 49             0.9050000              0.925000                 0.895000
#> 99             0.9500000              0.970000                 0.930000
#> 149            0.9500000              0.975000                 0.940000
#> 199            0.9600000              0.980000                 0.960000
#> Inf*           0.9781568              1.000832                 0.971915
#>      boot_coverage_mcse_normal boot_coverage_mcse_basic
#> 49                  0.03159049               0.03703969
#> 99                  0.02259899               0.02945075
#> 149                 0.02475389               0.02945075
#> 199                 0.02799417               0.02799417
#> Inf*                0.02545305               0.02974339
#>      boot_coverage_mcse_student boot_coverage_mcse_percentile boot_width_normal
#> 49                   0.02879378                    0.03648455          1.161037
#> 99                   0.02099563                    0.02948538          1.176419
#> 149                  0.02051630                    0.03077237          1.173476
#> 199                  0.02000000                    0.02799417          1.174070
#> Inf*                 0.02210396                    0.03133116          1.180663
#>      boot_width_basic boot_width_student boot_width_percentile
#> 49           1.099122           1.209360              1.099122
#> 99           1.181889           1.295052              1.181889
#> 149          1.192432           1.309590              1.192432
#> 199          1.206777           1.327062              1.206777
#> Inf*         1.243604           1.365006              1.243604
#>      boot_width_mcse_normal boot_width_mcse_basic boot_width_mcse_student
#> 49               0.04262056            0.04119083              0.05537994
#> 99               0.04139945            0.04393271              0.05322824
#> 149              0.04095786            0.04256404              0.05186126
#> 199              0.04078999            0.04416288              0.05192157
#> Inf*             0.04082743            0.04573544              0.05174618
#>      boot_width_mcse_percentile
#> 49                   0.04119083
#> 99                   0.04393271
#> 149                  0.04256404
#> 199                  0.04416288
#> Inf*                 0.04573544

extrapolate_coverage(
  data = boot_results,
  CI_subsamples = CIs,
  true_param = 2,
  B_target = 999,
  format = "long"
)
#>    K_boot_coverage bootstraps extrapolated    CI_type boot_coverage
#> 1               50         49        FALSE     normal     0.9400000
#> 2               50         99        FALSE     normal     0.9650000
#> 3               50        149        FALSE     normal     0.9650000
#> 4               50        199        FALSE     normal     0.9600000
#> 5               50        999         TRUE     normal     0.9721437
#> 6               50         49        FALSE      basic     0.9050000
#> 7               50         99        FALSE      basic     0.9500000
#> 8               50        149        FALSE      basic     0.9500000
#> 9               50        199        FALSE      basic     0.9600000
#> 10              50        999         TRUE      basic     0.9746588
#> 11              50         49        FALSE    student     0.9250000
#> 12              50         99        FALSE    student     0.9700000
#> 13              50        149        FALSE    student     0.9750000
#> 14              50        199        FALSE    student     0.9800000
#> 15              50        999         TRUE    student     0.9971990
#> 16              50         49        FALSE percentile     0.8950000
#> 17              50         99        FALSE percentile     0.9300000
#> 18              50        149        FALSE percentile     0.9400000
#> 19              50        199        FALSE percentile     0.9600000
#> 20              50        999         TRUE percentile     0.9680608
#>    boot_coverage_mcse boot_width boot_width_mcse
#> 1          0.03159049   1.161037      0.04262056
#> 2          0.02259899   1.176419      0.04139945
#> 3          0.02475389   1.173476      0.04095786
#> 4          0.02799417   1.174070      0.04078999
#> 5          0.02527111   1.179771      0.04081420
#> 6          0.03703969   1.099122      0.04119083
#> 7          0.02945075   1.181889      0.04393271
#> 8          0.02945075   1.192432      0.04256404
#> 9          0.02799417   1.206777      0.04416288
#> 10         0.02930965   1.236633      0.04535434
#> 11         0.02879378   1.209360      0.05537994
#> 12         0.02099563   1.295052      0.05322824
#> 13         0.02051630   1.309590      0.05186126
#> 14         0.02000000   1.327062      0.05192157
#> 15         0.02153827   1.357448      0.05173691
#> 16         0.03648455   1.099122      0.04119083
#> 17         0.02948538   1.181889      0.04393271
#> 18         0.03077237   1.192432      0.04256404
#> 19         0.02799417   1.206777      0.04416288
#> 20         0.03052684   1.236633      0.04535434
```
