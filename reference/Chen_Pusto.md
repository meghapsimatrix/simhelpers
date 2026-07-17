# Simulation results from Chen & Pustejovsky (2025)

A dataset containing a subset of results from a simulation study
examining the performance of methods to correct for publication bias in
meta-analyses that involve dependent effect sizes.

## Usage

``` r
Chen_Pusto
```

## Format

A tibble with 10,368 rows and 22 variables:

- k:

  Parameter setting for the number of studies included in each
  meta-analysis.

- mu:

  Parameter setting for the average effect size across studies.

- tau:

  Parameter setting for the between-study standard deviation of the
  effect size distribution.

- cor_mu:

  Parameter setting for the correlation between outcomes measured within
  the same study.

- wts:

  Parameter setting for the selection weight, which controls the
  probability that a non-affirmative result is reported.

- iterations:

  Number of simulation iterations per condition.

- method:

  Estimation method applied to each simulated dataset.

- n_converged:

  Number of simulation iterations in which the estimation method
  converged.

- bias:

  Bias of the estimator for the average effect size (mu)

- bias_mcse:

  Monte Carlo standard error of bias.

- var:

  Variance of the estimator method for the average effect size (mu)

- var_mcse:

  Monte Carlo standard error of variance.

- mse:

  Mean squared error of the estimator for the average effect size (mu)

- mse_mcse:

  Monte Carlo standard error of mean squared error.

- rmse:

  Root mean squared error of the estimator for the average effect size
  (mu)

- rmse_mcse:

  Monte Carlo standard error of root mean squared error.

- coverage:

  Coverage level of the 95% confidence interval for the average effect
  size (mu).

- coverage_mcse:

  Monte Carlo standard error of the coverage level.

- width:

  Average width of the 95% confidence interval for the average effect
  size (mu)

- width_mcse:

  Monte Carlo standard error of the average width.

- rej_rate:

  Rejection rate of a hypothesis test that the average effect size (mu)
  is equal to zero.

- rej_rate_mcse:

  Monte Carlo standard error of the rejection rate.

## Source

Chen M, Pustejovsky JE (2025). “Adapting Methods for Correcting
Selective Reporting Bias in Meta-Analysis of Dependent Effect Sizes.”
*Psychological Methods*, Advance online publication.
[doi:10.1037/met0000773](https://doi.org/10.1037/met0000773) .

## Details

This dataset contains only a subset of the results from the simulation
study reported in Chen and Pustejovsky (2025). The simulation followed a
full factorial design involving 4 levels for `k`, 4 levels for `mu`, 4
levels for `tau`, 3 levels for `cor_mu`, and 6 levels for `wts`, for a
total of 1152 unique conditions. For each condition, the dataset
includes performance measures for each of 9 estimation methods:

- `"3PSM"`: a three-parameter step function selection model, with a step
  at \\\alpha = .025\\, ignoring the presence of dependent effect sizes

- `"4PSM"`: a four-parameter step function selection model, with steps
  at \\\alpha = .025, .500\\, ignoring the presence of dependent effect
  sizes

- `"CHE-ISCW"`: a summary meta-analysis using the
  correlated-and-heirarchical effects working model with inverse
  sampling-covariance weighting

- `"EK"`: a multivariate version of the endogenous kink meta-regression

- `"PET-PEESE"`: a multivariate version of PET-PEESE meta-regression
  (i.e., a limit meta-regression)

- `"TF"`: Trim-and-Fill, ignoring the presence of dependent effect sizes

- `"WAAP"`: a multivariate version of the weighted average of adequately
  powered studies

- `"WILS"`: a multivariate version of the weighted-and-iterated least
  squares method, stopping at a minimum of \\k = 5\\ studies

- `"p-uniform*"`: the p-uniform\* estimator, ignoring the presence of
  dependent effect sizes
