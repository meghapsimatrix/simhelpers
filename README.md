
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SimHelpers

<!-- badges: start -->

<!-- badges: end -->

The goal of SimHelpers is to help with running simulation studies. It
calculates performance criteria measures, Monte Carlo Standard
Errors…(under development).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("meghapsimatrix/SimHelpers")
```

## Example

This is a basic example which shows you how to solve a common problem.
We use the `welch_res` dataset included in the package. Here we are
calculating the absolute performance criteria for the estimate of the
mean difference. We present the results by method and mean difference
(conditions which were varied to generate the `welch_res` data).

``` r
library(SimHelpers)
library(tidyverse)
library(knitr)


welch_res %>%
  group_by(method, mean_diff) %>%
  do(calc_abs(.,  estimates = est, true_param = mean_diff)) %>%
  kable()
```

| method                  | mean\_diff |        bias | bias\_mcse |       var | var\_mcse |       mse | mse\_mcse |      rmse | rmse\_mcse |
| :---------------------- | ---------: | ----------: | ---------: | --------: | --------: | --------: | --------: | --------: | ---------: |
| Two Sample t-test       |        0.0 |   0.0089008 |  0.0099994 | 0.0999874 | 0.0042486 | 0.1000667 | 0.0042500 | 0.3163332 |  0.0067175 |
| Two Sample t-test       |        0.5 | \-0.0002656 |  0.0099051 | 0.0981105 | 0.0043971 | 0.0981106 | 0.0043969 | 0.3132261 |  0.0070187 |
| Two Sample t-test       |        1.0 |   0.0055522 |  0.0103429 | 0.1069763 | 0.0047303 | 0.1070071 | 0.0047342 | 0.3271194 |  0.0072362 |
| Two Sample t-test       |        2.0 | \-0.0044892 |  0.0102527 | 0.1051173 | 0.0046796 | 0.1051375 | 0.0046819 | 0.3242491 |  0.0072196 |
| Welch Two Sample t-test |        0.0 |   0.0072647 |  0.0098822 | 0.0976587 | 0.0042312 | 0.0977115 | 0.0042348 | 0.3125884 |  0.0067738 |
| Welch Two Sample t-test |        0.5 |   0.0130957 |  0.0099863 | 0.0997258 | 0.0041311 | 0.0998973 | 0.0041468 | 0.3160654 |  0.0065600 |
| Welch Two Sample t-test |        1.0 | \-0.0072504 |  0.0098140 | 0.0963144 | 0.0043984 | 0.0963669 | 0.0043896 | 0.3104303 |  0.0070702 |
| Welch Two Sample t-test |        2.0 | \-0.0052789 |  0.0099083 | 0.0981735 | 0.0044486 | 0.0982013 | 0.0044465 | 0.3133709 |  0.0070946 |

``` r

welch_res %>%
  mutate(params = mean_diff) %>%
  group_by(method, mean_diff) %>%
  group_modify(~ calc_abs(.x,  estimates = est, true_param = params)) %>%
  kable()
```

| method                  | mean\_diff |        bias | bias\_mcse |       var | var\_mcse |       mse | mse\_mcse |      rmse | rmse\_mcse |
| :---------------------- | ---------: | ----------: | ---------: | --------: | --------: | --------: | --------: | --------: | ---------: |
| Two Sample t-test       |        0.0 |   0.0089008 |  0.0099994 | 0.0999874 | 0.0042486 | 0.1000667 | 0.0042500 | 0.3163332 |  0.0067175 |
| Two Sample t-test       |        0.5 | \-0.0002656 |  0.0099051 | 0.0981105 | 0.0043971 | 0.0981106 | 0.0043969 | 0.3132261 |  0.0070187 |
| Two Sample t-test       |        1.0 |   0.0055522 |  0.0103429 | 0.1069763 | 0.0047303 | 0.1070071 | 0.0047342 | 0.3271194 |  0.0072362 |
| Two Sample t-test       |        2.0 | \-0.0044892 |  0.0102527 | 0.1051173 | 0.0046796 | 0.1051375 | 0.0046819 | 0.3242491 |  0.0072196 |
| Welch Two Sample t-test |        0.0 |   0.0072647 |  0.0098822 | 0.0976587 | 0.0042312 | 0.0977115 | 0.0042348 | 0.3125884 |  0.0067738 |
| Welch Two Sample t-test |        0.5 |   0.0130957 |  0.0099863 | 0.0997258 | 0.0041311 | 0.0998973 | 0.0041468 | 0.3160654 |  0.0065600 |
| Welch Two Sample t-test |        1.0 | \-0.0072504 |  0.0098140 | 0.0963144 | 0.0043984 | 0.0963669 | 0.0043896 | 0.3104303 |  0.0070702 |
| Welch Two Sample t-test |        2.0 | \-0.0052789 |  0.0099083 | 0.0981735 | 0.0044486 | 0.0982013 | 0.0044465 | 0.3133709 |  0.0070946 |

Below, we calculate the relative criteria for the mean difference
estimates. Note that when mean difference is 0, the relative measures
cannot be calculated. The function returns `NA` values for conditions
where the mean difference is 0.

``` r
welch_res %>%
  group_by(method, mean_diff) %>%
  do(calc_relative(., estimates = est, true_param = mean_diff)) %>%
  kable()
```

| method                  | mean\_diff | rel\_bias | rel\_bias\_mcse |  rel\_mse | rel\_mse\_mcse |
| :---------------------- | ---------: | --------: | --------------: | --------: | -------------: |
| Two Sample t-test       |        0.0 |        NA |              NA |        NA |             NA |
| Two Sample t-test       |        0.5 | 0.9994687 |       0.0198102 | 0.3924424 |      0.0021984 |
| Two Sample t-test       |        1.0 | 1.0055522 |       0.0103429 | 0.1070071 |      0.0047342 |
| Two Sample t-test       |        2.0 | 0.9977554 |       0.0051263 | 0.0262844 |      0.0093637 |
| Welch Two Sample t-test |        0.0 |        NA |              NA |        NA |             NA |
| Welch Two Sample t-test |        0.5 | 1.0261914 |       0.0199726 | 0.3995894 |      0.0020734 |
| Welch Two Sample t-test |        1.0 | 0.9927496 |       0.0098140 | 0.0963669 |      0.0043896 |
| Welch Two Sample t-test |        2.0 | 0.9973605 |       0.0049541 | 0.0245503 |      0.0088929 |

``` r

welch_res %>%
  mutate(params = mean_diff) %>%
  group_by(method, mean_diff) %>%
  group_modify(~ calc_relative(.x,  estimates = est, true_param = params)) %>%
  kable()
```

| method                  | mean\_diff | rel\_bias | rel\_bias\_mcse |  rel\_mse | rel\_mse\_mcse |
| :---------------------- | ---------: | --------: | --------------: | --------: | -------------: |
| Two Sample t-test       |        0.0 |        NA |              NA |        NA |             NA |
| Two Sample t-test       |        0.5 | 0.9994687 |       0.0198102 | 0.3924424 |      0.0021984 |
| Two Sample t-test       |        1.0 | 1.0055522 |       0.0103429 | 0.1070071 |      0.0047342 |
| Two Sample t-test       |        2.0 | 0.9977554 |       0.0051263 | 0.0262844 |      0.0093637 |
| Welch Two Sample t-test |        0.0 |        NA |              NA |        NA |             NA |
| Welch Two Sample t-test |        0.5 | 1.0261914 |       0.0199726 | 0.3995894 |      0.0020734 |
| Welch Two Sample t-test |        1.0 | 0.9927496 |       0.0098140 | 0.0963669 |      0.0043896 |
| Welch Two Sample t-test |        2.0 | 0.9973605 |       0.0049541 | 0.0245503 |      0.0088929 |

Below we calculate the rejection rate performance criteria for the
hypothesis tests done using t-test.

``` r
welch_res %>%
  group_by(method, mean_diff) %>%
  do(calc_rr(., p_values = p_val)) %>%
  kable()
```

| method                  | mean\_diff | rej\_rate | rej\_rate\_mcse |
| :---------------------- | ---------: | --------: | --------------: |
| Two Sample t-test       |        0.0 |     0.048 |       0.0067599 |
| Two Sample t-test       |        0.5 |     0.342 |       0.0150012 |
| Two Sample t-test       |        1.0 |     0.872 |       0.0105648 |
| Two Sample t-test       |        2.0 |     1.000 |       0.0000000 |
| Welch Two Sample t-test |        0.0 |     0.047 |       0.0066926 |
| Welch Two Sample t-test |        0.5 |     0.369 |       0.0152591 |
| Welch Two Sample t-test |        1.0 |     0.868 |       0.0107040 |
| Welch Two Sample t-test |        2.0 |     1.000 |       0.0000000 |

Below we calculate the coverage and width performance criteria for the
confidence intervals for the estimate of the mean difference.

``` r
welch_res %>%
  group_by(method, mean_diff) %>%
  do(calc_coverage(., lower_bound = lower_bound, upper_bound = upper_bound, true_param = mean_diff)) %>%
  kable()
```

| method                  | mean\_diff | coverage | coverage\_mcse |    width | width\_mcse |
| :---------------------- | ---------: | -------: | -------------: | -------: | ----------: |
| Two Sample t-test       |        0.0 |    0.952 |      0.0067599 | 1.246972 |   0.0033816 |
| Two Sample t-test       |        0.5 |    0.951 |      0.0068263 | 1.252593 |   0.0033538 |
| Two Sample t-test       |        1.0 |    0.943 |      0.0073315 | 1.252228 |   0.0033152 |
| Two Sample t-test       |        2.0 |    0.948 |      0.0070211 | 1.252764 |   0.0032775 |
| Welch Two Sample t-test |        0.0 |    0.953 |      0.0066926 | 1.257411 |   0.0033893 |
| Welch Two Sample t-test |        0.5 |    0.953 |      0.0066926 | 1.254026 |   0.0033805 |
| Welch Two Sample t-test |        1.0 |    0.959 |      0.0062705 | 1.258586 |   0.0033484 |
| Welch Two Sample t-test |        2.0 |    0.951 |      0.0068263 | 1.254581 |   0.0033210 |

``` r

welch_res %>%
  mutate(params = mean_diff) %>%
  group_by(method, mean_diff) %>%
  group_modify(~ calc_coverage(.x, lower_bound = lower_bound, upper_bound = upper_bound, true_param = params)) %>%
  kable()
```

| method                  | mean\_diff | coverage | coverage\_mcse |    width | width\_mcse |
| :---------------------- | ---------: | -------: | -------------: | -------: | ----------: |
| Two Sample t-test       |        0.0 |    0.952 |      0.0067599 | 1.246972 |   0.0033816 |
| Two Sample t-test       |        0.5 |    0.951 |      0.0068263 | 1.252593 |   0.0033538 |
| Two Sample t-test       |        1.0 |    0.943 |      0.0073315 | 1.252228 |   0.0033152 |
| Two Sample t-test       |        2.0 |    0.948 |      0.0070211 | 1.252764 |   0.0032775 |
| Welch Two Sample t-test |        0.0 |    0.953 |      0.0066926 | 1.257411 |   0.0033893 |
| Welch Two Sample t-test |        0.5 |    0.953 |      0.0066926 | 1.254026 |   0.0033805 |
| Welch Two Sample t-test |        1.0 |    0.959 |      0.0062705 | 1.258586 |   0.0033484 |
| Welch Two Sample t-test |        2.0 |    0.951 |      0.0068263 | 1.254581 |   0.0033210 |

# Jacknife MCSE for variance estimates

UNDER DEVELOPMENT
:)

``` r
calc_jacknife(res_dat = alpha_res, estimates = Var_A, true_param = true_param) %>%
  kable()
```

|      rbv | rbv\_mcse | rbv\_jack\_mcse |     rmsev | rmsev\_jack\_mcse |
| -------: | --------: | --------------: | --------: | ----------------: |
| 138.5863 | 0.0001563 |        11.63622 | 0.7966284 |           4.6e-06 |
