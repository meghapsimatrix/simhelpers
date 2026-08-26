# Simulation Performance Criteria and MCSE

Monte Carlo simulations are computer experiments designed to study the
performance of statistical methods under known data-generating
conditions (Morris, White, & Crowther, 2019). Methodologists use
simulations to examine questions such as: (1) how does ordinary least
squares (OLS) regression perform if errors are heteroskedastic? (2) how
does the presence of missing data affect treatment effect estimates from
a propensity score analysis? (3) how does cluster robust variance
estimation perform when the number of clusters is small? To answer such
questions, we conduct experiments by simulating thousands of datasets
based on pseudo-random sampling (Morris et al., 2019). We generate
datasets under known conditions. For example, we can generate data where
the errors are heteroskedastic or there is missing data present
following Missing at Random (MAR) mechanism. Then, we apply statistical
methods, like OLS or propensity score estimation using logistic
regression, to each of the datasets and extract results like regression
coefficients, p-values, and confidence intervals. We then analyze the
performance of these estimation methods using criteria such as bias,
Type 1 error rate, or root mean squared error. Performance measures from
simulation studies are estimates, based on a finite number of repeated
samples from a data-generating process, and thus include some random
error. When interpreting performance measures, it is important to
consider how large the estimation error is. This is typically quantified
as Monte Carlo standard error (MCSE).

The goal of `simhelpers` is to assist in running simulation studies.
This package provides a set of functions that calculate various
performance measures and associated MCSE. This vignette focuses on the
set of functions for calculating performance measures. Below, we explain
three major categories of performance criteria: (1) absolute criteria,
(2) relative criteria, and (3) criteria to evaluate hypothesis testing.
We provide formulas used to calculate the performance measures and MCSE
and demonstrate how to use the functions in the `simhelpers` package.

The notation and explanations of the performance measures largely follow
notes from Dr. James Pustejovsky’s [Data Analysis, Simulation and
Programming in R course (Spring,
2019)](https://jepusto.com/teaching/daspir/).

## Performance Criteria and MCSE

### Absolute Criteria

Bias characterizes whether an estimator tends to lie above or below the
true parameter, on average. Variance characterizes the precision of an
estimator, as the average squared deviation of the estimator from its
average. Note that variance is the inverse of precision. Therefore, the
higher the variance, the lower the precision. Standard error, also
characterizing precision, is the square root of variance. Mean squared
error (MSE) and root mean squared error (RMSE) characterize the accuracy
of the estimates. MSE and RMSE measure how far off, on average, an
estimator is from the true parameter. Absolute criteria are in the same
scale as the estimate. MSE is in squared deviation scale and RMSE is in
the scale of the estimates.

Let $`T`$ denote an estimator for a parameter $`\theta`$. By running a
simulation study, we obtain $`K`$ estimates $`T_1,...,T_K`$ (or
realizations of the estimator) for each data generating condition. We
can calculate the following sample statistics for the estimates:

- Sample mean: $`\bar{T} = \frac{1}{K}\sum_{k=1}^K T_k`$
- Sample variance:
  $`S_T^2 = \frac{1}{K - 1}\sum_{k=1}^K \left(T_k - \bar{T}\right)^2`$
- Sample skewness (standardized):
  $`g_T = \frac{1}{K S_T^3}\sum_{k=1}^K \left(T_k - \bar{T}\right)^3`$
- Sample kurtosis (standardized):
  $`k_T = \frac{1}{K S_T^4} \sum_{k=1}^K \left(T_k - \bar{T}\right)^4`$

Table 1 below shows each of the performance criteria, its
interpretation, its formal definition, how the criterion is estimated in
a simulation study, and the formula for the MCSE of the estimated
performance measure.

| Criterion | Measure | Definition | Estimate | MCSE |
|:---|:---|:---|:---|:---|
| Bias | Difference from true parameter | $`\text{E}(T) - \theta`$ | $`\bar{T} - \theta`$ | $`\sqrt{S_T^2/ K}`$ |
| Variance | Precision | $`\text{E}\left[(T - \text{E}(T))^2\right]`$ | $`S_T^2`$ | $`S_T^2 \sqrt{\frac{k_T - 1}{K}}`$ |
| Standard Error | Precision | $`\sqrt{\text{E}\left[(T - \text{E}(T))^2\right]}`$ | $`S_T`$ | $`\sqrt{\frac{K - 1}{K} \sum_{j=1}^K (\sqrt{S_{T(j)}^2} - S_T)^2 }`$ |
| MSE | Accuracy | $`\text{E}\left[(T - \theta)^2\right]`$ | $`\frac{1}{K}\sum_{k=1}^{K}\left(T_k - \theta\right)^2`$ | $`\sqrt{\frac{1}{K}\left[S_T^4 (k_T - 1) + 4 S_T^3 g_T(\bar{T} - \theta) + 4 S_T^2 (\bar{T} - \theta)^2\right]}`$ |
| RMSE | Accuracy | $`\sqrt{\text{E}\left[(T - \theta)^2\right]}`$ | $`\sqrt{\frac{1}{K}\sum_{k=1}^{K}\left(T_k - \theta\right)^2}`$ | $`\sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(RMSE_{(j)} - RMSE\right)^2}`$ |

Table 1. Absolute Performance Criteria {.table .table .table-striped
.table-hover style="margin-left: auto; margin-right: auto;"}

The equation for the MCSE of the standard deviation is derived using the
jack-knife technique (Efron & Stein, 1981). First we calculate variance
of the estimates leaving out replicate $`j`$. Instead of calculating
each jack-knife estimate, we use algebraic tricks to calculate
$`S^2_{T(j)}`$ as follows:

``` math
S_{T(j)}^2 = \frac{1}{K - 2} \left[(K - 1) S_T^2 - \frac{K}{K - 1}\left(T_j - \bar{T}\right)^2\right]
```
Then, the jack-knife MCSE of standard deviation will be calculated as:

``` math
\sqrt{\frac{K - 1}{K} \sum_{j=1}^K (\sqrt{S_{T(j)}^2} - S_T)^2 }
```

The equation for the MCSE of RMSE is also derived using the jack-knife
technique, which involves excluding a replicate $`j`$ and calculating
RMSE (Efron & Stein, 1981). The formula for RMSE is:

``` math
\sqrt{\frac{1}{K}\sum_{k=1}^{K}\left(T_k - \theta\right)^2}
```

This is approximately equivalent to:

``` math
RMSE = \sqrt{(\bar{T} - \theta)^2 + S^2_T}
```

The jack-knife RMSE will be calculated as:

``` math
RMSE_{(j)}  = \sqrt{(\bar{T}_{(j)} - \theta)^2 + S^2_{T(j)}}
```

Here $`\bar{T}_{(j)}`$ and $`S^2_{T(j)}`$ indicate the mean and variance
of the estimates leaving out replicate $`j`$. Instead of calculating
each jack-knife estimate, we use algebraic tricks to calculate
$`\bar{T}_{(j)}`$ and $`S^2_{T(j)}`$ as follows:

``` math
\bar{T}_{(j)} = \frac{1}{K-1} \left(K \bar{T} - T_j\right)
```

``` math
S_{T(j)}^2 = \frac{1}{K - 2} \left[(K - 1) S_T^2 - \frac{K}{K - 1}\left(T_j - \bar{T}\right)^2\right]
```

Then, the jack-knife MCSE of RMSE will be calculated as:

``` math
MCSE_{RMSE(JK)}  = \sqrt{\frac{K -1}{K}\sum_{j=1}^K  \left(RMSE_{(j)} - RMSE\right)^2}
```

#### Example

We use the `welch_res` dataset included in the package. It contains the
results from an example simulation study comparing the
heteroskedasticity-robust Welch t-test to the usual two-sample t-test
assuming equal variances. The code used to generate the data and derive
results can be found
[here](https://github.com/meghapsimatrix/simhelpers/blob/master/data-raw/welch_res_dat.R).
We varied the sample sizes per group. We set the sample size of Group 1
to 50 and we varied sample size of Group 2 to be 50 and 70. We varied
the mean difference parameter when generating data for two groups. We
set the values to 0, 0.5, 1 and 2. We generated the data with slightly
unequal variances. With each simulated dataset, we ran the usual t-test,
which assumes homogeneity of variance, and we also ran Welch t-test,
which does not assume homogeneity of variance. We extracted the
estimates (mean differences), variances of the estimates, p-values, and
the lower and upper bounds of the confidence intervals.

``` r

library(simhelpers)
library(dplyr)
library(tibble)
library(knitr)
library(dplyr)
library(kableExtra)

welch_res %>%
  glimpse()
#> Rows: 16,000
#> Columns: 11
#> $ n1          <dbl> 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50…
#> $ n2          <dbl> 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50…
#> $ mean_diff   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ iterations  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000…
#> $ seed        <dbl> 204809087, 204809087, 204809087, 204809087, 204809087, 204…
#> $ method      <chr> "t-test", "Welch t-test", "t-test", "Welch t-test", "t-tes…
#> $ est         <dbl> 0.025836000, 0.025836000, 0.005158587, 0.005158587, -0.079…
#> $ var         <dbl> 0.09543914, 0.09543914, 0.08481717, 0.08481717, 0.08179330…
#> $ p_val       <dbl> 0.9335212, 0.9335804, 0.9859039, 0.9859109, 0.7807543, 0.7…
#> $ lower_bound <dbl> -0.5872300, -0.5899041, -0.5727856, -0.5741984, -0.6473703…
#> $ upper_bound <dbl> 0.6389020, 0.6415761, 0.5831027, 0.5845155, 0.4877263, 0.4…
```

Below, we calculate the absolute performance criteria for the estimates
of the mean differences. We present the results by sample sizes, mean
difference, and the t-test method. The
[`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md)
function is designed to work with the
[`tidyeval`](https://www.tidyverse.org/) workflow (Wickham et al.,
2019). The first argument, `res_dat`, requires a data frame or a tibble
containing the results from a simulation study. The second argument,
`estimates`, requires the name of the column containing the estimates
like mean difference or regression coefficients. The third argument,
`true_param`, requires the name of the column containing the true
parameters. The fourth argument, `perfm_criteria`, lets the user specify
which criteria to evaluate. The criteria can be specified using a
character or character vector. If the user only wants bias, they can
specify `perfm_criteria = "bias"`. If the user wants bias and root mean
squared error, they can specify `perfm_criteria = c("bias", "rmse")`. By
default, the function returns bias, variance, standard error, mean
squared error (mse), and root mean squared error (rmse),
`perfm_criteria = c("bias", "variance", "stddev", "mse", "rmse")`. In
the example below, we ask for all the available criteria.

We calculate the absolute performance measures only for the conventional
t-test results because the mean difference is identical for the Welch
t-test. We use [`dplyr`](https://dplyr.tidyverse.org/index.html) syntax
to group by the sample sizes and mean difference that were used to
generate the data. We provide examples using
[`do()`](https://dplyr.tidyverse.org/reference/do.html) and
[`group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html)
to run
[`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md)
on results for each combination of the conditions. Results are rounded
to five decimal places.

If there are any convergence issues with estimation, please make sure to
[write your estimation function to output `NA` for the values of the
point estimates, variance estimates, p-values, or confidence interval
estimates](https://meghapsimatrix.github.io/simhelpers/articles/simulation_workflow.html#convergence-issues).
The functions in the `simhelpers` package will calculate performance
criteria and MCSE after deleting any missing estimates and will output
the number of iterations, `K`, which will exclude iterations with `NA`
values for estimates. Note that `K` may differ by condition.

``` r

# using do()
welch_res %>%
  filter(method == "t-test") %>% # filter just conventional t-test res
  group_by(n1, n2, mean_diff) %>% # grouping 
  do(calc_absolute(., estimates = est, true_param = mean_diff)) %>% # run the function
  kable(digits = 5) # create a kable table 
```

| n1 | n2 | mean_diff | K_absolute | bias | bias_mcse | var | var_mcse | stddev | stddev_mcse | mse | mse_mcse | rmse | rmse_mcse |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 50 | 50 | 0.0 | 1000 | -0.00890 | 0.01000 | 0.09999 | 0.00425 | 0.31621 | 0.00674 | 0.09997 | 0.00425 | 0.31618 | 0.00839 |
| 50 | 50 | 0.5 | 1000 | -0.00555 | 0.01034 | 0.10698 | 0.00473 | 0.32707 | 0.00726 | 0.10690 | 0.00473 | 0.32696 | 0.00891 |
| 50 | 50 | 1.0 | 1000 | -0.00726 | 0.00988 | 0.09766 | 0.00423 | 0.31250 | 0.00680 | 0.09761 | 0.00423 | 0.31243 | 0.00840 |
| 50 | 50 | 2.0 | 1000 | 0.00725 | 0.00981 | 0.09631 | 0.00440 | 0.31035 | 0.00712 | 0.09627 | 0.00439 | 0.31028 | 0.00862 |
| 50 | 70 | 0.0 | 1000 | -0.00266 | 0.00867 | 0.07521 | 0.00330 | 0.27424 | 0.00605 | 0.07514 | 0.00331 | 0.27411 | 0.00744 |
| 50 | 70 | 0.5 | 1000 | -0.00514 | 0.00895 | 0.08002 | 0.00391 | 0.28288 | 0.00694 | 0.07997 | 0.00391 | 0.28278 | 0.00826 |
| 50 | 70 | 1.0 | 1000 | -0.01124 | 0.00899 | 0.08081 | 0.00358 | 0.28427 | 0.00632 | 0.08086 | 0.00358 | 0.28435 | 0.00775 |
| 50 | 70 | 2.0 | 1000 | -0.00079 | 0.00883 | 0.07805 | 0.00345 | 0.27938 | 0.00619 | 0.07798 | 0.00345 | 0.27924 | 0.00760 |

``` r

# using group_modify()
welch_res %>%
  filter(method == "t-test") %>% # filter just conventional t-test res
  mutate(params = mean_diff) %>% # group_modify cannot take in a group column as an argument
  group_by(n1, n2, mean_diff) %>% # grouping 
  group_modify(~ calc_absolute(.x, estimates = est, true_param = params)) %>%
  kable(digits = 5)
```

| n1 | n2 | mean_diff | K_absolute | bias | bias_mcse | var | var_mcse | stddev | stddev_mcse | mse | mse_mcse | rmse | rmse_mcse |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 50 | 50 | 0.0 | 1000 | -0.00890 | 0.01000 | 0.09999 | 0.00425 | 0.31621 | 0.00674 | 0.09997 | 0.00425 | 0.31618 | 0.00839 |
| 50 | 50 | 0.5 | 1000 | -0.00555 | 0.01034 | 0.10698 | 0.00473 | 0.32707 | 0.00726 | 0.10690 | 0.00473 | 0.32696 | 0.00891 |
| 50 | 50 | 1.0 | 1000 | -0.00726 | 0.00988 | 0.09766 | 0.00423 | 0.31250 | 0.00680 | 0.09761 | 0.00423 | 0.31243 | 0.00840 |
| 50 | 50 | 2.0 | 1000 | 0.00725 | 0.00981 | 0.09631 | 0.00440 | 0.31035 | 0.00712 | 0.09627 | 0.00439 | 0.31028 | 0.00862 |
| 50 | 70 | 0.0 | 1000 | -0.00266 | 0.00867 | 0.07521 | 0.00330 | 0.27424 | 0.00605 | 0.07514 | 0.00331 | 0.27411 | 0.00744 |
| 50 | 70 | 0.5 | 1000 | -0.00514 | 0.00895 | 0.08002 | 0.00391 | 0.28288 | 0.00694 | 0.07997 | 0.00391 | 0.28278 | 0.00826 |
| 50 | 70 | 1.0 | 1000 | -0.01124 | 0.00899 | 0.08081 | 0.00358 | 0.28427 | 0.00632 | 0.08086 | 0.00358 | 0.28435 | 0.00775 |
| 50 | 70 | 2.0 | 1000 | -0.00079 | 0.00883 | 0.07805 | 0.00345 | 0.27938 | 0.00619 | 0.07798 | 0.00345 | 0.27924 | 0.00760 |

### Relative Criteria

Relative criteria can be useful for describing an estimator’s
performance, especially if the performance varies in proportion to the
true value of the target parameter. It can be only used when
$`|\theta| > 0`$ as we cannot divide by $`0`$(Morris et al., 2019).

To derive the MCSE for relative RMSE, we again used the jack-knife
technique. The formula to calculate relative RMSE is:

``` math
rRMSE = \sqrt{\frac{(\bar{T} - \theta)^2 + S_T^2}{\theta^2}}
```

The jack-knife RMSE will be calculated as:

``` math
rRMSE_{(j)}  = \sqrt{\frac{(\bar{T}_{(j)} - \theta)^2 + S_{T(j)}^2}{\theta^2}}
```

Here $`\bar{T}_{(j)}`$ and $`S^2_{T(j)}`$ are calculated as specified
above when we described the algebra trick to estimate these values. The
MCSE is then calculated as specified in the table below.

Table 2 below shows each of the relative performance criteria, its
interpretation, its formal definition, how the criterion is estimated in
a simulation study, and its MCSE formula.

| Criterion | Measure | Definition | Estimate | MCSE |
|:---|:---|:---|:---|:---|
| Relative Bias | Relative difference from true parameter | $`\text{E}(T) / \theta`$ | $`\bar{T} / \theta`$ | $`\sqrt{S_T^2 / (K\theta^2)}`$ |
| Relative MSE | Accuracy | $`\text{E}\left[(T - \theta)^2\right]/ \theta^2`$ | $`\frac{(\bar{T} - \theta)^2 + S_T^2}{\theta^2}`$ | $`\sqrt{\frac{1}{K\theta^4}\left[S_T^4 (k_T - 1) + 4 S_T^3 g_T(\bar{T} - \theta) + 4 S_T^2 (\bar{T} - \theta)^2\right]}`$ |
| Relative RMSE | Accuracy | $`\sqrt{\text{E}\left[(T - \theta)^2\right]/ \theta^2}`$ | $`\sqrt{\frac{(\bar{T} - \theta)^2 + S_T^2}{\theta^2}}`$ | $`\sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(rRMSE_{(j)} - rRMSE)^2\right)}`$ |

Table 2. Relative Performance Criteria {.table .table .table-striped
.table-hover style="margin-left: auto; margin-right: auto;"}

#### Example

Below, we calculate the relative criteria for the mean difference
estimates. Note that when the mean difference is 0, the relative
measures cannot be calculated and the function returns `NA` values. The
syntax for
[`calc_relative()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative.md)
is similar to the one that we used earlier for
[`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md).
The `perfm_criteria` argument allows the user to specify which criteria
to evaluate:
`perfm_criteria = c("relative bias", "relative mse", "relative rmse")`.

``` r

# using group_modify()
welch_res %>%
  filter(method == "t-test") %>%
  mutate(params = mean_diff) %>%
  group_by(n1, n2, mean_diff) %>%
  group_modify(~ calc_relative(.x, estimates = est, true_param = params)) %>%
  kable(digits = 5)
```

| n1 | n2 | mean_diff | K_relative | rel_bias | rel_bias_mcse | rel_mse | rel_mse_mcse | rel_rmse | rel_rmse_mcse |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 50 | 50 | 0.0 | 1000 | NA | NA | NA | NA | NA | NA |
| 50 | 50 | 0.5 | 1000 | 0.98890 | 0.02069 | 0.42760 | 0.01894 | 0.65391 | 0.01782 |
| 50 | 50 | 1.0 | 1000 | 0.99274 | 0.00988 | 0.09761 | 0.00423 | 0.31243 | 0.00840 |
| 50 | 50 | 2.0 | 1000 | 1.00363 | 0.00491 | 0.02407 | 0.00110 | 0.15514 | 0.00431 |
| 50 | 70 | 0.0 | 1000 | NA | NA | NA | NA | NA | NA |
| 50 | 70 | 0.5 | 1000 | 0.98972 | 0.01789 | 0.31986 | 0.01566 | 0.56557 | 0.01652 |
| 50 | 70 | 1.0 | 1000 | 0.98876 | 0.00899 | 0.08086 | 0.00358 | 0.28435 | 0.00775 |
| 50 | 70 | 2.0 | 1000 | 0.99961 | 0.00442 | 0.01949 | 0.00086 | 0.13962 | 0.00380 |

### Relative Criteria for Variance Estimators

Variance estimators are always positive, and so relative criteria are
often used to characterize their performance. For variance estimators,
we have $`V`$ denoting the sampling variance of a point estimator $`T`$.
To assess the relative criteria for $`V`$, we need to divide it by the
true value of the sampling variance of $`T`$,
$`\lambda = \text{Var}(T)`$, which we may not be able to calculate
directly. In such scenario, we can use the sample variance of $`T`$
across the replications, $`S_T^2`$, to estimate the true sampling
variance.

The relative bias would then be estimated by $`rB = \bar{V} / S_T^2`$,
the average of the variance estimates divided by the sample variance of
the point estimates. To estimate MCSE of the relative bias of the
variance estimator, we need to account for the uncertainty in the
estimation of the true sampling variance. One way to do so is to use the
jack-knife technique that we described above, which entails excluding a
replicate $`j`$ and calculating relative bias
$`\bar{V}_{(j)}/ S_{T(j)}^2`$. The Monte Carlo standard error can then
be calculated as:

``` math
MCSE\left(rB\right) = \sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(rB_{(j)} - rB\right)^2}
```
which can be written as:

``` math
MCSE\left(rB\right) = \sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(\frac{\bar{V}_{(j)}}{S_{T(j)}^2} - \frac{\bar{V}}{S_T^2}\right)^2}
```

We reformulate the MCSE using some algebra tricks similar to how we
reformulated them for the RMSE MCSE formulas above.

``` math
\begin{aligned}
\bar{V}_{(j)} &= \frac{1}{K - 1}\left(K \bar{V} - V_j\right) \\
S_{T(j)}^2 &= \frac{1}{K - 2} \left[(K - 1) S_T^2 - \frac{K}{K - 1}\left(T_j - \bar{T}\right)^2\right]
\end{aligned}
```

Similarly, we can estimate the MCSE of relative MSE and RMSE using the
jack-knife technique. To estimate the MSE we need to estimate
$`S_{V(j)}^2`$, which represents the sample variance of the variance
estimates leaving replicate $`j`$ out. We calculate $`S_{V(j)}^2`$ as

``` math
S_{V(j)}^2 = \frac{1}{K - 2} \left[(K - 1) S_V^2 - \frac{K}{K - 1}\left(V_j - \bar{V}\right)^2\right].
```

We can then estimate the jack-knife relative MSE and RMSE following the
same logic as we described above and then calculate the MCSE.

Table 3 below lists relative performance measures for variance
estimators.

| Criterion | Measure | Definition | Estimate | MCSE |
|:---|:---|:---|:---|:---|
| Relative Bias | Relative difference from true parameter | $`\text{E}(V) / \lambda`$ | $`\bar{V} / S_T^2`$ | $`\sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(rB_{(j)} - rB\right)^2}`$ |
| Relative MSE | Accuracy | $`\text{E}\left[(V - \lambda)^2\right]/ \lambda^2`$ | $`\frac{(\bar{V} - S_T^2)^2 + S_V^2}{S_T^4}`$ | $`\sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(rMSE_{(j)} - rMSE\right)^2}`$ |
| Relative RMSE | Accuracy | $`\sqrt{\text{E}\left[(V - \lambda)^2\right]/ \lambda^2}`$ | $`\sqrt{\frac{(\bar{V} - S_T^2)^2 + S_V^2}{S_T^4}}`$ | $`\sqrt{\frac{K - 1}{K} \sum_{j=1}^K \left(rRMSE_{(j)} - rRMSE\right)^2}`$ |

Table 3. Relative Performance Criteria for Variance Estimators {.table
.table .table-striped .table-hover
style="margin-left: auto; margin-right: auto;"}

The function
[`calc_relative_var()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative_var.md)
calculates the relative performance criteria estimates and corresponding
jack-knife MCSEs for a variance estimator. The function requires
`res_dat`, a data frame or tibble containing simulation results,
`estimates`, the name of the column containing point estimates,
`var_estimates`, the name of the column containing the variance
estimates, and `perfm_criteria`, the criteria to be evaluated:
`perfm_criteria = c("relative bias", "relative mse", "relative rmse")`.

Below we demonstrate the use of the
[`calc_relative_var()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative_var.md)
function. The variance estimates are expected to be different when
derived from the conventional t-test as opposed to when they are derived
from the Welch t-test. We group by the sample sizes, mean difference,
and the t-test method. Note the difference between the conventional
t-test and the Welch t-test results, especially when the group sample
sizes are unequal.

If there are any convergence issues with estimation, please again make
sure that the estimation function returns `NA` values for the variance
estimate and the point estimate. The
[`calc_relative_var()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative_var.md)
function will omit the pair of point estimate and variance estimate if
either value is `NA` before calculating the performance measure and
MCSE.

#### Example

``` r

welch_res %>%
  group_by(n1, n2, mean_diff, method) %>%
  group_modify(~ calc_relative_var(.x, estimates = est, var_estimates = var)) %>%
  kable(digits = 5)
```

| n1 | n2 | mean_diff | method | K_relvar | rel_bias_var | rel_bias_var_mcse | rel_mse_var | rel_mse_var_mcse | rel_rmse_var | rel_rmse_var_mcse |
|---:|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|
| 50 | 50 | 0.0 | Welch t-test | 1000 | 0.99449 | 0.04264 | 0.02941 | 0.00259 | 0.17148 | 0.00756 |
| 50 | 50 | 0.0 | t-test | 1000 | 0.99449 | 0.04264 | 0.02941 | 0.00259 | 0.17148 | 0.00756 |
| 50 | 50 | 0.5 | Welch t-test | 1000 | 0.93705 | 0.04202 | 0.02839 | 0.00313 | 0.16849 | 0.00929 |
| 50 | 50 | 0.5 | t-test | 1000 | 0.93705 | 0.04202 | 0.02839 | 0.00313 | 0.16849 | 0.00929 |
| 50 | 50 | 1.0 | Welch t-test | 1000 | 1.02571 | 0.04508 | 0.03103 | 0.00534 | 0.17616 | 0.01512 |
| 50 | 50 | 1.0 | t-test | 1000 | 1.02571 | 0.04508 | 0.03103 | 0.00534 | 0.17616 | 0.01512 |
| 50 | 50 | 2.0 | Welch t-test | 1000 | 1.04180 | 0.04785 | 0.03211 | 0.00717 | 0.17918 | 0.01990 |
| 50 | 50 | 2.0 | t-test | 1000 | 1.04180 | 0.04785 | 0.03211 | 0.00717 | 0.17918 | 0.01990 |
| 50 | 70 | 0.0 | Welch t-test | 1000 | 1.02720 | 0.04555 | 0.02079 | 0.00452 | 0.14420 | 0.01561 |
| 50 | 70 | 0.0 | t-test | 1000 | 1.25701 | 0.05583 | 0.10150 | 0.03217 | 0.31859 | 0.05013 |
| 50 | 70 | 0.5 | Welch t-test | 1000 | 0.96512 | 0.04765 | 0.01795 | 0.00157 | 0.13398 | 0.00586 |
| 50 | 70 | 0.5 | t-test | 1000 | 1.18047 | 0.05831 | 0.06131 | 0.02440 | 0.24760 | 0.04864 |
| 50 | 70 | 1.0 | Welch t-test | 1000 | 0.95326 | 0.04270 | 0.01921 | 0.00245 | 0.13859 | 0.00885 |
| 50 | 70 | 1.0 | t-test | 1000 | 1.16667 | 0.05225 | 0.05714 | 0.02032 | 0.23905 | 0.04221 |
| 50 | 70 | 2.0 | Welch t-test | 1000 | 0.98888 | 0.04413 | 0.01938 | 0.00126 | 0.13919 | 0.00451 |
| 50 | 70 | 2.0 | t-test | 1000 | 1.21007 | 0.05409 | 0.07709 | 0.02594 | 0.27765 | 0.04640 |

### Hypothesis Testing and Confidence Intervals

When doing hypothesis tests, we are often interested in whether the Type
1 error rate is adequately controlled and whether the test has enough
power to detect an effect size of substantive interest. The rejection
rate of a hypothesis test captures the proportion of times the p-value
is below a specified $`\alpha`$ level—that is, the proportion of times
we reject the null hypothesis. When the specified effect size is 0, we
can examine Type 1 error rates and when the magnitude of the effect is
greater than 0, we can examine power. We are also interested in
confidence interval coverage, the proportion of intervals that contain
the true parameter, and the interval width, which is an indicator of the
precision of the interval estimator.

Table 4 below presents the performance criteria used to evaluate
hypothesis tests. In the table, let $`P_k`$ denote the p-value from
simulation replication $`k`$, for $`k = 1,...,K`$. Suppose that the
confidence intervals are for the target parameter $`\theta`$ and have
coverage level $`\beta`$. Let $`A_k`$ and $`B_k`$ denote the lower and
upper end-points of the confidence interval from simulation replication
$`k`$, and let $`W_k = B_k − A_k`$, for $`k = 1,...,K`$.

| Criterion | Measure | Definition | Estimate | MCSE |
|:---|:---|:---|:---|:---|
| Rejection Rate | Type 1 error or power | \$\rho\_\alpha = Pr(P_k) &lt; \alpha\$ | \$r\_\alpha = \frac{1}{K} \sum\_{k=1}^K I(P_k &lt; \alpha)\$ | $`\sqrt{r_\alpha(1 - r_\alpha) / K}`$ |
| Coverage | Proportion of intervals containing true parameter | $`\omega_\beta = Pr(A \leq \theta \leq B)`$ | $`c_\beta = \frac{1}{K}\sum_{k=1}^K I(A_k \leq \theta \leq B_k)`$ | $`\sqrt{c_\beta (1 - c_\beta) / K}`$ |
| Width | Precision | $`\text{E}(W) = \text{E}(B - A)`$ | $`\bar{W} = \bar{B} - \bar{A}`$ | $`\sqrt{S_W^2 / K}`$ |

Table 4. Hypothesis Testing and Confidence Intervals Performance
Criteria {.table .table .table-striped .table-hover
style="margin-left: auto; margin-right: auto;"}

#### Example

Below we calculate the rejection rates for the hypothesis tests
conducted using the conventional two-sample t-test and the Welch t-test.
The null hypothesis is that the two groups have the same means. The
[`calc_rejection()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_rejection.md)
function requires a data frame or tibble containing simulation results
as the first argument. The second argument, `p_values`, requires the
name of the column containing p-values. The third argument, `alpha`,
lets the user specify a value for $`\alpha`$. The default value is set
to the conventional 0.05.

``` r

# using group_modify()
welch_res %>%
  group_by(n1, n2, mean_diff, method) %>%
  group_modify(~ calc_rejection(.x, p_values = p_val)) %>%
  kable(digits = 5)
```

|  n1 |  n2 | mean_diff | method       | K_rejection | rej_rate | rej_rate_mcse |
|----:|----:|----------:|:-------------|------------:|---------:|--------------:|
|  50 |  50 |       0.0 | Welch t-test |        1000 |    0.047 |       0.00669 |
|  50 |  50 |       0.0 | t-test       |        1000 |    0.048 |       0.00676 |
|  50 |  50 |       0.5 | Welch t-test |        1000 |    0.335 |       0.01493 |
|  50 |  50 |       0.5 | t-test       |        1000 |    0.340 |       0.01498 |
|  50 |  50 |       1.0 | Welch t-test |        1000 |    0.871 |       0.01060 |
|  50 |  50 |       1.0 | t-test       |        1000 |    0.876 |       0.01042 |
|  50 |  50 |       2.0 | Welch t-test |        1000 |    1.000 |       0.00000 |
|  50 |  50 |       2.0 | t-test       |        1000 |    1.000 |       0.00000 |
|  50 |  70 |       0.0 | Welch t-test |        1000 |    0.039 |       0.00612 |
|  50 |  70 |       0.0 | t-test       |        1000 |    0.027 |       0.00513 |
|  50 |  70 |       0.5 | Welch t-test |        1000 |    0.426 |       0.01564 |
|  50 |  70 |       0.5 | t-test       |        1000 |    0.341 |       0.01499 |
|  50 |  70 |       1.0 | Welch t-test |        1000 |    0.937 |       0.00768 |
|  50 |  70 |       1.0 | t-test       |        1000 |    0.904 |       0.00932 |
|  50 |  70 |       2.0 | Welch t-test |        1000 |    1.000 |       0.00000 |
|  50 |  70 |       2.0 | t-test       |        1000 |    1.000 |       0.00000 |

Below we calculate the confidence interval coverage rates and widths for
the estimates of the mean difference. The
[`calc_coverage()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_coverage.md)
function requires a data frame or tibble containing the simulation
results as the first argument. The second and third arguments,
`lower_bound` and `upper_bound`, take in the name of the columns that
contain the lower and upper bound estimates of the confidence intervals.
The `true_param` argument requires the name of the column containing the
true parameters. Like
[`calc_absolute()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_absolute.md),
[`calc_relative()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative.md)
and
[`calc_relative_var()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_relative_var.md),
[`calc_coverage()`](https://meghapsimatrix.github.io/simhelpers/reference/calc_coverage.md)
also has an argument, `perfm_criteria`, where the user can specify which
criteria to evaluate: `perfm_criteria = c("coverage", "width")`.

``` r

# using group_modify()
welch_res %>%
  mutate(params = mean_diff) %>%
  group_by(n1, n2, mean_diff, method) %>%
  group_modify(~ calc_coverage(.x, lower_bound = lower_bound, upper_bound = upper_bound, true_param = params)) %>%
  kable(digits = 5)
```

| n1 | n2 | mean_diff | method | K_coverage | coverage | coverage_mcse | width | width_mcse |
|---:|---:|---:|:---|---:|---:|---:|---:|---:|
| 50 | 50 | 0.0 | Welch t-test | 1000 | 0.953 | 0.00669 | 1.25266 | 0.00343 |
| 50 | 50 | 0.0 | t-test | 1000 | 0.952 | 0.00676 | 1.24697 | 0.00338 |
| 50 | 50 | 0.5 | Welch t-test | 1000 | 0.944 | 0.00727 | 1.25800 | 0.00336 |
| 50 | 50 | 0.5 | t-test | 1000 | 0.943 | 0.00733 | 1.25223 | 0.00332 |
| 50 | 50 | 1.0 | Welch t-test | 1000 | 0.953 | 0.00669 | 1.25741 | 0.00339 |
| 50 | 50 | 1.0 | t-test | 1000 | 0.952 | 0.00676 | 1.25169 | 0.00335 |
| 50 | 50 | 2.0 | Welch t-test | 1000 | 0.959 | 0.00627 | 1.25859 | 0.00335 |
| 50 | 50 | 2.0 | t-test | 1000 | 0.958 | 0.00634 | 1.25287 | 0.00331 |
| 50 | 70 | 0.0 | Welch t-test | 1000 | 0.961 | 0.00612 | 1.09943 | 0.00241 |
| 50 | 70 | 0.0 | t-test | 1000 | 0.973 | 0.00513 | 1.21433 | 0.00288 |
| 50 | 70 | 0.5 | Welch t-test | 1000 | 0.952 | 0.00676 | 1.09938 | 0.00234 |
| 50 | 70 | 0.5 | t-test | 1000 | 0.972 | 0.00522 | 1.21412 | 0.00276 |
| 50 | 70 | 1.0 | Welch t-test | 1000 | 0.940 | 0.00751 | 1.09791 | 0.00239 |
| 50 | 70 | 1.0 | t-test | 1000 | 0.963 | 0.00597 | 1.21280 | 0.00283 |
| 50 | 70 | 2.0 | Welch t-test | 1000 | 0.951 | 0.00683 | 1.09886 | 0.00245 |
| 50 | 70 | 2.0 | t-test | 1000 | 0.971 | 0.00531 | 1.21376 | 0.00289 |

## How to Evaluate MCSE

Generally, the associated MCSE should be small compared to the
performance measure. Below is an example from Frane (2015) of how to
write up MCSE results as part of simulation study results. Frane (2015)
compared various methods to control Type 1 error rates when conducting
analysis on multiple outcome variables. In describing the simulation
results, he wrote:

> Only Bonferroni and MP strictly controlled the PFER; the observed
> maximum PFER was 0.050 for Bonferroni, 0.051 for Sidák, 0.061 for
> Holm, 0.100 for Hochberg, and 0.050 for MP (estimated SE ≤ 0.0002 for
> all estimates).

The PFER refers to per family error rate, the expected number of Type 1
errors out of $`m`$ comparisons. MP refers to multivariate analysis of
variance (MANOVA) protected tests. The SE refers to the MCSE. The number
of replications was 1,000,000 in Frane (2015). We note that the number
is quite high and generally 1,000 to 10,000 replications are enough. The
resulting MCSEs in Frane (2015), therefore, are exceedingly small
compared to the values for PFER.

For more information for performance criteria and MCSE, we recommend
Morris et al. (2019).

## References

Efron, B., & Stein, C. (1981). The jackknife estimate of variance. *The
Annals of Statistics*, 586–596.

Frane, A. V. (2015). Power and type i error control for univariate
comparisons in multivariate two-group designs. *Multivariate Behavioral
Research*, *50*(2), 233–247.

Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. *Statistics in Medicine*,
*38*(11), 2074–2102.

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D.,
François, R., … Yutani, H. (2019). Welcome to the tidyverse. *Journal of
Open Source Software*, *4*(43), 1686.
<https://doi.org/10.21105/joss.01686>
