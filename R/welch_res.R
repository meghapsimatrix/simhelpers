#' Welch t-test Simulation Results
#'
#' A dataset containing simulation results comparing Welch t-test to normal t-test
#'
#'
#' @format A tibble with 24000 rows and 10 variables:
#' \describe{
#'   \item{n}{sample size per group assuming equal sample size}
#'   \item{mean_diff}{true difference in means of two groups used to generate the data}
#'   \item{iterations}{number of iterations}
#'   \item{seed}{seed used to generate data}
#'   \item{method}{indicates whether Welch or normal t-test was used}
#'   \item{est}{estimate of the mean difference}
#'   \item{var}{variance of the estimate}
#'   \item{p_val}{p-value from the t-test}
#'   \item{lower_bound}{lower bound of the confidence interval}
#'   \item{upper_bound}{upper bound of the confidence interval}
#'
#' }
#'
"welch_res"
