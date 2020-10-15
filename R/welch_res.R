#' Welch t-test simulation results
#'
#' A dataset containing simulation results from a study comparing Welch t-test to the conventional t-test.
#'
#'
#' @format A tibble with 16,000 rows and 11 variables:
#' \describe{
#'   \item{n1}{sample size for Group 1.}
#'   \item{n2}{sample size for Group 2.}
#'   \item{mean_diff}{true difference in means of two groups used to generate the data.}
#'   \item{iterations}{number of iterations.}
#'   \item{seed}{seed used to generate data.}
#'   \item{method}{indicates whether Welch or conventional t-test was used.}
#'   \item{est}{estimate of the mean difference.}
#'   \item{var}{variance of the estimate.}
#'   \item{p_val}{p-value from the t-test.}
#'   \item{lower_bound}{lower bound of the confidence interval.}
#'   \item{upper_bound}{upper bound of the confidence interval.}
#'
#' }
#'
"welch_res"
