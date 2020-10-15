#' t-test simulation results
#'
#' A dataset containing simulation results from a study that just runs a t-test.
#'
#'
#' @format A tibble with 1,000 rows and 5 variables:
#' \describe{
#'   \item{est}{estimate of the mean difference.}
#'   \item{p_val}{p-value from the t-test.}
#'   \item{lower_bound}{lower bound of the confidence interval.}
#'   \item{upper_bound}{upper bound of the confidence interval.}
#'   \item{true_param}{true mean difference used to generate the data.}
#'
#' }
#'
"t_res"
