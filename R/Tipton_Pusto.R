#' Results for Figure 2 of Tipton & Pustejovsky (2015)
#'
#' A dataset containing simulation results comparing small sample correction
#' methods for cluster robust variance estimation in meta-analysis.
#'
#'
#' @format A tibble with 15,300 rows and 8 variables:
#' \describe{
#'   \item{num_studies}{the number of studies included in the meta-analysis.}
#'   \item{r}{correlation between outcomes.}
#'   \item{Isq}{measure of heterogeneity of true effects.}
#'   \item{contrast}{type of contrast that was tested.}
#'   \item{test}{small sample method used.}
#'   \item{q}{the number of parameters in the hypothesis test.}
#'   \item{rej_rate}{the Type 1 error rate.}
#'   \item{mcse}{the Monte Carlo standard error for the estimate of the Type 1 error rate.}
#'
#' }
#'
#' @source
#' \insertRef{tipton_small-sample_2015}{simhelpers}
#'
#' @importFrom Rdpack reprompt
#'
"Tipton_Pusto"
