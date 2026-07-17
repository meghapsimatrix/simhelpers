#' Simulation results from Chen & Pustejovsky (2025)
#'
#' A dataset containing a subset of results from a simulation study examining
#' the performance of methods to correct for publication bias in meta-analyses
#' that involve dependent effect sizes.
#'
#'
#' @format A tibble with 10,368 rows and 22 variables:
#' \describe{
#'   \item{k}{Parameter setting for the number of studies included in each meta-analysis.}
#'   \item{mu}{Parameter setting for the average effect size across studies.}
#'   \item{tau}{Parameter setting for the between-study standard deviation of the effect size distribution.}
#'   \item{cor_mu}{Parameter setting for the correlation between outcomes measured within the same study.}
#'   \item{wts}{Parameter setting for the selection weight, which controls the probability that a non-affirmative result is reported.}
#'   \item{iterations}{Number of simulation iterations per condition.}
#'   \item{method}{Estimation method applied to each simulated dataset.}
#'   \item{n_converged}{Number of simulation iterations in which the estimation method converged.}
#'   \item{bias}{Bias of the estimator for the average effect size (mu)}.
#'   \item{bias_mcse}{Monte Carlo standard error of bias.}
#'   \item{var}{Variance of the estimator method for the average effect size (mu)}.
#'   \item{var_mcse}{Monte Carlo standard error of variance.}
#'   \item{mse}{Mean squared error of the estimator for the average effect size (mu)}.
#'   \item{mse_mcse}{Monte Carlo standard error of mean squared error.}
#'   \item{rmse}{Root mean squared error of the estimator for the average effect size (mu)}.
#'   \item{rmse_mcse}{Monte Carlo standard error of root mean squared error.}
#'   \item{coverage}{Coverage level of the 95% confidence interval for the average effect size (mu).}.
#'   \item{coverage_mcse}{Monte Carlo standard error of the coverage level.}
#'   \item{width}{Average width of the 95% confidence interval for the average effect size (mu)}.
#'   \item{width_mcse}{Monte Carlo standard error of the average width.}
#'   \item{rej_rate}{Rejection rate of a hypothesis test that the average effect size (mu) is equal to zero.}.
#'   \item{rej_rate_mcse}{Monte Carlo standard error of the rejection rate.}
#' }
#'
#' @details This dataset contains only a subset of the results from the
#'   simulation study reported in Chen and Pustejovsky (2025). The simulation
#'   followed a full factorial design involving 4 levels for \code{k}, 4 levels
#'   for \code{mu}, 4 levels for \code{tau}, 3 levels for \code{cor_mu}, and 6
#'   levels for \code{wts}, for a total of 1152 unique conditions. For each
#'   condition, the dataset includes performance measures for each of 9
#'   estimation methods:
#'   * `"3PSM"`: a three-parameter step function selection model, with a step at \eqn{\alpha = .025}, ignoring the presence of dependent effect sizes
#'   * `"4PSM"`: a four-parameter step function selection model, with steps at \eqn{\alpha = .025, .500}, ignoring the presence of dependent effect sizes
#'   * `"CHE-ISCW"`: a summary meta-analysis using the correlated-and-heirarchical effects working model with inverse sampling-covariance weighting
#'   * `"EK"`: a multivariate version of the endogenous kink meta-regression
#'   * `"PET-PEESE"`: a multivariate version of PET-PEESE meta-regression (i.e., a limit meta-regression)
#'   * `"TF"`: Trim-and-Fill, ignoring the presence of dependent effect sizes
#'   * `"WAAP"`: a multivariate version of the weighted average of adequately powered studies
#'   * `"WILS"`: a multivariate version of the weighted-and-iterated least squares method, stopping at a minimum of \eqn{k = 5} studies
#'   * `"p-uniform*"`: the p-uniform* estimator, ignoring the presence of dependent effect sizes
#'
#'
#'
#' @source \insertRef{chen2025adapting}{simhelpers}
#'
#' @importFrom Rdpack reprompt
#'
"Chen_Pusto"
