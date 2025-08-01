% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_by_row.R
\name{evaluate_by_row}
\alias{evaluate_by_row}
\title{Evaluate a simulation function on each row of a data frame or tibble}
\usage{
evaluate_by_row(
  params,
  sim_function,
  ...,
  nest_results = FALSE,
  results_name = ".results",
  .progress = FALSE,
  .options = furrr::furrr_options(seed = TRUE),
  system_time = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{params}{data frame or tibble containing simulation parameter values.
Each row should represent a separate set of parameter values. Column names
must exactly match the argument names of \code{sim_function}. Non-matching
columns are ignored.}

\item{sim_function}{function to be evaluated, with argument names matching
the variable names in \code{params}. The function must return a
\code{data.frame}, \code{tibble}, or vector.}

\item{...}{additional arguments passed to \code{sim_function}.}

\item{nest_results}{logical indicating whether to store the results of evaluating
\code{sim_function} in a nested column. Default is \code{FALSE}.}

\item{results_name}{character string to set the name of the nested column storing
the results of the simulation. Default is \code{".results"}.}

\item{.progress}{A single logical. Should a progress bar be displayed?
Only works with multisession, multicore, and multiprocess futures. Note
that if a multicore/multisession future falls back to sequential, then
a progress bar will not be displayed.

\strong{Warning:} The \code{.progress} argument will be deprecated and removed
in a future version of furrr in favor of using the more robust
\href{https://CRAN.R-project.org/package=progressr}{progressr}
package.}

\item{.options}{The \code{future} specific options to use with the workers. This
must be the result from a call to \code{\link[furrr:furrr_options]{furrr_options()}}.}

\item{system_time}{logical indicating whether to print computation time.
\code{TRUE} by default.}

\item{verbose}{logical indicating whether to display a message about
variables used in function evaluation. \code{TRUE} by default.}
}
\value{
A tibble containing parameter values and simulation results.
}
\description{
Evaluates a simulation function on each row of a data frame or
  tibble containing parameter values. Returns a single tibble with parameters
  and simulation results. The function uses \code{furrr::future_pmap}, which
  allows for easy parallelization.
}
\examples{
df <- data.frame(
  n = 3:5,
  lambda = seq(8, 16, 4)
)

evaluate_by_row(df, rpois)

}
