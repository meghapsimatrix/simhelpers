% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bundle_sim.R
\name{bundle_sim}
\alias{bundle_sim}
\title{Bundle functions into a simulation driver function}
\usage{
bundle_sim(
  f_generate,
  f_analyze,
  f_summarize = NULL,
  reps_name = "reps",
  seed_name = "seed",
  summarize_opt_name = "summarize",
  stack_reps = TRUE,
  id = NULL
)
}
\arguments{
\item{f_generate}{function for data-generation}

\item{f_analyze}{function for data-analysis. The first argument must be the
data, in the format generated by \code{f_analyze()}.}

\item{f_summarize}{function for calculating performance summaries across
replications. The first argument must be the replicated data analysis
results. Default is \code{NULL}, so that no summary function is used.}

\item{reps_name}{character string to set the name of the argument for the
number of replications, with a default value of \code{"reps"}.}

\item{seed_name}{character string to set the name of the argument for the
seed option, with a default value of \code{"seed"}. Set to \code{NULL} to
remove the argument from the simulation driver.}

\item{summarize_opt_name}{character string to set the name of the argument
for where to apply \code{f_summarize} to the simulation results, with a
default value of \code{"summarize"}. Ignored if no \code{f_summarize} function is
specified. Set to \code{NULL} to remove the argument from the simulation
driver.}

\item{stack_reps}{logical indicating whether to combine the simulation
results into a \code{data.frame}, with a default value of
\code{TRUE}. If \code{FALSE}, then the function will return replications in
a list and so \code{f_summarize} must be able to take a list as its first
argument. Passed to \code{repeat_and_stack()}.}

\item{id}{Character string to use for creating a variable with a unique
identifier for each repetition of `f_generate` and `f_analyze`. If set to \code{NULL} (the default), then
no identifier is created. Passed to \code{repeat_and_stack()}.}
}
\value{
A function to repeatedly run the `f_generate` and `f_analyze`
  functions and (optionally) apply `f_summarize` to the resulting
  replications.
}
\description{
Bundle a data-generation function, a data-analysis function, and
  (optionally) a performance summary function into a simulation driver.
}
\examples{
f_G <- rnorm
f_A <- function(x, trim = 0) data.frame(y_bar = mean(x, trim = trim))
f_S <- function(x, calc_sd = FALSE) {
  if (calc_sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

# bundle data-generation and data-analysis functions
sim1 <- bundle_sim(f_generate = f_G, f_analyze = f_A)
args(sim1)
res1 <- sim1(4, n = 70, mean = 0.5, sd = 1, trim = 0.2)
res1

# bundle data-generation, data-analysis, and performance summary functions
sim2 <- bundle_sim(f_generate = f_G, f_analyze = f_A, f_summarize = f_S)
args(sim2)
res2 <- sim2(24, n = 7, mean = 0, sd = 1, trim = 0.2, calc_sd = TRUE)
res2

# bundle data-generation and data-analysis functions, returning results as a list
sim3 <- bundle_sim(f_generate = f_G, f_analyze = f_A, stack_reps = FALSE)
args(sim3)
res3 <- sim3(4, n = 70, mean = 0.5, sd = 3, trim = 0.2)
res3

}
