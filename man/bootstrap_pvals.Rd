% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap-projection.R
\name{bootstrap_pvals}
\alias{bootstrap_pvals}
\title{Calculate one or multiple bootstrap p-values}
\usage{
bootstrap_pvals(
  boot_stat,
  stat,
  alternative = "two-sided",
  B_vals = length(boot_stat),
  reps = 1L,
  enlist = FALSE,
  seed = NULL
)
}
\arguments{
\item{boot_stat}{vector of bootstrap replications of a test statistic.}

\item{stat}{numeric value of the test statistic based on the original sample.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two-sided"} (the default), \code{"greater"} or
\code{"less"}.}

\item{B_vals}{vector of sub-sample sizes for which to calculate p-values.
Setting \code{B_vals = length(boot_stat)} (the default) will return a
single p-value calculated on the full set of bootstrap replications. For
\code{B_vals < length(boot_stat)}, p-values will be calculated after
sub-sampling (without replacement) the bootstrap replications.}

\item{reps}{integer value for the number of sub-sample p-values to generate
when \code{B_vals < length(boot_stat)}, with a default of \code{reps = 1}.}

\item{enlist}{logical indicating whether to wrap the returned values in an
unnamed list, with a default of \code{FALSE}. Setting \code{enlist = TRUE}
makes it easier to store the output as a single entry in a \code{tibble}.}

\item{seed}{Single numeric value to which the random number generator seed
will be set. Default is \code{NULL}, which does not set a seed.}
}
\value{
The format of the output depends on several contingencies. If only a
  single value of \code{B_vals} is specified and \code{reps = 1}, then the
  function returns a vector with a single p-value. If only a single value of
  \code{B_vals} is specified but \code{B_vals < length(boot_stat)} and
  \code{reps > 1}, then the function returns a vector p-values, with an entry
  for each sub-sample replication. If \code{B_vals} is a vector of multiple
  values, then the function returns a list with one entry per entry of
  \code{B_vals}, where each entry is a vector of length \code{reps} with
  entries for each sub-sample replication.

  If \code{enlist = TRUE}, then results will be wrapped in an unnamed list,
  which makes it easier to sore the output in a tibble.
}
\description{
Calculate one or multiple bootstrap p-values, given a bootstrap
  sample of test statistics.
}
\details{
p-values are calculated by comparing \code{stat} to the distribution
  of \code{boot_stat}, which is taken to represent the null distribution of
  the test statistic. If \code{alternative = "two-sided"} (the default), then
  the p-value is the proportion of the bootstrap sample where the absolute
  value of the bootstrapped statistic exceeds the absolute value of the
  original statistic. If \code{alternative = "greater"}, then the p-value is
  the proportion of the bootstrap sample where the value of the bootstrapped
  statistic is larger than the original statistic. If \code{alternative =
  "less"}, then the p-value is the proportion of the bootstrap sample where
  the value of the bootstrapped statistic is less than the original
  statistic.
}
\examples{
# generate data from two distinct populations
dat <- data.frame(
  group = rep(c("A","B"), c(40, 50)),
  y = c(
    rgamma(40, shape = 7, scale = 2),
    rgamma(50, shape = 3, scale = 4)
  )
)
stat <- t.test(y ~ group, data = dat)$statistic

# create bootstrap replications under the null of no difference
boot_dat <- dat
booties <- replicate(399, {
  boot_dat$group <- sample(dat$group)
  t.test(y ~ group, data = boot_dat)$statistic
})

# calculate bootstrap p-values from full set of bootstrap replicates
bootstrap_pvals(boot_stat = booties, stat = stat)

# calculate multiple bootstrap p-values using sub-sampling of replicates
bootstrap_pvals(
  boot_stat = booties, stat = stat,
  B_vals = 199,
  reps = 4L
)

# calculate multiple bootstrap p-values using sub-sampling of replicates,
# for each of several sub-sample sizes.
bootstrap_pvals(
  boot_stat = booties, stat = stat,
  B_vals = c(49,99,199),
  reps = 4L
)

}
\references{
Davison, A.C. and Hinkley, D.V. (1997). _Bootstrap Methods and
  Their Application_, Chapter 4. Cambridge University Press.
}
