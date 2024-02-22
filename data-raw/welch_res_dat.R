## code to prepare `results` dataset goes here
library(tidyverse)
library(broom)


# Data generation ---------------------------------------------------------

# function to create normally distributed data for each group to run t test

generate_dat <- function(n1, n2, mean_diff){

  dat <- tibble(
    y = c(rnorm(n = n1, mean_diff, 1), # mean diff as mean, sd 1
          rnorm(n = n2, 0, 2)), # mean 0, sd 2
    group = c(rep("Group 1", n1), rep("Group 2", n2))
  )

  return(dat)

}
}



# Estimation Procedures ---------------------------------------------------

# function to calculate t-test, extracts estimate of the mean difference, p val and ci

# t and p value
calc_t <- function(est, vd, df, method){

  se <- sqrt(vd)  # standard error
  t <- est / se # t-test
  p_val <-  2 * pt(-abs(t), df = df) # p value
  ci <- est + c(-1, 1) * qt(.975, df = df) * se # confidence interval

  return(tibble(method = method, est = est, var = vd, p_val = p_val, lower_bound = ci[1], upper_bound = ci[2]))
}


estimate <- function(dat, n1, n2){

  # calculate summary stats
  means <- tapply(dat$y, dat$group, mean)
  vars <- tapply(dat$y, dat$group, var)

  # calculate summary stats
  est <- means[1] - means[2] # mean diff
  var_1 <- vars[1] # var for group 1
  var_2 <- vars[2] # var for group 2

  # conventional t-test
  dft <- n1 + n2 - 2  # degrees of freedom
  sp_sq <- ((n1 - 1) * var_1 + (n2 - 1) * var_2) / dft  # pooled var
  vdt <- sp_sq * (1 / n1 + 1 / n2) # variance of estimate

  # welch t-test
  dfw <- (var_1 / n1 + var_2 / n2)^2 / (((1 / (n1 - 1)) * (var_1 / n1)^2) + ((1 / (n2 - 1)) * (var_2 / n2)^2))  # degrees of freedom
  vdw <- var_1 / n1 + var_2 / n2 # variance of estimate

  results <- bind_rows(calc_t(est = est, vd = vdt, df = dft, method = "t-test"),
                       calc_t(est = est, vd = vdw, df = dfw, method = "Welch t-test"))


  return(results)

}

# Simulation Driver -------------------------------------------------------

run_sim <- function(iterations, n1, n2, mean_diff, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  results <-
    map_dfr(1:iterations, ~ {
      dat <- generate_dat(n1, n2, mean_diff)
      estimate(dat, n1, n2)
    })

}


# Experimental Design -----------------------------------------------------

# generating 1000 iterations

set.seed(20200110)

# now express the simulation parameters as vectors/lists

design_factors <- list(
  n1 = 50,
  n2 = c(50, 70),
  mean_diff = c(0, .5, 1, 2)
)

params <-
  expand_grid(!!!design_factors) %>%
  mutate(
    iterations = 1000,
    seed = round(runif(1) * 2^30) + 1:n()
  )




# Running Sim -------------------------------------------------------------

system.time(
  results <-
    params %>%
    mutate(
      res = pmap(., .f = run_sim)
    ) %>%
    unnest(cols = c(res))
)

welch_res <- results

usethis::use_data(welch_res, overwrite = TRUE)
