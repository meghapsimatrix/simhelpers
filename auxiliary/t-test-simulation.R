#---------------------------------------------------------------
# Generate one simulated dataset
#---------------------------------------------------------------

generate_chisq_samples <- function(nA, nB, mu_A, mu_B) {
  
  YA <- rchisq(n = nA, df = mu_A)
  YB <- rchisq(n = nB, df = mu_B)
  
  sample_data <- data.frame(
    group = rep(c("A","B"), times = c(nA, nB)), 
    Y = c(YA, YB)
  )
  return(sample_data)
}


sample_data <- generate_chisq_samples(nA = 4, nB = 10, mu_A = 5, mu_B = 7)
sample_data


#---------------------------------------------------------------
# Function for Welch's t-test 
# https://en.wikipedia.org/wiki/Welch%27s_t-test
#---------------------------------------------------------------

t_test <- function(sample_data) {
  
  # calculate raw summary statistics
  Ns <- table(sample_data$group)
  means <- tapply(sample_data$Y, sample_data$group, mean)
  sds <- tapply(sample_data$Y, sample_data$group, sd)
  
  # t-test
  tstat <- (means[[2]] - means[[1]]) / sqrt(sum(sds^2 / Ns))
  df <- sum(sds^2 / Ns)^2 / sum(sds^4 / (Ns^2 * (Ns - 1)))
  pval <- 2 * pt(q = abs(tstat), df = df, lower.tail = FALSE)
  
  return(data.frame(tstat, df, pval))
}

t_test(sample_data)


#---------------------------------------------------------------
# Run all the analysis
#---------------------------------------------------------------

run_t_tests <- function(sample_data) {
  t_raw <- t_test(sample_data)
  t_raw$transform <- "raw"
  
  log_data <- data.frame(
    group = sample_data$group,
    Y = log(sample_data$Y)
  )
  t_log <- t_test(log_data)
  t_log$transform <- "log"
  
  return(rbind(t_raw, t_log))
}

run_t_tests(sample_data)


#---------------------------------------------------------------
# Repeat stuff with rerun()
#---------------------------------------------------------------

library(dplyr)
library(purrr)

replications <- rerun(5, {
  dat <- generate_chisq_samples(nA = 4, nB = 10, mu_A = 5, mu_B = 7)
  run_t_tests(dat)
})

replications # output is a list of results


# Stack the results with bind_rows()

replications <- rerun(5, {
  dat <- generate_chisq_samples(nA = 4, nB = 10, mu_A = 5, mu_B = 7)
  run_t_tests(dat)
}) %>%
  bind_rows()

replications

#---------------------------------------------------------------
# Estimating rejection rates
#---------------------------------------------------------------

# Now with 20 replications

replications <- rerun(20, {
  generate_chisq_samples(nA = 4, nB = 10, mu_A = 5, mu_B = 7) %>%
    run_t_tests()
}) %>% 
  bind_rows()


# Calculate rejection rates from p-values

replications %>%
  group_by(transform) %>%
  summarize(
    rate_05 = mean(pval < .05),
    rate_10 = mean(pval < .10)
  )

#---------------------------------------------------------------
# Function to simulate a single scenario
#---------------------------------------------------------------

run_sim <- function(reps, nA, nB, mu_A, mu_B) {
  replications <- 
    rerun(reps, {
      dat <- generate_chisq_samples(nA = nA, nB = nB, mu_A = mu_A, mu_B = mu_B)
      run_t_tests(dat)
    }) %>% 
    bind_rows()
  
  replications %>%
    group_by(transform) %>%
    summarize(
      rate_05 = mean(pval < .05),
      rate_10 = mean(pval < .10)
    )
}

run_sim(reps = 1000, nA = 4, nB = 7, mu_A = 10, mu_B = 10)

# Reproducible run_sim()

run_sim <- function(reps, nA, nB, mu_A, mu_B, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  replications <- 
    rerun(reps, {
      dat <- generate_chisq_samples(nA = nA, nB = nB, mu_A = mu_A, mu_B = mu_B)
      run_t_tests(dat)
    }) %>% 
    bind_rows()
  
  replications %>%
    group_by(transform) %>%
    summarize(
      rate_05 = mean(pval < .05),
      rate_10 = mean(pval < .10)
    )
}

run_sim(50, nA=8, nB=12, mu_A=15, mu_B=15, seed = 20200311)
run_sim(50, nA=8, nB=12, mu_A=15, mu_B=15, seed = 20200311)



#---------------------------------------------------------------
# Simulation experiments
#---------------------------------------------------------------

# The hard way

run_sim(1000, nA=4, nB=8, mu_A=5, mu_B=5)
run_sim(1000, nA=4, nB=8, mu_A=10, mu_B=10)
run_sim(1000, nA=4, nB=8, mu_A=15, mu_B=15)

#---------------------------------------------------------------
# Creating an experimental design
#---------------------------------------------------------------

design_factors <- list(
  mu_A = c(5, 10, 15, 20),
  nA = c(4, 8, 16),
  nB = c(4, 8, 16)
)

params <- 
  cross_df(design_factors) %>%
  mutate(
    mu_B = mu_A,
    reps = 1000,
    seed = 20200311 + 1:n()
  )

params # A "menu" of conditions we would like to study


#---------------------------------------------------------------
# Iterate over conditions using pmap()
#---------------------------------------------------------------

sim_results <- pmap(params, .f = run_sim)


# store the results as a new variable with the parameter values

sim_results <- params
sim_results$res <- pmap(params, .f = run_sim)


# Use unnest() to expand the res variable

library(tidyr)
unnest(sim_results, cols = res)


# A tidy workflow

sim_results <- 
  params %>%
  mutate(res = pmap(., .f = run_sim)) %>%
  unnest(cols = res)

sim_results

#---------------------------------------------------------------
# Parallel processing
#---------------------------------------------------------------

parallel::detectCores()

library(future)
library(furrr)

plan(multiprocess) # choose an appropriate plan from future package

sim_results <- 
  params %>%
  mutate(res = future_pmap(., .f = run_sim)) %>%
  unnest(cols = res)


# In serial

system.time(
  params %>%
    mutate(
      res = pmap(., .f = run_sim)
    ) %>%
    unnest(cols = res)
)


# In parallel

system.time(
  params %>%
    mutate(
      res = future_pmap(., .f = run_sim)
    ) %>%
    unnest(cols = res)
)


#---------------------------------------------------------------
# Visualization
#---------------------------------------------------------------

library(ggplot2)

ggplot(sim_results, 
       aes(x = mu_A, y = rate_05, 
           shape = transform, color = transform)) + 
  geom_point() + 
  geom_line() +
  facet_grid(nA ~ nB, labeller = "label_both") + 
  expand_limits(y = 0) + 
  geom_hline(yintercept = .05) + 
  theme_light()

ggplot(sim_results, 
       aes(x = mu_A, y = rate_10, 
           shape = transform, color = transform)) + 
  geom_point() + 
  geom_line() +
  facet_grid(nA ~ nB, labeller = "label_both") + 
  expand_limits(y = 0) + 
  geom_hline(yintercept = .10) + 
  theme_light()
