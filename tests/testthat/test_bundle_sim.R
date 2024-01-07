f_G <- rnorm

f_A1 <- function(x, trim = 0) data.frame(y_bar = mean(x, trim = trim))
f_A2 <- function(x, trim) data.frame(y_bar = mean(x, trim = trim))

f_S1 <- function(x, calc_sd = FALSE) {
  if (calc_sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

f_S2 <- function(x, sd = FALSE) {
  if (sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

f_S3 <- function(x, calc_sd) {
  if (calc_sd) {
    res_SD <- apply(x, 2, sd)
    res <- data.frame(M = colMeans(x), SD = res_SD)
  } else {
    res <- data.frame(M = colMeans(x))
  }
  res
}

simmer_1 <- bundle_sim(f_generate = f_G, f_analyze = f_A1)
simmer_1(50, n = 10)

simmer_2 <- bundle_sim(f_generate = f_G, f_analyze = f_A1, f_summarize = f_S1)
simmer_2(50, n = 10)
simmer_2(50, n = 10, calc_sd = TRUE)
simmer_2(50, n = 10, calc_sd = TRUE, summarize = FALSE)
