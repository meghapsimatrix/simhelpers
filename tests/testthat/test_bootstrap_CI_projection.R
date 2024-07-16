library(boot)

set.seed(20240629)
N <- 50
mu <- 2
nu <- 5
dat <- mu + rt(N, df = nu)

f <- \(x,i) {
  c(
    M = mean(x[i], trim = 0.1),
    SE = sd(x[i]) / sqrt(length(x[i]))
  )
}

booties <- boot(dat, f, R = 399)

boot_CIs <- boot.ci(
  booties,
  type = c("norm","basic", "stud", "perc"),
  var.t0 = booties$t0[2]^2,
  var.t = booties$t[,2]^2
)

rbind(
  normal = boot_CIs$normal[2:3],
  basic = boot_CIs$basic[4:5],
  student = boot_CIs$student[4:5],
  percentile = boot_CIs$percent[4:5]
)

calc_boot_CIs(
  i=1:399,
  boot_est = booties$t[,1],
  boot_se = booties$t[,2],
  est = booties$t0[1],
  se = booties$t0[2],
  CI_type = c("normal","basic","student","percentile")
)
