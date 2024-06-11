
winsorize <- function(x, winz) {
  quartiles <- quantile(x, c(.25, .75))
  IQR <- diff(quartiles)
  trunc_points <- quartiles + c(-1, 1) * winz * IQR
  winsorization_pct <- mean((x < trunc_points[1]) | (x > trunc_points[2]))
  res <- pmax(pmin(x, trunc_points[2]), trunc_points[1])
  attr(res, "winsor_pct") <- winsorization_pct
  return(res)
}
