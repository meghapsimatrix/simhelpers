
winsorize <- function(x, winz) {
  quartiles <- stats::quantile(x, c(.25, .75))
  IQR <- diff(quartiles)
  trunc_points <- quartiles + c(-1, 1) * winz * IQR
  winsorization_pct <- mean((x < trunc_points[1]) | (x > trunc_points[2]))
  res <- pmax(pmin(x, trunc_points[2]), trunc_points[1])
  attr(res, "winsor_pct") <- winsorization_pct
  return(res)
}

winsorize_by <- function(x, by, winz) {
  res_list <- tapply(x, by, winsorize, winz = winz, simplify = FALSE)
  res <- unsplit(res_list, by)
  winsor_pct <- sapply(res_list, \(x) attr(x, "winsor_pct"))
  attr(res, "winsor_pct") <- winsor_pct
  res
}
