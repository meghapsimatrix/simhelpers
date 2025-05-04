
winsorize <- function(x, winz, na_val = NA) {
  if (!is.na(na_val)) x[is.na(x)] <- na_val
  quartiles <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
  IQR <- diff(quartiles)
  trunc_points <- quartiles + c(-1, 1) * winz * IQR
  winsorization_pct <- mean((x < trunc_points[1]) | (x > trunc_points[2]), na.rm = TRUE)
  res <- pmax(pmin(x, trunc_points[2]), trunc_points[1])
  attr(res, "winsor_pct") <- winsorization_pct
  return(res)
}

winsorize_by <- function(x, by, winz, na_val = NA) {
  res_list <- tapply(x, by, winsorize, winz = winz, na_val = na_val, simplify = FALSE)
  res <- unsplit(res_list, by)
  winsor_pct <- sapply(res_list, \(x) attr(x, "winsor_pct"))
  attr(res, "winsor_pct") <- winsor_pct
  res
}
