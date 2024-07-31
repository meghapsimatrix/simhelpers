repeat_and_stack <- function(
  n,
  expr,
  stack = TRUE
) {
  f <- eval.parent(substitute(function(...) expr))
  res <- lapply(integer(n), f)
  if (stack) res <- do.call(rbind, res)
  return(res)
}
