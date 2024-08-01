

#' Repeat an expression multiple times and (optionally) stack the results.
#'
#' @description Repeat an expression (usually involving random number
#'   generation) multiple times. Optionally, organize the results into a
#'   \code{data.frame} that stacks the output from all replications of the
#'   expression.

#' @param n Number of times to repeat the expression
#' @param expr An expression to be evaluated.
#' @param stack Logical value indicating whether to organize the results into a
#'   \code{data.frame}.
#'
#' @return If \code{stack = TRUE} (the default), the results of each evaluation
#'   of \code{expr} will be stacked together using \code{rbind}. If \code{stack
#'   = FALSE}, a list of length \code{n} with entries corresponding to the
#'   output of each replication of \code{expr}.
#' @export
#'
#' @examples
#' repeat_and_stack(n = 3, data.frame(x = rexp(2)))
#'
#' repeat_and_stack(n = 3, data.frame(x = rexp(2)), stack = FALSE)
#'

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
