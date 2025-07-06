

#' Repeat an expression multiple times and (optionally) stack the results.
#'
#' @description Repeat an expression (usually involving random number
#'   generation) multiple times. Optionally, organize the results into a
#'   \code{data.frame} that stacks the output from all replications of the
#'   expression.

#' @param n Number of times to repeat the expression
#' @param expr An expression to be evaluated.
#' @param id Character string to use for creating a variable with a unique
#'   identifier for each repetition. If set to \code{NULL} (the default), then
#'   no identifier is created.
#' @param stack Logical value indicating whether to organize the results into a
#'   \code{data.frame}.
#'
#' @return If \code{stack = TRUE} (the default), the results of each evaluation
#'   of \code{expr} will be stacked together using \code{rbind} and a unique identifier will be stored in the variable \code{id} (if specified). If \code{stack
#'   = FALSE}, a list of length \code{n} with entries corresponding to the
#'   output of each replication of \code{expr}, with names corresponding to the unique identifier (if specified)
#' @export
#'
#' @examples
#' repeat_and_stack(n = 3, data.frame(x = rexp(2)))
#' repeat_and_stack(n = 3, data.frame(x = rexp(2)), id = "ID")
#'
#' repeat_and_stack(n = 3, data.frame(x = rexp(2)), stack = FALSE)
#' repeat_and_stack(n = 3, data.frame(x = rexp(2)), id = "ID", stack = FALSE)
#'

repeat_and_stack <- function(
  n,
  expr,
  id = NULL,
  stack = TRUE
) {
  f <- eval.parent(substitute(function(...) expr))
  res <- lapply(integer(n), f)
  if (stack) {
    if (!is.null(id)) {
      res <- mapply(
        function(i, r) {
          r <- as.data.frame(r)
          id_dat <- data.frame(x = rep(i, nrow(r)))
          names(id_dat) <- id
          cbind(id_dat, r)
        },
        i = 1:n, r = res,
        SIMPLIFY = FALSE
      )
    }
    res <- do.call(rbind, res)
  } else {
    if (!is.null(id)) names(res) <- 1:n
  }
  return(res)
}
