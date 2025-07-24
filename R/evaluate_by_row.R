#' @title Evaluate a simulation function on each row of a data frame or tibble
#'
#' @description Evaluates a simulation function on each row of a data frame or
#'   tibble containing parameter values. Returns a single tibble with parameters
#'   and simulation results. The function uses \code{furrr::future_pmap}, which
#'   allows for easy parallelization.
#'
#' @param params data frame or tibble containing simulation parameter values.
#'   Each row should represent a separate set of parameter values. Column names
#'   must exactly match the argument names of \code{sim_function}. Non-matching
#'   columns are ignored.
#' @param sim_function function to be evaluated, with argument names matching
#'   the variable names in \code{params}. The function must return a
#'   \code{data.frame}, \code{tibble}, or vector.
#' @param ... additional arguments passed to \code{sim_function}.
#' @param nest_results logical indicating whether to store the results of evaluating
#'   \code{sim_function} in a nested column. Default is \code{FALSE}.
#' @param results_name character string to set the name of the nested column storing
#'   the results of the simulation. Default is \code{".results"}.
#' @param system_time logical indicating whether to print computation time.
#'   \code{TRUE} by default.
#' @param verbose logical indicating whether to display a message about
#'   variables used in function evaluation. \code{TRUE} by default.
#' @inheritParams furrr::future_pmap
#'
#' @export
#'
#' @return A tibble containing parameter values and simulation results.
#'
#' @examples
#' df <- data.frame(
#'   n = 3:5,
#'   lambda = seq(8, 16, 4)
#' )
#'
#' evaluate_by_row(df, rpois)
#'

evaluate_by_row <- function(
  params, sim_function, ...,
  nest_results = FALSE,
  results_name = ".results",
  .progress = FALSE,
  .options = furrr::furrr_options(seed = TRUE),
  system_time = TRUE,
  verbose = TRUE
) {

  sim_function_name <- paste0(deparse(substitute(sim_function)),"()")
  arg_names <- names(formals(sim_function))
  if ("..." %in% arg_names) {
    eval_names <- names(params)
    param_dat <- params
  } else {
    var_names <- names(params)
    eval_names <- intersect(arg_names, var_names)
    param_dat <- dplyr::select(params, tidyr::all_of(eval_names))
  }

  if (verbose) {
    msg <- paste("Evaluating", sim_function_name, "using the following variables:", paste(eval_names, collapse = ", "))
    message(msg)
  }

  sys_tm <- system.time(
    results_list <- furrr::future_pmap(
      param_dat,
      .f = sim_function,
      ...,
      .progress = .progress,
      .options = .options
    )
  )

  if (system_time) print(sys_tm, "\n")

  res <- params
  res[[results_name]] <- results_list

  if (!nest_results) {
    res <- tidyr::unnest(res, cols = tidyr::all_of(results_name))
  }

  return(res)
}
