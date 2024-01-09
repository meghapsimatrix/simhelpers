#' @title Evaluate a simulation function on each row of a data frame or tibble
#'
#' @description Evaluates a simulation function on each row of a data frame or tibble
#'   containing parameter values. Returns a single tibble with parameters
#'   and simulation results. The function uses \code{furrr::future_pmap}, which
#'   allows for easy parallelization.
#'
#' @param params data frame or tibble containing simulation parameter values. Each row should
#'   represent a separate set of parameter values.
#' @param sim_function function to be evaluated, with argument names matching
#'   the variable names in \code{params}. The function must return a
#'   \code{data.frame}, \code{tibble}, or vector.
#' @param ... additional arguments passed to \code{sim_function}.
#' @param results_name character string to set the name of the column storing the results of the simulation. Default is \code{".results"}.
#' @param system_time logical indicating whether to print computation time.
#'   \code{TRUE} by default.
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

evaluate_by_row <- function(params, sim_function, ...,
                            results_name = ".results",
                            .progress = FALSE, .options = furrr::furrr_options(),
                            system_time = TRUE) {

  sys_tm <- system.time(
    results_list <- furrr::future_pmap(
      params,
      .f = sim_function,
      ...,
      .progress = .progress,
      .options = .options
    )
  )

  if (system_time) print(sys_tm, "\n")

  params[[results_name]] <- results_list
  tidyr::unnest(params, cols = tidyr::all_of(results_name))

}
