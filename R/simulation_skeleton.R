#' @title Open a simulation skeleton
#'
#' @description
#' Creates and opens an .R file containing a skeleton for writing a Monte Carlo simulation study.
#'
#' @param ... Further arguments passed to file.copy
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_skeleton()
#' }

create_skeleton <- function(...) {

  from <- system.file("templates", "simulation_skeleton.R", package = "simhelpers")
  to <- tempfile(fileext = ".R")
  copy <- file.copy(from, to, ...)

  return(rstudioapi::navigateToFile(to))

}
