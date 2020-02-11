#' @title Open a simulation skeleton
#'
#' @description
#' Creates and opens an .R file containing a skeleton for writing a Monte Carlo simulation study.
#'
#' @param filename Name of file to create.
#' @param ... Further arguments passed to file.copy
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_skeleton("Behrens_Fisher_problem")
#' }

create_skeleton <- function(filename, ...) {

  from <- system.file("templates","simulation_skeleton.R", package = "SimHelpers")
  to <- paste0(filename,".R")
  copy <- file.copy(from, to, ...)

  return(utils::file.edit(to))
}
