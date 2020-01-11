#' @title Open an APA-6 style .Rnw template
#'
#' @description
#' Creates and opens an .Rnw file containing a template for writing a manuscript in APA-6 style.
#'
#' @param filename Name of file to create.
#' @param ... Further arguments passed to file.copy
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Rnw_APA6("Awesome-New-Paper")
#' }
#'


Rnw_APA6 <- function(filename, ...) {

  from <- system.file("templates", "APA6.Rnw", package = "SimHelpers")
  to <- paste0(gsub(" ","-", filename), ".Rnw")
  copy <- file.copy(from, to, ...)

  return(utils::file.edit(to))
}


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
#' create_skeleton("Behrens-Fisher problem")
#' }

create_skeleton <- function(filename, ...) {

  from <- system.file("templates","simulation_skeleton.R", package = "SimHelpers")
  to <- paste0(filename,".R")
  copy <- file.copy(from, to, ...)

  return(utils::file.edit(to))
}
