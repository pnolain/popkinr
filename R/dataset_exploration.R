#' PMxplore shiny application
#'
#' A Shiny application for NONMEM dataset exploration.
#'
#' @param ... arguments passed to \code{shiny::runApp(...)} function.
#'
#' @return A Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' dataset_exploration()
#' }
dataset_exploration <- function(browser = getOption("browser"), ...){
  pmxplore_app_dir <- system.file("popkinr/dataset_exploration", package = "popkinr")

  runApp(appDir = pmxplore_app_dir, ...)
}
