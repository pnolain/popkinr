#' NONMEM Launcher
#'
#' A Shiny application to monitor and start of NONMEM jobs.
#'
#' @param ... arguments passed to \code{shiny::runApp(...)} function.
#' @param type
#' @param nonmem_exe
#' @param nonmem_call
#' @param nmcheck_exe
#' @param nmcheck_call
#' @param nonmem_parafile
#' @param execution_dir
#' @param results_subdir
#'
#' @return A Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' nonmem_monitor()
#' }
nonmem_monitor <- function(type = "local",
                           nonmem_exe = NULL,
                           nonmem_call = NULL,
                           nmcheck_exe = NULL,
                           nmcheck_call = NULL,
                           nonmem_parafile = NULL,
                           execution_dir = "pmxrun",
                           results_subdir = "results", ...){

  if(!is.null(nonmem_exe))
    Sys.setenv(NM_EXE = nonmem_exe)

  if(!is.null(nonmem_call))
    Sys.setenv(NM_CALL = nonmem_call)

  if(!is.null(nmcheck_exe))
    Sys.setenv(NMCHECK_EXE = nmcheck_exe)

  if(!is.null(nmcheck_call))
    Sys.setenv(NMCHECK_CALL = nmcheck_call)

  if(!is.null(nonmem_parafile))
    Sys.setenv(NM_PARAFILE = nonmem_parafile)

  if(!is.null(execution_dir))
    Sys.setenv(RUN_EXECUTION_DIR = execution_dir)

  if(!is.null(results_subdir))
    Sys.setenv(RUN_RESULTS_SUBDIR = results_subdir)

  if(type == "local"){
    app_dir <- system.file("popkinr/nonmem_local", package = "popkinr")
  } else if(type == "torque"){
    stop(simpleError("Torque version is not implemented yet."))
  }

  runApp(appDir = app_dir, ...)
}
