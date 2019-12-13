#' NONMEM Toolbox
#'
#' A Shiny application for interactive post-processing of NONMEM run results.
#'
#' @param ... arguments passed to \code{shiny::runApp(...)} function.
#' @param run_path character string pointing to the path of a run to load at startup.
#'   Optional.
#' @param nm_exe_path NONMEM executable path
#' @param nmcheck_exe_path NMTRAN check executable path
#'
#' @return A Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' nonmem_toolbox()
#' }
nonmem_toolbox <- function(run_path = NULL,
                           nonmem_exe = NULL,
                           nonmem_call = NULL,
                           nmcheck_exe = NULL,
                           nmcheck_call = NULL, ...){
  toolbox_dir <- system.file("popkinr/nonmem_toolbox", package = "popkinr")

  if(!is.null(run_path) && str_detect(run_path, "(\\.tar\\.gz|\\.zip|\\.tgz)$")){
    run_path <- normalizePath(run_path)

    Sys.setenv(STARTUP_PATH = run_path)
  }

  if(!is.null(nonmem_exe))
    Sys.setenv(NM_EXE = nonmem_exe)

  if(!is.null(nonmem_call))
    Sys.setenv(NM_CALL = nonmem_call)

  if(!is.null(nmcheck_exe))
    Sys.setenv(NMCHECK_EXE = nmcheck_exe)

  if(!is.null(nmcheck_call))
    Sys.setenv(NMCHECK_CALL = nmcheck_call)

  runApp(appDir = toolbox_dir, ...)
}
