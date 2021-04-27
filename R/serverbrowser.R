#' @rdname serverbrowser
#'
#' @param id UI element id
#'
#' @export
#'
#' @examples
serverBrowserUI <- function(id, max_height = "500px") {
  ns <- NS(id)

  tagList(
    uiOutput(ns("folder_shortcuts")),
    uiOutput(ns("dir_path")),
    div(id = "browser_container",
        style = sprintf("overflow: auto; max-height:%s;", max_height),
    shinyTree(ns("tree"),
    types = "
    {
    'directory': {'icon': 'fa fa-archive'},
    'directory-highlight': {'icon': 'fa fa-archive', 'a_attr' : { 'style' : 'font-weight:bold' }},
    'file':{'icon': 'fa fa-file'},
    'file-highlight': {'icon': 'fa fa-file', 'a_attr' : { 'style' : 'color:red' }}
    }"))
  )
}


#' @name serverbrowser
#' @title Server browser tree module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param root_directory character. Path of the root directory to browse from.
#'   Defaults to root directory.
#' @param initial_selection optional character. Path of the selected folder at
#'   start.
#' @param dir_highlight optional character or function. If character, regular
#'   expression matching directory elements to highlight. If function, one
#'   argument function used to determine if the element should be highlighted.
#' @param file_highlight optional character or function. If character, regular
#'   expression matching file elements to highlight. If function, one
#'   argument function used to determine if the element should be highlighted.
#'
#' @return A shiny module
#'
#' @export
#'
serverBrowser <- function(input, output, session, root_directory = "/",
                             initial_selection = NULL, dir_highlight = NULL, file_highlight = NULL, folder_shortcuts = NULL) {

  root_dir <- root_directory

  rvx <- reactiveValues(selection = list(is_dir = is_dir(initial_selection),
                                         path = initial_selection),
                        refresh = Sys.time())

  list_files <- function(path){
    dirlist <- dir_ls(path, type = "directory", recurse = FALSE, full.names = TRUE)

    if(.Platform$OS.type != "windows"){
      dirs <- dir_ls(path, type = "directory", recurse = FALSE, full.names = TRUE) %>% path_real()
      allFiles <- dir_ls(path, type = "file") %>% path_real()
    } else {
      # browser()
      dirs <- dir_ls(path, type = "directory", recurse = FALSE, full.names = TRUE) %>% path_expand() %>% path_norm()
      allFiles <- dir_ls(path, type = "file") %>% path_expand() %>% path_norm()
    }
    files <- setdiff(allFiles, dirs)
    np <- path_real(path)

    selected_path <- rvx$selection$path

    files_str <- lapply(files, function(x){
      structure(basename(x),
                sttype="file",
                stpath = x,
                stselected = identical(x, selected_path))
    } )

      subtree <- append(lapply(dirs, function(nextDir){
        structure(nextDir,
                  sttype = "directory",
                  stpath = nextDir,
                  stselected = identical(nextDir, selected_path))
      }), values = files_str)

      if(path != root_dir){
        dirs <- c("..", dirs)

        subtree <- append(list(structure("..",
                                         stpath = np,
                                         sttype = "directory")),
                          values = subtree)
      }

      names(subtree) <- basename(append(dirs, files))

    subtree
  }

  output$dir_path <- renderUI({
    selected_dir <- ifelse(rvx$selection$is_dir, rvx$selection$path, dirname(rvx$selection$path))

    tags$strong(selected_dir)
  })

  output$tree <- renderTree({
    rvx$refresh

    selected_dir <- ifelse(rvx$selection$is_dir, rvx$selection$path, dirname(rvx$selection$path))

    tree <- list_files(selected_dir)

    if(!is.null(dir_highlight)){
      tree <- tree %>%
        modify_if(
          .p = function(x){
            if(attr(x, "sttype") != "directory") return(FALSE)

            if(is.function(dir_highlight)){
              dir_highlight(x)
            } else {
              str_detect(basename(x), dir_highlight)
            }
          },
          .f = function(x){
            attr(x, "sttype") <- "directory-highlight"
            x
          })
    }

    if(!is.null(file_highlight)){
      tree <- tree %>%
        modify_if(
          .p = function(x){
            if(attr(x, "sttype") != "file") return(FALSE)

            if(is.function(file_highlight)){
              file_highlight(x)
            } else {
              str_detect(basename(x), file_highlight)
            }
          },
          .f = function(x){
            attr(x, "sttype") <- "file-highlight"
            x
          })
    }

    tree
  })

  observeEvent(input$tree,
               {
                 node <- get_selected(input$tree)
                 req(length(node)> 0)
                 sel <- node[[1]]

                 sel_path <- ifelse(sel == "..",
                                    dirname(attr(sel, "stpath")),
                                    attr(sel,"stpath"))

                 rvx$selection <- list(is_dir = dir.exists(sel_path),
                                      path = sel_path)
               })


  inner_folder_shortcuts <- reactive({
    if(is.null(folder_shortcuts)) return(NULL)

    folder_shortcuts()
  })

  output$folder_shortcuts <- renderUI({
    folders <- req(inner_folder_shortcuts())

    folders_paths <- folders %>% path_real() %>% unique()

    selected_dir <- isolate(rvx$selection$path)

    if(is.null(selected_dir) || !dir.exists(selected_dir))
      selected_dir <- initial_selection

    default_root <- ifelse(.Platform$OS.type == "windows", "C:/", "/")

    selectInput(session$ns("folder_shortcuts"),
                "Jump to folder",
                choices = c(default_root, sort(folders_paths)),
                selected = selected_dir,
                width = "100%")
  })

  observeEvent(input$folder_shortcuts, {
    shortcut_dir <- input$folder_shortcuts

    if(dir.exists(shortcut_dir)){
      rvx$selection = list(is_dir = is_dir(shortcut_dir),
                           path = shortcut_dir)
    }
  })


  return(reactive({
    list(folder = ifelse(rvx$selection$is_dir, rvx$selection$path, NA_character_),
         file = ifelse(!rvx$selection$is_dir, rvx$selection$path, NA_character_),
         reset = function(selection = NULL){
           selection <- ifelse(is.null(selection), initial_selection, selection)

           rvx$selection = list(is_dir = is_dir(selection),
                                path = selection)
         },
         initialize_ui = function(){
             rvx$refresh <- Sys.time()
         })}))
}
