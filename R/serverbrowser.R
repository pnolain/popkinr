#' Server Browser Module UI
#'
#' @param id
#' @param max_height
#'
#' @return
#' @export
#'
#' @examples
serverBrowserUI <- function(id, max_height = "600px"){

  ns <- NS(id)

  shinyjs::inlineCSS("#browser_container.waiting, #browser_container.waiting * { cursor: wait; }")

  div(
    fluidRow(
      column(3,
             selectInput(ns("sorting"), "Sort files by", choices = c("Date" = "time", "Name" = "name"))),
      column(9,
             uiOutput(ns("folder_shortcuts")))),
    div(id = "browser_container",
        style = sprintf("overflow: auto; max-height:%s;", max_height),
        shinyTree(ns("browsing_tree")))
  )
}


#' Server Browser Module
#'
#' @param input
#' @param output
#' @param session
#' @param root_directory
#' @param initial_selection
#' @param file_extensions
#' @param folder_shortcuts
#' @param formatting_function
#'
#' @return
#' @export
#'
#' @examples
serverBrowser <- function(input, output, session, root_directory,
                          initial_selection = NULL, file_extensions = NULL, folder_shortcuts = NULL,
                          formatting_function = NULL){

  file_pattern <- sprintf("\\.(%s)$", paste(str_replace(file_extensions, "\\.", "\\\\."), collapse = "|"))

  is_windows <- (tolower(Sys.info()["sysname"]) == "windows")

  if(is_windows){
    win_disks <- system("wmic logicaldisk get name", intern = TRUE) %>%
      str_subset("^[B-Z]:") %>%
      str_trim()
  }

  rv <- reactiveValues(inner_root_directory = NULL,
                       parent_directory = NULL,
                       file_selection = NULL,
                       folder_selection = initial_selection,
                       prev_data = NULL,
                       refresh_flag = Sys.time(),
                       force_scroll = Sys.time())

  # Tree is initialized without nodes
  output$browsing_tree <- renderEmptyTree()

  outputOptions(output, "browsing_tree", suspendWhenHidden = FALSE)

  # An observer is used to trigger a tree update with new data.
  observe_stuff <- function(){
    rv$inner_root_directory <- ifelse(is.reactive(root_directory),
                                      root_directory(),
                                      root_directory)

    if(is.null(rv$parent_directory)){
      rv$parent_directory <- ifelse(is.null(rv$folder_selection), rv$inner_root_directory, rv$folder_selection)
    }

    updated_browsing_tree <- json_tree()

    if(!(identical(rv$prev_data, updated_browsing_tree))){
      updateTree(session, "browsing_tree", updated_browsing_tree)
      rv$prev_data <-updated_browsing_tree
    }
  }

  safe_observe <- purrr::safely(observe_stuff)

  observe({

    # session$sendCustomMessage(type = "show_waiting_cursor", message = "browser_container")
    shinyjs::runjs("$('#browser_container').addClass('waiting');")

    so <- safe_observe()

    if(!is.null(so$error)){
      print(so$error)
      print(so$error$message)
    }

    shinyjs::runjs("$('#browser_container').removeClass('waiting');")
    # session$sendCustomMessage(type = "hide_waiting_cursor", message = "browser_container")
  })

  browsing_tree <- reactive({
    rv$refresh_flag
    # current selection
    selection <- rv$parent_directory %>% normalizePath

    # return if selection is a file
    if(str_detect(selection, file_pattern)) return(NULL)

    if(is.null(input$sorting))
      return(NULL)

    sort_type <- input$sorting

    time_sorted <- (sort_type == "time")

    # chain of directory from root to selection
    dir_chain <- selection

    if(is_windows){
      prev_dir <- NULL
      while(!(((fc <- first(dir_chain)) %in% c("/", normalizePath(rv$inner_root_directory)))) &&
            fc != dirname(fc)){
        prev_dir <- dirname(fc)
        dir_chain <- c(prev_dir, dir_chain)
      }

      dir_chain <- normalizePath(dir_chain)
    } else {
      while(!(first(dir_chain) %in% c("/", normalizePath(rv$inner_root_directory)))){
        dir_chain <- c(dirname(first(dir_chain)), dir_chain)
      }
    }

    sub_tree <- function(parent, level = 1) {
      sub_dirs <- list.files(parent, recursive = FALSE, full.names = TRUE) %>%
        .[which(dir.exists(.))] %>% # remove symbolik links
        normalizePath()

      # directories dataframe
      d_df <- seq_along(sub_dirs) %>%
        map_df(function(i){
          sub_dir <- sub_dirs[i]

          children <- NULL
          if(sub_dir %in% dir_chain) {
            children <- sub_tree(sub_dir, level = level + 1)
          }

          label <- basename(sub_dir)

          d_df <- tibble(id = sub_dir,
                         html_id = sprintf("j%s_%s", level, i),
                         text = label,
                         state = "FLAG",
                         children = list(children))

          if(!is.null(formatting_function)){
            d_df <- d_df %>%
              mutate(text = pmap_chr(list(id = id, text = text), formatting_function))
          }

          if(sub_dir == rv$folder_selection){
            d_df$state = "FLAG2"
          }

          d_df
        })

      # if current parent is selected, show files matching file pattern
      if(parent %in% dir_chain) {

        dir_files <- list.files(parent, full.names = TRUE, pattern = file_pattern) %>%
          .[which(file.exists(.))] %>% # remove symbolik links
          normalizePath() %>%
          setdiff(sub_dirs)

        if(length(dir_files) > 0 & time_sorted){
          time_info <- map_df(dir_files, function(x) tibble(file = x, time = file.info(x)$ctime)) %>%
            arrange(desc(time))

          dir_files <- time_info$file
        }

        f_df <- map_df(dir_files, function(x){
          tibble(id = x,
                 text = basename(x),
                 state = "FLAG",
                 icon = FALSE)
        }) %>% as_tibble()

        return(bind_rows(d_df, f_df))
      } else {
        d_df
      }
    }

    # start the tree from root
    tree <- sub_tree(dir_chain[1])

    if(is_windows && rv$inner_root_directory == "/"){

      tree2 <- tibble(id = str_c(win_disks, "\\"),
                      html_id = str_c("j0_", seq_along(id)),
                      text = win_disks,
                      state = "FLAG",
                      children = map(id, function(x){
                        if(x == normalizePath(dirname(tree[1,]$id)))
                          return(tree)

                        NA
                      }))

      tree <- tree2

    }


    tree
  })

  json_tree <- reactive({

    df_tree <- browsing_tree()

    if(is.null(df_tree) || nrow(df_tree) == 0)
      return(NULL)

    # return a JSON string (with 2 hacks for correct display)
    res <- toJSON(df_tree) %>%
      str_replace_all("\"FLAG2\"", "{\"opened\":true, \"selected\":true}") %>% # hack 1
      str_replace_all("\"FLAG\"", "{\"opened\":true}") %>% # hack 2
      str_replace_all(",\"children\":\\[null\\]", "") # hack 3

    res
  })

  observeEvent(input$browsing_tree, {
    sel <- get_selected(input$browsing_tree, format = "classid")

    req(length(sel) > 0)

    selected_items <- map_chr(sel, function(x) attr(x, "id"))

    items_df <- tibble(path = selected_items,
                       type = ifelse(str_detect(path, file_pattern), "file", "directory"))

    file <- items_df %>% filter(type == "file")

    rv$file_selection <- file$path

    dir_nodes <- items_df %>% filter(type == "directory")

    if(nrow(dir_nodes) > 0){
      rv$parent_directory <- dir_nodes$path[1]
      rv$folder_selection <- dir_nodes$path[1]
    }
  })

  inner_folder_shortcuts <- reactive({
    if(is.null(folder_shortcuts)) return(NULL)

    folder_shortcuts()
  })

  output$folder_shortcuts <- renderUI({
    folders <- req(inner_folder_shortcuts())

    folders_paths <- folders[dir.exists(folders)] %>% normalizePath() %>% unique()

    selectInput(session$ns("folder_shortcuts"),
                "Jump to folder",
                choices = c("/", sort(folders_paths)),
                selected = isolate(rv$folder_selection),
                width = "100%")
  })

  observeEvent(input$folder_shortcuts, {
    selected_dir <- input$folder_shortcuts

    if(dir.exists(selected_dir)){
      rv$file_selection <- NULL
      rv$parent_directory <- selected_dir
      rv$folder_selection <- selected_dir

      rv$force_scroll <- Sys.time()
    }
  })

  observeEvent(input$browsing_tree, {
    # Scroll to directory item when tree is updated
    if(!is.null(rv$force_scroll) && rv$folder_selection != "/"){

      shinyjs::runjs(sprintf("var id = '%s'; var elem = document.getElementById(id);
                             if(elem !== null) elem.scrollIntoView();", rv$folder_selection))

      # shinyjs::runjs(sprintf("$('#' + %s).scrollIntoView();", rv$folder_selection))

      rv$force_scroll <- NULL
    }
  })

  return(reactive({
    list(folder = rv$folder_selection,
         file = rv$file_selection,
         reset = function(parent_directory = NULL, folder_selection = NULL, file_selection = NULL){
           rv$inner_root_directory <- NULL
           rv$parent_directory <- parent_directory
           rv$file_selection <- file_selection
           rv$folder_selection <- folder_selection
         },
         initialize_ui = function(force = FALSE){
           rv$prev_data <- NULL

           if(force){
             rv$refresh_flag <- Sys.time()
           }
         })}))

}
