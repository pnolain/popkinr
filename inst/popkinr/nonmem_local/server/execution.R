check_trigger <- makeReactiveTrigger()
submission_trigger <- makeReactiveTrigger()
local_list_trigger <- makeReactiveTrigger()

observeEvent(input$browse_control_file, {
  control_file_browser()$initialize_ui(force = TRUE)

  showModal(modalDialog(
    title = "Select control file(s)",
    size = "l",
    popkinr::serverBrowserUI("control_file_browser"),
    footer = list(modalButton("Close"),
                  actionButton("load_control_file", "Load selection")),
    easyClose = TRUE)
  )
})

observeEvent(input$load_control_file, {
  new_cs_files <- req(control_file_browser()$file)

  add_cs_files(new_cs_files)

  removeModal()
})

output$results_subdir_name <- renderUI({
  textInput("results_subdir_name", "Results subdirectory name", Sys.getenv("RUN_RESULTS_SUBDIR"))
})

add_cs_files <- function(my_files){

  current_cs <- rv$control_files
  current_cs_files <- current_cs$filepath

  n_cs <- length(current_cs_files)

  files <- setdiff(my_files, current_cs_files)

  df <- tibble(id = n_cs + seq_along(files),
               filepath = files,
               last_edition = file.mtime(files),
               parsing = list(NULL),
               nmtran_test = list(NULL),
               error = NA)

  if(!is.null(rv$control_files))
    df <- bind_rows(rv$control_files, df)

  rv$control_files <- df
}


observe({
  input$perform_nmcheck
  check_trigger$depend()

  if(file.exists(env_nmcheck_exe)){
    isolate({
      # Perform a NMTRAN check only if control file was edited
      df <- req(rv$control_files) %>%
        mutate(to_update = pmap_lgl(list(filepath, last_edition, parsing),
                                    function(x, y, z) (length(z) == 0 || file.mtime(x) > y)))

      if(any(df$to_update)){
        i_rows <- which(df$to_update)
        n_tu <- length(i_rows)


        selected_cs <- control_file_detail()

        withProgress({

          safe_parse <- safely(parse_nm_control_stream)
          go_check <- function(cs_path, cs_data){
            res <- NULL

            if(!is.null(cs_data$error)){ # if pmxploit parsing error
              list(path = cs_path,
                   error = NA,
                   message = cs_data$error$message)
            }

            if(file.access(dirname(cs_path), mode = 2) == -1){
              if(is.null(cs_data$result$dataset_file))
                return(NULL)

              dataset <- paste(dirname(cs_path), cs_data$result$dataset_file, sep = "/")

              run_files <- c(cs_path, dataset)

              temp_path <- paste(app_temp_directory, basename(run_files), sep = "/")
              file.copy(run_files, temp_path)

              check <- nm_check(control_file = temp_path[1],
                                nmcheck_exe = env_nmcheck_exe,
                                call_template = env_nmcheck_call)

              file.remove(temp_path)

              res <- check
            } else {
              res <- nm_check(control_file = cs_path,
                              nmcheck_exe = env_nmcheck_exe,
                              call_template = env_nmcheck_call)
            }

            if(!is.null(selected_cs) && selected_cs$filepath == cs_path){
              if(res$error){


                toastr_error(message = "An error was detected, see NM-TRAN check message", title = "NM-TRAN check",
                             timeOut = 3000,
                             position = "top-center", closeButton = TRUE)
              } else {
                toastr_success(message = "Done !", title = "NM-TRAN check",
                               timeOut = 1000,
                               position = "top-center", closeButton = TRUE)
              }
            }

            incProgress(amount = (1 - 0.1) / n_tu, detail = basename(cs_path))

            res
          }

          for(i in i_rows){
            df[i,]$parsing <- map(df[i,]$filepath, safe_parse)
            df[i,]$nmtran_test <-  map2(df[i,]$filepath, df[i,]$parsing, go_check)
            df[i,]$last_edition <- file.mtime(df[i,]$filepath)
          }
        }, message = "Performing NM-TRAN test", value = 0.1)
      }

      df <- df %>%
        mutate(error = map_lgl(nmtran_test, "error"))

      rv$control_files <- select(df, -to_update)
    })

  }

})


observeEvent(input$start_run, {

  if(input$run_box == "run"){
    start_run()

    rv$control_files <- NULL
    rv$selected_cs_id <- NULL
  } else {
    if(input$evaluation_method == "chain"){
      start_chain()
    } else if(input$evaluation_method == "bootstrap"){
      start_bootstrap()
    } else if(input$evaluation_method == "jacknife"){
      start_jacknife()
    }
  }

  local_list_trigger$trigger()
})

start_run <- function(){

  nodes <- as.integer(input$nodes)
  res_subdir_name <- req(input$results_subdir_name)

  cs_files <- req(rv$control_files)

  plan(multiprocess)

  cs_files %>%
    filter(!error | is.na(error)) %>%
    pull(filepath) %>%
    walk(function(x){

      run_tmp_dir <- str_c(env_execution_dir, "tmp", str_c("pmxecute_", tools::file_path_sans_ext(basename(x))), sep = "/")

      run_args <- list(control_file = x,
                       nonmem_exe = env_nm_exe,
                       call_template = env_nm_call,
                       n_nodes = nodes,
                       run_directory = run_tmp_dir,
                       parafile = env_nm_parafile_path,
                       result_directory = str_c(dirname(x), res_subdir_name, sep = "/"),
                       archive = input$compress,
                       cleanup = input$cleanup,
                       quiet = TRUE)

      rv$run_queue$enqueue(run_args,
                           start_message = sprintf("Queue %s (%s %s)", basename(x), nodes, ifelse(nodes > 1, "CPUs", "CPU")),
                           end_message = sprintf("Dequeue %s (%s %s)", basename(x), nodes, ifelse(nodes > 1, "CPUs", "CPU")))
    })
}

output$control_files_table <- renderDataTable({

  df <- tibble(id = integer(), filepath = character(), error = character())

  if(!is.null(rv$control_files)) {
    df <- rv$control_files %>%
      select(-nmtran_test, -parsing) %>%
      mutate(details = sprintf("<a href=\"#/\" onclick=\"show_cs_details('%s');\">Details</a>", id),
             error = ifelse(error, "Yes", "No"),
             remove = sprintf("<a href=\"#/\" onclick=\"remove_cs('%s');\">Remove</a>", id))
  }

  dt <- datatable(df,
                  escape = FALSE,
                  selection = "none",
                  options = list(paging = FALSE, scrollX = TRUE, dom = "rt", language = list(emptyTable = "No control file")),
                  rownames = FALSE) %>%
    formatStyle("error",
                target = "row",
                backgroundColor = styleEqual(levels = c("Yes", "No"),
                                             values = c("#F96969", "#89D3FF")))
  if(!is.null(rv$selected_cs_id)){
    dt <- dt %>%
      formatStyle("id",
                  target = "row",
                  fontWeight = styleEqual(levels = rv$selected_cs_id,
                                          values = "bold"))
  }

  dt
})


observeEvent(input$show_cs_details, {
  js <- jsonlite::fromJSON(input$show_cs_details)

  rv$selected_cs_id <- js$cs_id
# })
#
# observeEvent(rv$selected_cs_id, {

  selected_cs <- req(control_file_detail())

  callModule(controlStreamUIModule, "control_file_parsing", session = session, control_file =
               selected_cs$parsing[[1]]$result)

  showModal(modalDialog(title = str_c("Control stream: ", selected_cs$filepath),
                        size = "l",
                        fluidRow(
                          box(width = 12, title = "Code edition",
                              collapsible = TRUE,
                              collapsed = TRUE,
                              shinyAce::aceEditor("control_file_editor",
                                                  height = "1000px"),
                              div(align = "center",
                                  actionButton("save_control_file", "Save"))
                          ),
                          box(width = 12, title = "NM-TRAN check",
                              collapsible = TRUE,
                              collapsed = (!is.na(selected_cs$error) && !selected_cs$error),
                              verbatimTextOutput("nmtran_errors")),
                          box(width = 12, title = "Model details",
                              collapsible = TRUE,
                              controlStreamUI("control_file_parsing"))
                        ),
                        footer = modalButton("Close"),
                        easyClose = TRUE))

  tryCatch({
    # read raw file for preventing bug with aceEditor with non utf-8 characters
    fullraw <- readr::read_file_raw(selected_cs$filepath)

    chars_to_skip <- which(!map_lgl(fullraw, stringi::stri_enc_isutf8))

    # removing non utf-8 character
    if(length(chars_to_skip) > 0){
      fullraw <- fullraw[-chars_to_skip]
    }

    shinyAce::updateAceEditor(session, "control_file_editor", value = rawToChar(fullraw))
    # shinyAce::updateAceEditor(session, "control_file_editor", value = read_file(selected_cs$filepath))
  })

})


observeEvent(input$remove_cs, {
  js <- jsonlite::fromJSON(input$remove_cs)

  cs_files <- req(rv$control_files)

  cs <- cs_files %>%
    filter(id == js$cs_id)

  rv$control_files <- rv$control_files %>%
    filter(filepath != cs$filepath)

  if(!is.null(rv$selected_cs_id)){
    if(rv$selected_cs_id == js$cs_id){
      rv$selected_cs_id <- NULL
    } else if(rv$selected_cs_id > js$cs_id){
      rv$selected_cs_id <- rv$selected_cs_id - 1
    }
  }
})



control_file_detail <- reactive({
  cs_files <- req(rv$control_files)

  if(nrow(cs_files) == 0)
    return(NULL)

  if(is.null(rv$selected_cs_id))
    return(NULL)

  cs_id <- rv$selected_cs_id

  cs_files %>%
    filter(id == cs_id)
})

observe({
  can_run <- FALSE

  if(input$run_box == "run"){
    can_run <- ((!is.null(rv$control_files) && nrow(filter(rv$control_files, !error | is.na(error))) > 0) &&
                  (!is.null(input$results_subdir_name) && input$results_subdir_name != ""))
  } else {
    can_run <- !is.null(evaluation_run())
  }

  shinyjs::toggleState("start_run", condition = can_run)
})

observe({
  selected_cs <- control_file_detail()

  has_rights <- (!is.null(selected_cs) && unname(file.access(selected_cs$filepath, mode = 2)) == 0)
  shinyjs::toggleState("save_control_file", condition = has_rights)
  shinyjs::toggleState("control_file_editor", condition = has_rights)
})

observeEvent(input$save_control_file, {
  selected_cs <- req(control_file_detail())

  txt <- input$control_file_editor

  write_file(txt, path = selected_cs$filepath)

  check_trigger$trigger()

  # removeModal()
})

observeEvent(input$clear_control_file_selection, {
  rv$control_files <- NULL
  rv$selected_cs_id <- NULL
})

output$nmtran_errors <- renderText({
  selected_cs <- req(control_file_detail())

  msg <- selected_cs$nmtran_test[[1]]$message

  str_trim(msg)
})
