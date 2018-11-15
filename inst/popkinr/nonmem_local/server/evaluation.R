observeEvent(input$browse_evaluation_run, {
  evaluation_run_browser()$initialize_ui(force = TRUE)

  showModal(modalDialog(
    title = "Select a run folder or archive",
    size = "l",
    popkinr::serverBrowserUI("evaluation_run_browser"),
    div(tags$em("* Legend: Bold: directories containing *.tar.gz archive files; Red: directories containing NONMEM run data")),
    footer = list(modalButton("Close"),
                  actionButton("load_evaluation_run", "Load selection")),
    easyClose = TRUE)
  )
})

observeEvent(input$load_evaluation_run, {
  if(length(evaluation_run_browser()$file) == 1){
    rv$evaluation_run_path <- evaluation_run_browser()$file
  } else {
    rv$evaluation_run_path <- evaluation_run_browser()$folder
  }

  removeModal()
})

evaluation_run <- reactive({
  arch <- req(rv$evaluation_run_path)

  safe_load <- safely(load_nm_run)


  withProgress({
    run <- safe_load(arch, load_tables = FALSE)

    setProgress(1, detail = "Done !")
  }, value = 0.3, message = "Loading run...")

  if(!is.null(run$error)){
    toastr_error(message = run$error$message,
                 title = "Loading error",
                 timeOut = 5000,
                 position = "bottom-right",
                 closeButton = TRUE)

    return(NULL)
  }

  run$result
})

output$evaluation_run_info <- renderText({
  req(rv$evaluation_run_path)
})

updateProgress <- function(value = NULL, detail = NULL) {
  if (is.null(value)) {
    value <- progress$getValue()
    value <- value + (progress$getMax() - value) / 5
  }
  progress$set(value = value, detail = detail)
}


output$chain_df <- renderUI({
  run <- req(evaluation_run())

  omega <- run$control_stream$parameters$initialization$OMEGA
  n_df <- 1

  if(!is.null(omega))
    n_df <- nrow(omega)

  numericInput("chain_df", "Degrees of freedom", value = n_df)
})

start_chain <- function() {
  ctype <- input$chain_ctype
  iaccept <- req(input$chain_iaccept)
  n_sample <- req(input$chain_nsample)
  seed <- req(input$chain_seed)
  degrees_of_freedom <- req(input$chain_df)

  nodes <- as.numeric(input$nodes)

  run <- evaluation_run()

  plan(multiprocess)

  progress <- shiny::Progress$new(session)
  progress$set(message = "Generating $CHAIN control files", value = 0)
  on.exit(progress$close())

  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }

  chain_cs_files <- nm_chain(run,
                             n_sample = n_sample,
                             iaccept = iaccept,
                             ctype = ctype,
                             degrees_of_freedom = degrees_of_freedom,
                             seed = seed,
                             target_directory = str_c(dirname(run$info$path),
                                                      str_c("initial.", run$info$run_name),
                                                      sep = "/"),
                             update_progress = updateProgress)

  seq_along(chain_cs_files) %>%
    walk(function(i){
      x <- chain_cs_files[i]

      run_tmp_dir <- str_c(local_app_folder, "tmp", str_c("pmxecute_", tools::file_path_sans_ext(basename(x))), sep = "/")

      run_args <- list(control_file = x,
                       nonmem_exe = env_nm_exe,
                       call_template = env_nm_call,
                       run_directory = run_tmp_dir,
                       n_nodes = nodes,
                       parafile = env_nm_parafile_path,
                       result_directory = str_c(dirname(x), "../results", sep = "/"),
                       archive = TRUE,
                       cleanup = TRUE,
                       quiet = TRUE)

      rv$run_queue$enqueue(run_args,
                           start_message = sprintf("$CHAIN %s, ID:%s (%s %s)",
                                                   basename(x), i, nodes, ifelse(nodes > 1, "CPUs", "CPU")))
    })
}

start_bootstrap <- function() {
  n_sample <- req(input$bootstrap_nsample)
  seed <- req(input$bootstrap_seed)

  nodes <- as.numeric(input$nodes)

  run <- evaluation_run()

  plan(multiprocess)

  progress <- shiny::Progress$new(session)
  progress$set(message = "Generating bootstrap data", value = 0)
  on.exit(progress$close())

  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }

  bootstrap_cs_files <- nm_bootstrap(run,
                                     n_sample = n_sample,
                                     seed = seed,
                                     target_directory = str_c(dirname(run$info$path),
                                                              str_c("bootstrap.", run$info$run_name),
                                                              sep = "/"),
                                     update_progress = updateProgress)

  seq_along(bootstrap_cs_files) %>%
    walk(function(i){
      x <- bootstrap_cs_files[i]

      run_tmp_dir <- str_c(local_app_folder, "tmp", str_c("pmxecute_", tools::file_path_sans_ext(basename(x))), sep = "/")

      run_args <- list(control_file = x,
                       nonmem_exe = env_nm_exe,
                       call_template = env_nm_call,
                       run_directory = run_tmp_dir,
                       n_nodes = nodes,
                       parafile = env_nm_parafile_path,
                       result_directory = str_c(dirname(x), "../results", sep = "/"),
                       archive = TRUE,
                       cleanup = TRUE,
                       quiet = TRUE)

      rv$run_queue$enqueue(run_args,
                           start_message = sprintf("Bootstrap %s, ID:%s (%s %s)",
                                                   basename(x), i, nodes, ifelse(nodes > 1, "CPUs", "CPU")))
    })
}

start_jacknife <- function(){
  nodes <- as.numeric(input$nodes)

  run <- evaluation_run()

  plan(multiprocess)

  progress <- shiny::Progress$new(session)
  progress$set(message = "Generating jacknife data", value = 0)
  on.exit(progress$close())

  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }

  jacknife_cs_files <- nm_jacknife(run,
                                   target_directory = str_c(dirname(run$info$path),
                                                            str_c("jacknife.", run$info$run_name),
                                                            sep = "/"),
                                   update_progress = updateProgress)

  seq_along(jacknife_cs_files) %>%
    walk(function(i){
      x <- jacknife_cs_files[i]

      run_tmp_dir <- str_c(local_app_folder, "tmp", str_c("pmxecute_", tools::file_path_sans_ext(basename(x))), sep = "/")

      run_args <- list(control_file = x,
                       nonmem_exe = env_nm_exe,
                       call_template = env_nm_call,
                       run_directory = run_tmp_dir,
                       n_nodes = nodes,
                       parafile = env_nm_parafile_path,
                       result_directory = str_c(dirname(x), "../results", sep = "/"),
                       archive = TRUE,
                       cleanup = TRUE,
                       quiet = TRUE)

      rv$run_queue$enqueue(run_args,
                           start_message = sprintf("Jacknife %s, ID:%s (%s %s)",
                                                   basename(x), i, nodes, ifelse(nodes > 1, "CPUs", "CPU")))
    })
}

