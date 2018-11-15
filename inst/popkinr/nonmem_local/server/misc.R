observeEvent(input$browse_misc_run, {
  misc_run_browser()$initialize_ui(force = TRUE)

  showModal(modalDialog(
    title = "Select a run folder or archive",
    size = "l",
    popkinr::serverBrowserUI("misc_run_browser"),
    div(tags$em("* Legend: Bold: directories containing *.tar.gz archive files; Red: directories containing NONMEM run data")),
    footer = list(modalButton("Close"),
                  actionButton("load_misc_run", "Load selection")),
    easyClose = TRUE)
  )
})

observeEvent(input$load_misc_run, {
  if(length(misc_run_browser()$file) == 1){
    rv$misc_run_path <- misc_run_browser()$file
  } else {
    rv$misc_run_path <- misc_run_browser()$folder
  }

  removeModal()
})

misc_run <- reactive({
  arch <- req(rv$misc_run_path)

  withProgress({
    run <- load_nm_run(arch, load_tables = FALSE)

    setProgress(1, detail = "Done !")
  }, value = 0.3, message = "Loading run...")

  run
})

output$misc_run_info <- renderText({
  req(rv$misc_run_path)
})

prior_control_stream <- reactive({
  run <- req(misc_run())

  df_formula <- as.integer(input$prior_df_formula)

  safe_prior <- safely(nm_prior)

  prior_text <- safe_prior(run, df_formula = df_formula)

  if(!is.null(prior_text$error))
    return(str_c("An error occured while trying to generate the control stream file:\n\n", prior_text$error$message))

  prior_text$result
})

simulation_control_stream <- reactive({
  run <- req(misc_run())

  safe_sim <- safely(nm_simulation)

  sim_text <- safe_sim(run,
                       n_simulations = input$simulation_n_sample,
                       seed = input$simulation_seed)

  if(!is.null(sim_text$error))
    return(str_c("An error occured while trying to generate the control stream file:\n\n", sim_text$error$message))

  sim_text$result
})

observe({
  cs <- NULL

  if(input$misc_method == "prior"){
    cs <- req(prior_control_stream())
  } else {
    cs <- req(simulation_control_stream())
  }

  shinyAce::updateAceEditor(session, "misc_control_file_editor", value = cs)
})

output$download_generated_cs <- downloadHandler(filename = function(){
  run_name <- tools::file_path_sans_ext(basename(rv$misc_run_path), compression = TRUE)

  str_c(run_name, ".", ifelse(input$misc_method == "prior", "prior", "simulation"), ".ctl")
},
content = function(file){
  cs_code <- req(input$misc_control_file_editor)

  write_lines(cs_code, path = file)
})
