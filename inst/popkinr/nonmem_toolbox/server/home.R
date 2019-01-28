observeEvent(input$select_run, {
    run_browser()$initialize_ui(force = TRUE)

    showModal(modalDialog(
      title = "Select a run folder or archive",
      size = "l",
      popkinr::serverBrowserUI("run_browser"),
      div(tags$em("* Legend: Bold: directories containing *.tar.gz archive files; Red: directories containing NONMEM run data")),
      footer = list(modalButton("Close"),
                    actionButton("load_run", "Load selection")),
      easyClose = TRUE)
    )
  })

observe({
  shinyjs::toggle("summary_box", condition = !is.null(rv$run))
})

  output$run_information <- renderUI({
    run <- req(rv$run)
    info <- run$info

    ui <- list(h5(strong("Problem")),
               tags$pre(info$problem),
               h5(strong("Directory")),
               dirname(rv$run_path),
               h5(strong("Run time")),
               str_c("Start: ", format(info$start_time, "%H:%M:%S %x")),
               br(),
               str_c("End: ", format(info$stop_time, "%H:%M:%S %x")),
               h5(strong("Control file")),
               info$control_stream_file,
               h5(strong("Dataset file")),
               info$dataset_file)

    ui
  })

output$estimations_table <- renderDataTable({
    run <- req(rv$run)

    est_table <- map_df(run$estimations, function(est){
      tibble(Method = est$title,
             Minimization = ifelse(est$failed, "Failed",
                                   ifelse(!est$minimization, "No minimization",
                                          ifelse(est$termination_status == 0, "Successful", "Terminated"))),
             `Final OFV` = est$final_ofv,
             Eigenratio = est$eigenratio,
             Correlation = est$correlation,
             AIC = est$aic,
             BIC = est$bic,
             `CPU` = ifelse(!is.null(est$parallel), est$parallel$nodes, 1),
             `Significant digits` = est$significant_digits,
             `Function evaluations` = est$nfuncevals,
             Messages = ifelse(!is.null(est$termination_messages),
                               paste(est$termination_messages$message, collapse = "<br />"),
                               NA))
    }, .id = NULL) %>%
      mutate_if(is.numeric, ~ round(., 2))

    # Remove empty columns
    est_table <- Filter(function(x){ !all(is.na(x))}, est_table)

    dt <- datatable(est_table, escape = FALSE,
                    options = list(scrollX = TRUE, dom = 'rt'))

    if(nrow(est_table) > 0){
      dt <- dt %>%
      formatStyle("Minimization",
                  target = "row",
                  backgroundColor = styleEqual(levels = c("Failed", "Terminated", "Successful"),
                                               values = c("#F96969", "#F9B269", "#89D3FF")))

      if("Correlation" %in% colnames(est_table)){
        dt <- dt %>%
          formatStyle("Correlation",
                      color = styleEqual("TRUE", values = "red"),
                      fontWeight = styleEqual("TRUE", values = "bold"))
      }
    }

    dt
  })



  output$subjects_box <- renderInfoBox({
    run <- req(rv$run)
    infoBox("Number of subjects", run$info$number_of_subjects, color = "green", icon = icon("users"))
  })

  output$observations_box <- renderInfoBox({
    run <- req(rv$run)
    infoBox("Number of observations", run$info$number_of_observations, color = "green", icon = icon("line-chart"))
  })

  output$duration_box <- renderInfoBox({
    run <- req(rv$run)
    dur <- run$info$duration

    infoBox("Run duration", value = lubridate::seconds_to_period(dur),
            color = "green", icon = icon("hourglass-end"))
  })

  output$cpu_box <- renderValueBox({
    run <- req(rv$run)

    parallel_msg <- ""

    n_nodes <- run$info$nodes

    if(!is.na(n_nodes)){
      parallel_msg <- ifelse(n_nodes > 1,
                             sprintf("%s CPUs", n_nodes),
                             "1 CPU")
    }

    infoBox("Parallel processing", parallel_msg, color = "green", icon = icon("laptop"))
  })

  output$nm_version_box <- renderInfoBox({
    run <- req(rv$run)

    infoBox("NONMEM version", run$info$nm_version, color = "green", icon = icon("laptop"))
  })
