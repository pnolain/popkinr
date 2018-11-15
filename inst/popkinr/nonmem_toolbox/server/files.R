
  # files tabs----
  output$control_stream_raw_content <- renderPrint({
    run <- req(rv$run)

    writeLines(str_c(run$control_stream$code, collapse = "\n"))
  })


  output$control_stream_parsed_content <- renderPrint({
    run <- req(rv$run)

    str(run$control_stream)
  })

  output$report_content <- renderText({
    run <- req(rv$run)

    run$files$report
  })



  # Tables tab----
  output$run_table_selection <- renderUI({

    run <- req(rv$run)

    selectInput("run_table_selection", "Select a table",
                choices = c("Dataset" = "dataset", "pmxploitab (merged table)" = "pmxploitab",
                            run$control_stream$tables$file))
  })

  run_selected_table <- reactive({
    run <- req(rv$run)

    selected_table <- req(input$run_table_selection)

    df <- run$tables[[selected_table]]

    if(input$subject_first_row)
      df <- df %>% group_by(ID) %>% slice(1) %>% ungroup()

    df
  })
