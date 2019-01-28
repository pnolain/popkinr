
  output$estimation_selection <- renderUI({

    run <- req(rv$run)

    estimations <- seq_along(run$estimations)

    est_names <- map(seq_along(run$estimations), function(i){
      est <- run$estimations[[i]]

      sprintf("%s: %s", i, est$title)
    }) %>% unlist

    names(estimations) <- est_names

    selectInput("estimation_selection", "Selection",
                choices = estimations, selected = last(estimations))
  })

  observeEvent(input$estimation_selection, {

    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.numeric
    selected_est <- run$estimations[[estimation_id]]

    rv$selected_estimation <- selected_est
  })


  run_thetas_table <- reactive({
    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.integer()

    list(data = run %>%
           summarize_thetas(estimation_number = estimation_id),
         formatting = function(x){
           datatable(x, extensions = "Buttons",
                     colnames = c("ID" = "id",
                                  "Name" = "name",
                                  "Estimate" = "estimate",
                                  "Standard-error" = "se",
                                  "Relative standard-error" = "rse",
                                  "95% Confidence interval (lower bound)" = "ci_low",
                                  "95% Confidence interval (upper bound)" = "ci_up")) %>%
             formatPercentage("Relative standard-error", 2) %>%
             formatStyle("Relative standard-error", target = "row",
                         backgroundColor = styleInterval(rse_warning_breaks, rse_warning_colours))
         })
  })


  run_omega_table <- reactive({
    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.integer()

    list(data = run %>% summarize_omega(estimation_number = estimation_id),
         formatting = function(x){
           datatable(x, colnames = c("ETA1" = "eta1",
                                     "ETA2" = "eta2",
                                     "Estimate" = "estimate",
                                     "Standard-error" = "se",
                                     "Relative standard-error" = "rse",
                                     "95% Confidence interval (lower bound)" = "ci_low",
                                     "95% Confidence interval (upper bound)" = "ci_up",
                                     "Coefficient of variation" = "cv")) %>%
             formatPercentage("Relative standard-error", 2) %>%
             formatPercentage("Coefficient of variation", 2) %>%
             formatStyle("Relative standard-error", target = "row",
                         backgroundColor = styleInterval(rse_warning_breaks, rse_warning_colours))
         })
  })

  output$omega_matrix_table <- renderRHandsontable({
    o_m <- req(rv$selected_estimation$omega_matrix)

    rhandsontable(o_m, readOnly = TRUE) %>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (row == col & value != 0) {
               td.style.background = '#00A65A';
               } else if (value != 0) {
               td.style.background = '#E6FFE6';
               }
               td.style.color = 'black';
               }")
  })

  output$correlation_matrix_table <- renderRHandsontable({
    o_m <- req(rv$selected_estimation$omega_matrix)

    corr_m <- cov2cor(o_m)

    rhandsontable(corr_m, readOnly = TRUE) %>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (row == col) {
               td.style.background = '#00A65A';
               } else if (value != 0) {
               td.style.background = '#E6FFE6';
               }
               td.style.color = 'black';
               }")
  })

  run_eta_bars_table <- reactive({
    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.integer()

    req(run %>% summarize_etabars(estimation_number = estimation_id))

    list(data = run %>% summarize_etabars(estimation_number = estimation_id),
         formatting = function(eta_bars){
           samp_size <- NULL

           if("n" %in% colnames(eta_bars)) # NONMEM 7.3 and higher
             samp_size <- c("Sample size" = "n")

           datatable(eta_bars,
                     colnames = c("ID" = "id",
                                  "Name" = "name",
                                  "Value" = "value",
                                  "Standard-error" = "se",
                                  samp_size,
                                  "p-value" = "pvalue")) %>%
             formatStyle("p-value", target = "row",
                         backgroundColor = styleInterval(pvalue_warning_breaks, pvalue_warning_colours))
         })
  })

  run_sigma_table <- reactive({
    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.integer()

    list(data = run %>% summarize_sigma(estimation_number = estimation_id),
         formatting = function(x){
           datatable(x, colnames = c("EPSILON1" = "epsilon1",
                                     "EPSILON2" = "epsilon2",
                                     "Estimate" = "estimate",
                                     "Standard-error" = "se",
                                     "Relative standard-error" = "rse",
                                     "95% Confidence interval (lower bound)" = "ci_low",
                                     "95% Confidence interval (upper bound)" = "ci_up")) %>%
             formatPercentage("Relative standard-error", 2) %>%
             formatStyle("Relative standard-error",
                         target = "row",
                         backgroundColor = styleInterval(rse_warning_breaks, rse_warning_colours))
         })
  })

  output$sigma_matrix <- renderRHandsontable({
    run <- req(rv$run)

    s_m <- req(rv$selected_estimation$sigma_matrix)

    rhandsontable(s_m, readOnly = TRUE) %>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (row == col & value != 0) {
               td.style.background = '#00A65A';
               } else if (value != 0) {
               td.style.background = '#E6FFE6';
               }
               td.style.color = 'black';
               }")
  })

  run_eta_shrinkage_table <- reactive({
    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.integer()

    req(run %>% summarize_shrinkage(estimation_number = estimation_id, type = c("ETA", "EBV")))

    list(data = run %>% summarize_shrinkage(estimation_number = estimation_id,
                                            type = c("ETA", "EBV")),
         formatting = function(x){
           df <- x %>%
             spread(type, shrinkage)

           shrinkage_columns <- intersect(colnames(df), c("ETA", "EBV"))

           if("subpop" %in% colnames(df)){
             df <- df %>%
               select(parameter, subpop, one_of(shrinkage_columns))
           } else {
             df <- df %>%
               select(parameter, one_of(shrinkage_columns))
           }

           df <- df %>%
             rename(Parameter = parameter)

           datatable(df) %>%
             formatPercentage(shrinkage_columns, 2) %>%
             formatStyle(names(df)[-1],
                         background = styleColorBar(c(0,1), 'lightblue'),
                         backgroundSize = '98% 88%',
                         backgroundRepeat = 'no-repeat',
                         backgroundPosition = 'center')
         })
  })

  run_eps_shrinkage_table <- reactive({

    run <- req(rv$run)

    estimation_id <- req(input$estimation_selection) %>% as.integer()

    req(run %>% summarize_shrinkage(estimation_number = estimation_id, type = "EPS"))

    list(data = run %>% summarize_shrinkage(estimation_number = estimation_id,
                                            type = "EPS"),
         formatting = function(x){
           df <- x %>%
             select(-type)

           datatable(df,
                     colnames = c("Parameter" = "parameter",
                                  "Shrinkage" = "shrinkage")) %>%
             formatPercentage("Shrinkage", 2)%>%
             formatStyle("Shrinkage",
                         background = styleColorBar(c(0,1), 'lightblue'),
                         backgroundSize = '98% 88%',
                         backgroundRepeat = 'no-repeat',
                         backgroundPosition = 'center')
         })
  })

  output$estimation_box <- renderInfoBox({
    selected_est <- req(rv$selected_estimation)

    infoBox("Estimation method", selected_est$title, color = "green", icon = icon("gear"))
  })

  output$termination_box <- renderValueBox({
    selected_est <- req(rv$selected_estimation)

    if(selected_est$failed) {
      infoBox("Minimization", "Estimation failed", icon = icon("thumbs-down"), color = "black")
    } else {
      if(!selected_est$minimization) {
        infoBox("Minimization", "No minimization", icon = icon("thumb-tack"),
                color = "blue")
      } else {
        successful <- selected_est$termination_status == 0

        if(successful) {
          infoBox("Minimization", "Successful",icon = icon("thumbs-up"),
                  color = "blue")
        }  else {
          msg <- switch(as.character(selected_est$termination_status),
                        "133" = "Rounding errors",
                        "134" = "Rounding errors",
                        paste("Termination status:", selected_est$termination_status))

          infoBox("Minimization", sprintf("Terminated (%s)", msg), icon = icon("thumbs-down"),
                  color = "red")
        }
      }
    }
  })

  output$parallel_box <- renderValueBox({
    selected_est <- req(rv$selected_estimation)

    parallel_msg <- "1 CPU"

    if(!is.null(selected_est$parallel)){
      parallel_msg <- sprintf("%s nodes", selected_est$parallel$nodes)
    }

    infoBox("Computing resources", parallel_msg, color = "green", icon = icon("laptop"))
  })

  output$ofv_box <- renderInfoBox({
    selected_est <- req(rv$selected_estimation)

    infoBox("Final Objective Function Value", selected_est$final_ofv %>% round(2), color = "green", icon = icon("calculator"))
  })

  output$aic_box <- renderInfoBox({
    selected_est <- req(rv$selected_estimation)

    infoBox("AIC", selected_est$aic %>% round(2), color = "green", icon = icon("calculator"))
  })

  output$bic_box <- renderInfoBox({
    selected_est <- req(rv$selected_estimation)

    infoBox("BIC", selected_est$bic %>% round(2), color = "green", icon = icon("calculator"))
  })

  output$eigenratio_box <- renderInfoBox({
    selected_est <- req(rv$selected_estimation)

    if(!is.null(selected_est$eigenratio)) {
      infoBox("Eigen ratio", selected_est$eigenratio %>% round(2), color = "green", icon = icon("calculator"))
    } else {
      infoBox("Eigen ratio", "-", color = "green", icon = icon("calculator"))
    }

  })

  output$correlation_box <- renderInfoBox({
    selected_est <- req(rv$selected_estimation)

    if(!is.null(selected_est$correlation)) {
      infoBox("Correlation", selected_est$correlation, color = "green", icon = icon("calculator"))
    } else {
      infoBox("Correlation", "-", color = "green", icon = icon("calculator"))
    }

  })

  output$termination_messages <- renderText({
    selected_est <- req(rv$selected_estimation)

    msgs <- selected_est$termination_messages

    if(!is.null(msgs)){
      str_c(msgs$message, collapse = "\n")
    }else{
      "No termination message."
    }
  })

  output$termination_messages <- renderText({
    selected_est <- req(rv$selected_estimation)

    term_info <- selected_est$termination_information

    if(!is.null(term_info)){
      term_info %>% str_trim()
    }else{
      "No termination information."
    }
  })

  run_ofv_convergence_plot <- reactive({
    run <- req(rv$run)
    est_sel <- req(input$estimation_selection)

    run %>%
      plot_ofv(estimation_number = as.numeric(est_sel))
  })

  run_individual_ofv <- reactive({

    run <- req(rv$run)
    selected_est <- req(rv$selected_estimation)

    run %>%
      plot_individual_ofv(estimation_number = selected_est$number)
  })

  run_parameters_convergence_plot <- reactive({
    run <- req(rv$run)
    est_sel <- req(input$estimation_selection)

    run %>%
      plot_convergence(estimation_number = as.numeric(est_sel),
                       parameters = input$convergence_parameters)
  })

  observeEvent(input$estimation_save_report, {
    run <- req(rv$run)
    src <- normalizePath("www/rmd/estimation_report.Rmd")

    rmd_format <- switch(input$estimation_report_format,
                         PDF = rmarkdown::pdf_document(),
                         HTML = rmarkdown::html_vignette(),
                         Word = rmarkdown::word_document())

    extension <- switch(input$estimation_report_format, PDF = "pdf", HTML = "html", Word = "docx")

    safe_save_to_run <- safely(save_rmd_to_run)

    save_res <- safe_save_to_run(output_filename = sprintf("%s.Estimation-%s-report.%s",
                                                   run$info$run_id,
                                                   input$estimation_selection,
                                                   extension),
                        rmd_file = src,
                        format = rmd_format,
                        rmd_params = list(run = run,
                                          estimation_number = req(input$estimation_selection) %>% as.numeric))

    if(!is.null(save_res$error)){
      toastr_error(message = "An error occured while saving report.", title = "Error")
    }
  })

  output$estimation_download_report <- downloadHandler(
    filename = function(){
      run <- req(rv$run)
      paste(sprintf("%s.estimation-%s-report", run$info$run_id, req(input$estimation_selection)), sep = ".", switch(
        input$estimation_report_format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    content = function(file){
      run <- req(rv$run)

      src <- normalizePath("www/rmd/estimation_report.Rmd")

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "estimation_report.Rmd", overwrite = TRUE)

      rmd_format <- switch(input$estimation_report_format,
                           PDF = rmarkdown::pdf_document(),
                           HTML = rmarkdown::html_vignette(),
                           Word = rmarkdown::word_document())

      out <- rmarkdown::render("estimation_report.Rmd",
                               output_format = rmd_format,
                               params = list(run = run,
                                             estimation_number = req(input$estimation_selection) %>% as.numeric))

      file.rename(out, file)
    }
  )
