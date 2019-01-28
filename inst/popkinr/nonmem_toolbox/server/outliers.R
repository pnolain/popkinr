
  # outliers tab----

  output$outliers_cmt <- renderUI({
    run <- req(rv$run)

    dv_cmts <- run$model$compartments %>% filter(dv_target == TRUE)

    cmts <- setNames(dv_cmts$cmt, nm = dv_cmts$name)

    current_selection <- isolate(input$outliers_cmt)

    selectInput("outliers_cmt", "Compartment", choices = cmts,
                selected = current_selection)
  })

  output$outliers_restype <- renderUI({
    run <- req(rv$run)

    available_residuals <- run$model$residuals

    selectInput("outliers_restype", "Type of residuals", choices = available_residuals,
                selected = ifelse("CWRES" %in% available_residuals, "CWRES",
                                  ifelse("IWRES" %in% available_residuals, "IWRES",
                                         ifelse("NPDE" %in% available_residuals, "IWRES", "WRES"))))
  })

  outliers_selected_compartment <- reactive({
    s_cmt <- req(input$outliers_cmt)
    run <- req(rv$run)

    cmt <- run$model$compartments %>% filter(cmt == s_cmt)

    setNames(cmt$cmt, nm = cmt$name)
  })

  outliers_qqplot <- reactive({
    run <- req(rv$run)
    cmt <- req(outliers_selected_compartment())
    res_type <- req(input$outliers_restype)

    # temp_run <- filtered_run() %>%
    #   filter(!is.na(DV) & MDV == 0 & CMT == cmt)
    #
    # if(input$outliers_avoid_zero)
    #   temp_run <- temp_run %>% filter(TIME > 0)
    #
    # df <- temp_run$tables$pmxploitab
    #
    # g <- ggplot(df)+
    #   geom_qq(aes_string(sample = res_type))+
    #   geom_abline(slope = 1, intercept = 0)+
    #   labs(x = "Normal distribution quantiles", y = paste(res_type, "distribution quantiles"))
    #
    # g

    filtered_run() %>%
      plot_residuals(compartment = cmt,
                     residuals = res_type,
                     keep_time_zero = !input$outliers_avoid_zero,
                     type = "qq",
                     facet_scales = "fixed")

  })

  outliers_ks_test <- reactive({
    run <- req(rv$run)
    cmt <- req(outliers_selected_compartment())
    res <- req(input$outliers_restype)

    temp_run <- filtered_run() %>%
      filter(!is.na(DV) & MDV == 0 & CMT == cmt)

    if(input$outliers_avoid_zero)
      temp_run <- temp_run %>% filter(TIME > 0)

    df <- temp_run$tables$pmxploitab

    values <- df[[res]]

    ks <- ks.test(unique(values), "pnorm", mean = mean(values), sd = sd(values))

    list(test = ks,
         residual = res,
         mean = mean(values),
         sd = sd(values))
  })

  output$outliers_ks_table <- renderDataTable({
    ks <- req(outliers_ks_test())

    stats_df <- tibble(mean = ks$mean, sd = ks$sd) %>%
      rename(!!!setNames(nm = sprintf("%s (%s)", ks$res, colnames(.)), colnames(.)))

    test_df <- broom::tidy(ks$test) %>%
      rename(!!!setNames(nm = sprintf("Kolmogorov-Smirnov %s", colnames(.)), colnames(.)))

    ks_df <- bind_cols(stats_df, test_df) %>%
      gather(Parameter, Value)

    #datatable(ks_df, rownames = FALSE, options = list(dom = 'r', scrollX = TRUE))
    datatable(ks_df, rownames = FALSE, options = list(dom = 'rt', scrollX = TRUE))
  })

  output$outliers_ks_decision <- renderText({
    ks <- req(outliers_ks_test())

    pval <- as.numeric(input$outliers_pvalue)

    normality_ok <- ks$test$p.value > pval

    if(normality_ok){
      sprintf("Normality hypothesis cannot be rejected (p-value > %s)", pval)
    } else {
      sprintf("Normality hypothesis is rejected (p-value < %s)", pval)
    }
  })

  output$selected_outliers_residuals_table <- renderDataTable({

    res_type <- req(input$outliers_restype)

    data <- req(outliers_qqplot()$data) %>%
      arrange(Residuals_Value)

    i      <- 1:length(data$Residuals_Value)
    fi     <- (i - 0.5) / length(data$Residuals_Value)
    x.norm <- qnorm(fi)

    data$QUANTILE_NORMALDIST <- x.norm

    ns <- NS("outliers_qqplot")

    brush <- input[[ns("brush")]]

    selected_subjects <- tibble(ID = numeric())

    b_df <- brushedPoints(data, brush, xvar = "QUANTILE_NORMALDIST", yvar = "Residuals_Value") %>% select(-QUANTILE_NORMALDIST)

    df <- tibble(ID = numeric(), TIME = numeric(), RES = numeric())

    if(nrow(b_df) > 0){
      run <- filtered_run()

      join_cols <- set_names(c("ID", "CMT", "Residuals_Value"), c("ID", "CMT", res_type))

      df <- run$tables$pmxploitab %>%
        semi_join(b_df, by = join_cols) %>%
        select(ID, CMT, one_of(res_type, run$model$regressors$column))
    }

    datatable(df, options = list(pageLength = 20, dom = 'rtip', scrollX = TRUE))
  })

  outliers_grubbs_detection <- reactive({
    run <- req(rv$run)
    cmt <- req(outliers_selected_compartment())
    res <- req(input$outliers_restype)
    pval <- as.numeric(input$outliers_pvalue)
    avoid_zero <- input$outliers_avoid_zero

    outliers_data <- filtered_run() %>%
      detect_outliers(compartment = cmt,
                      residuals = res,
                      method = "grubbs",
                      grubbs_pvalue_threshold = pval,
                      keep_time_zero = !avoid_zero)

    outliers_data
  })

  # output$outliers_grubbs_table <- renderDataTable({
  #
  #   outliers_data <- req(outliers_grubbs_detection())
  #
  #   datatable(outliers_data$outliers, rownames = FALSE,
  #             options = list(pageLength = 20, dom = 'rtip'))
  # })

  output$outliers_grubbs_message <- renderText({
    out_data <- req(outliers_grubbs_detection())

    n_outliers <- nrow(out_data$outliers)
    n_source <- nrow(out_data$source)

    if(n_outliers == 0) {
      sprintf("No outliers detected in %s observations.", n_source)
    } else if (n_outliers == 1) {
      sprintf("One outlier detected in %s observations (%s).", n_source, scales::percent(n_outliers / n_source))
    } else {
      sprintf("%s outliers detected in %s observations (%s).", n_outliers, n_source, scales::percent(n_outliers / n_source))
    }
  })

  outliers_boxplot_detection <- reactive({
    run <- req(rv$run)
    cmt <- req(outliers_selected_compartment())
    res <- req(input$outliers_restype)
    coefficient <- req(input$outliers_k_coefficient)
    avoid_zero <- input$outliers_avoid_zero


    outliers_data <- filtered_run() %>%
      detect_outliers(compartment = cmt,
                      residuals = res,
                      method = "boxplot",
                      boxplot_coefficient = coefficient,
                      keep_time_zero = !avoid_zero)

    outliers_data
  })

  outliers_boxplot <- reactive({
    out_data <- req(outliers_boxplot_detection())

    g <- ggplot(mapping = aes_string(x = factor(1), y = out_data$residuals))+
      geom_boxplot(data = out_data$source, coef = req(input$outliers_k_coefficient), outlier.colour = "red")

    if(nrow(out_data$outliers) > 0){
      g <- g +
        geom_text(data = out_data$outliers,
                  aes(label = ID), size = 4,
                  position = position_jitter(width = 0.2))
    }

    g <- g +
      scale_x_discrete(breaks = NULL)+
      labs(x = NULL, y = out_data$residuals)

    g
  })
#
#   output$outliers_boxplot_table <- renderDataTable({
#     out_data <- req(outliers_boxplot_detection())
#
#     datatable(out_data$outliers, rownames = FALSE,
#               options = list(pageLength = 20, dom = 'rtip'))
#   })

  output$outliers_boxplot_message <- renderText({
    out_data <- req(outliers_boxplot_detection())

    n_outliers <- nrow(out_data$outliers)
    n_source <- nrow(out_data$source)

    if(n_outliers == 0) {
      sprintf("No outliers detected in %s observations.", n_source)
    } else if (n_outliers == 1) {
      sprintf("One outlier detected in %s observations (%s).", n_source, scales::percent(n_outliers / n_source))
    } else {
      sprintf("%s outliers detected in %s observations (%s).", n_outliers, n_source, scales::percent(n_outliers / n_source))
    }
  })


observeEvent(input$outliers_save_report, {
    run <- req(rv$run)
    src <- normalizePath("www/rmd/outliers_report.Rmd")

    rmd_format <- switch(input$outliers_report_format,
                         PDF = rmarkdown::pdf_document(),
                         HTML = rmarkdown::html_vignette(),
                         Word = rmarkdown::word_document())

    extension <- switch(input$outliers_report_format, PDF = "pdf", HTML = "html", Word = "docx")

    safe_save_to_run <- safely(save_rmd_to_run)

    save_res <- safe_save_to_run(output_filename = sprintf("%s.%s-%s-%s-Outliers-report-%s.%s",
                                                  run$info$run_id,
                                                  outliers_selected_compartment(),
                                                  input$outliers_restype,
                                                  input$outliers_pvalue,
                                                  format(Sys.time(), "%Y%m%d-%H%M%S"),
                                                  extension),
                        rmd_file = src,
                        format = rmd_format,
                        rmd_params = list(run = filtered_run(),
                                          compartment = outliers_selected_compartment(),
                                          residuals = input$outliers_restype,
                                          keep_time_zero = !input$outliers_avoid_zero,
                                          pvalue = as.numeric(input$outliers_pvalue),
                                          boxplot_coefficient = input$outliers_k_coefficient))

    if(!is.null(save_res$error)){
      toastr_error(message = "An error occured while saving report.", title = "Error")
    }
  })


  output$outliers_download_report <- downloadHandler(
    filename = function() {
      run <- req(rv$run)
      paste(sprintf("%s.%s-%s-outliers-report", run$info$run_id, req(outliers_selected_compartment()),
                    req(input$outliers_restype)), sep = ".", switch(
        input$outliers_report_format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    content = function(file) {
      run <- req(rv$run)

      src <- normalizePath("www/rmd/outliers_report.Rmd")

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "outliers_report.Rmd", overwrite = TRUE)

      rmd_format <- switch(input$outliers_report_format,
                           PDF = rmarkdown::pdf_document(),
                           HTML = rmarkdown::html_vignette(),
                           Word = rmarkdown::word_document())

      out <- rmarkdown::render("outliers_report.Rmd",
                               output_format = rmd_format,
                               params = list(run = filtered_run(),
                                             compartment = req(outliers_selected_compartment()),
                                             residuals = input$outliers_restype,
                                             keep_time_zero = !input$outliers_avoid_zero,
                                             pvalue = as.numeric(input$outliers_pvalue),
                                             boxplot_coefficient = req(input$outliers_k_coefficient)))

      file.rename(out, file)
    }
  )
