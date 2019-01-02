run_quality_criteria <- reactive({
  run <- req(rv$run)

  pred <- req(input$qc_pred_type)
  log_data <- input$qc_log_data

  grps <- map(input$qc_split_by, as.symbol)

  run_qc <- filtered_run() %>%
    group_by(UQS(grps)) %>%
    quality_criteria(prediction = pred,
                     log_data = log_data,
                     alpha = as.numeric(input$qc_alpha))

  run_qc
})

output$qc_pred_type <- renderUI({
  run <- req(rv$run)
  types <- req(run$model$predictions)

  radioButtons("qc_pred_type", "Prediction types", selected = types[1], inline = TRUE,
               choices = types)
})

output$qc_standard <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "standard", plyr::mapvalues(input$qc_split_by,
                                                                                     rv$run$model$covariates$column,
                                                                                     rv$run$model$covariates$name,
                                                                                     warn_missing = FALSE)))

  qc_s <- qc %>% select(one_of(qc_cols)) %>% unnest() %>% rename("Observations" = n_observations,
                                                                 "Maximal Error" = max_err,
                                                                 "Absolute Average Fold Error" = aafe,
                                                                 "Average Fold Error" = afe)

  datatable(qc_s, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE)
})

output$qc_bias <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "bias", plyr::mapvalues(input$qc_split_by,
                                                                                 rv$run$model$covariates$column,
                                                                                 rv$run$model$covariates$name,
                                                                                 warn_missing = FALSE)))

  qc_b <- qc %>% select(one_of(qc_cols)) %>% unnest()

  ci <- (1 - as.numeric(input$qc_alpha)) * 100

  dots <- setNames(c("n_observations", "value", "ci_low", "ci_up", "relative_value"),
                   nm = c("Observations", "MPE (absolute)", paste(ci, "% CI (lower bound)"), paste(ci, "% CI (upper bound)"), "MPE (%)"))

  qc_b <- qc_b %>% rename(!!!dots)

  datatable(qc_b, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE) %>%
    formatPercentage("MPE (%)", 2)
})

output$qc_precision <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "precision", plyr::mapvalues(input$qc_split_by,
                                                                                      rv$run$model$covariates$column,
                                                                                      rv$run$model$covariates$name,
                                                                                      warn_missing = FALSE)))

  qc_p <- qc %>% select(one_of(qc_cols)) %>% unnest()

  ci <- (1 - as.numeric(input$qc_alpha)) * 100

  dots <- setNames(c("n_observations", "value", "ci_low", "ci_up", "relative_value"),
                   nm = c("Observations", "RMSE (absolute)", paste(ci, "% CI (lower bound)"), paste(ci, "% CI (upper bound)"), "RMSE (%)"))

  qc_p <- qc_p %>% rename(!!!dots)

  datatable(qc_p, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE) %>%
    formatPercentage("RMSE (%)", 2)
})

output$qc_t_test_obs <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "t_test_obs", plyr::mapvalues(input$qc_split_by,
                                                                                   rv$run$model$covariates$column,
                                                                                   rv$run$model$covariates$name,
                                                                                   warn_missing = FALSE)))

  qc_t <- qc %>% select(one_of(qc_cols))  %>%
    mutate(t_test_obs = map(t_test_obs, tidy)) %>%
    unnest() %>%
    select(-method, -alternative)

  df <- qc_t

  datatable(df, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE)
})


output$qc_t_test_res <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "t_test_res", plyr::mapvalues(input$qc_split_by,
                                                                                       rv$run$model$covariates$column,
                                                                                       rv$run$model$covariates$name,
                                                                                       warn_missing = FALSE)))

  qc_t <- qc %>%
    select(one_of(qc_cols))  %>%
    unnest() %>%
    mutate(t.test = map(t.test, tidy)) %>%
    unnest() %>%
    select(-method, -alternative)

  df <- qc_t

  datatable(df, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE)
})


output$qc_corr_test <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "correlation_test", plyr::mapvalues(input$qc_split_by,
                                                                                             rv$run$model$covariates$column,
                                                                                             rv$run$model$covariates$name,
                                                                                             warn_missing = FALSE)))

  qc_c <- qc %>%
    select(one_of(qc_cols)) %>%
    mutate(correlation_test = map(correlation_test, tidy)) %>%
    unnest() %>%
    select(-method, -alternative)

  df <- qc_c
  # df <- tidy(qc_c) %>%
  #   select(estimate, statistic, p.value, parameter,	conf.low,	conf.high)

  datatable(df, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE)
})

run_qc_lin_reg <- reactive({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "linear_regression", plyr::mapvalues(input$qc_split_by,
                                                                                              rv$run$model$covariates$column,
                                                                                              rv$run$model$covariates$name,
                                                                                              warn_missing = FALSE)))

  qc_l <- qc %>%
    select(one_of(qc_cols))
})

output$qc_lin_reg <- renderDataTable({
  qc_l <- req(run_qc_lin_reg()) %>%
    mutate(linear_regression = map(linear_regression, tidy)) %>%
    unnest()

  df <- qc_l %>%
    mutate(term = plyr::revalue(term, c("observations" = "slope")))

  datatable(df, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE)
})


output$run_qc_lin_reg_plot <- renderUI({

  qc_l <- req(run_qc_lin_reg())

  plots <- map(qc_l$linear_regression, ggplotRegression)
  splits <- attr(qc_l, "splits")

  plots_ui <- map(seq_along(plots), function(i){
    p <- plots[[i]]
    if(is.null(p))
      return(NULL)
    plot_id <- sprintf("lin_reg_%s", i)
    plot_name <- plot_id

    if(length(splits) > 0){
      p$labels$title <- splits[[i]]
      plot_name <- sprintf("linear-regression-%s", splits[[i]])
    }

    callModule(extendedPlot, plot_id, reactive_run = reactive(rv$run), reactive_plot = reactive(p), r_code = FALSE)
    extendedPlotUI(plot_id, title = plot_name, height = 500, width  = 500, customizable = FALSE, exportable = FALSE)
  })

  not_null <- which(!map_lgl(plots_ui, is.null))
  plots_ui <- plots_ui[not_null]
  splits <- splits[not_null]

  if(length(plots_ui) == 1)
    return(plots_ui)

  tabs <- map(seq_along(plots_ui), function(i){
    p <- plots_ui[[i]]
    tabPanel(title = splits[i], p)
  })
  do.call(tabsetPanel, tabs)
})

output$qc_download_report <- downloadHandler(
  filename = function() {
    run <- req(rv$run)
    paste(sprintf("%s.QC-%s-report", run$info$run_id, req(input$qc_pred_type)), sep = ".", switch(
      input$qc_report_format, PDF = "pdf", HTML = "html", Word = "docx"
    ))
  },

  content = function(file) {
    run <- req(rv$run)

    src <- normalizePath("www/rmd/qc_report.Rmd")

    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, "qc_report.Rmd", overwrite = TRUE)

    rmd_format <- switch(input$qc_report_format,
                         PDF = rmarkdown::pdf_document(),
                         HTML = rmarkdown::html_vignette(),
                         Word = rmarkdown::word_document())

    grps <- map(input$qc_split_by, as.symbol)

    out <- rmarkdown::render("qc_report.Rmd",
                             output_format = rmd_format,
                             params = list(run = filtered_run() %>% group_by(UQS(grps)),
                                           prediction = req(input$qc_pred_type),
                                           log_data = input$qc_log_data,
                                           alpha =  as.numeric(input$qc_alpha)))

    file.rename(out, file)
  }
)

ggplotRegression <- function (fit) {
  if(is.null(fit))
    return(NULL)
  eq <- substitute(italic(y) == a + b %*% italic(x)*","~~italic(r)^2~"="~r2,
                   list(y = fit$call$formula[[2]],
                        x = fit$call$formula[[3]],
                        a = format(coef(fit)[1], digits = 3),
                        b = format(coef(fit)[2], digits = 3),
                        r2 = format(summary(fit)$r.squared, digits = 4)))

  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(subtitle = eq)
}


observeEvent(input$qc_save_report, {
  run <- req(rv$run)
  src <- normalizePath("www/rmd/qc_report.Rmd")
  pred <- req(input$qc_pred_type)

  rmd_format <- switch(input$qc_report_format,
                       PDF = rmarkdown::pdf_document(),
                       HTML = rmarkdown::html_vignette(),
                       Word = rmarkdown::word_document())

  extension <- switch(input$qc_report_format, PDF = "pdf", HTML = "html", Word = "docx")

  grps <- map(input$qc_split_by, as.symbol)

  save_rmd_to_run(output_filename = sprintf("%s.QC-%s-report-%s.%s",
                                            run$info$run_id,
                                            pred,
                                            format(Sys.time(), "%Y%m%d-%H%M%S"),
                                            extension),
                  rmd_file = src,
                  format = rmd_format,
                  rmd_params = list(run = filtered_run() %>% group_by(UQS(grps)),
                                    prediction = pred,
                                    log_data = input$qc_log_data,
                                    alpha =  as.numeric(input$qc_alpha)))
})
