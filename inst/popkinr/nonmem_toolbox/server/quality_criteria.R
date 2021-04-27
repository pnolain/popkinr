run_quality_criteria <- reactive({
  run <- req(rv$run)

  pred <- req(input$qc_pred_type)
  log_data <- input$qc_log_data

  grps <- map(input$qc_split_by, as.symbol)

  filtered_run() %>%
    group_by(!!!(grps)) %>%
    quality_criteria(prediction = pred,
                     log_data = log_data,
                     alpha = as.numeric(input$qc_alpha))
})


observe({
  build_pmxploit_qc_call()
})


build_pmxploit_qc_call <- function(){
  # Get run_quality_criteria function content
  qc_reactive_envir <- get_env(run_quality_criteria)

  qc_reactive_fn <- qc_reactive_envir$.origFunc

  # Evaluate the qc function
  qc_fn_envir <- get_env(qc_reactive_fn)
  qc_fn_body <- body(qc_reactive_fn)

  # Remove last line from evaluation to prevent execution errors
  eval(qc_fn_body[-length(qc_fn_body)], qc_fn_envir)

  # Extract the last line of the function
  # -> supposed to be a call to a pmxploit qc function
  return_line <- as.list(qc_fn_body) %>% keep(is_call) %>% last()

  # Extract the links of a pipe chain (eg. filter %>% group_by %>% qc_fn)
  extract_chain_links <- function(chain){
    current_call_args <- call_args(chain)
    chain_links <- list()

    for(item in current_call_args){
      new_item <- if(is_call(item, "%>%")) extract_chain_links(item) else item
      chain_links <- c(chain_links, new_item)
    }
    chain_links
  }

  chain_links <- extract_chain_links(return_line)

  # Check if the run is currently filtered, ie. object comes either from `filtered_run()` or `filtered_run_show_mdv()` reactives
  filter_call <- NULL
  filter_link <- chain_links[which(map_lgl(chain_links, ~ is_call(., "filtered_run") || is_call(., "filtered_run_show_mdv")))]

  if(length(filter_link) == 1L){
    filter_fn <- call_name(filter_link[[1]])
    # Extract current filters from the application reactiveValues `rv`
    main_rv <- env_get(qc_fn_envir, "rv", default = NULL)
    run_filters <- main_rv$app_filters

    if(filter_fn == "filtered_run_show_mdv")
      run_filters <- discard(run_filters, ~ all.equal(., quo(MDV == 0)) == TRUE)

    if(length(run_filters) > 0L){
      # remove `~` operator
      run_filters <- run_filters %>% map(~as.list(.)[[2]])

      # Create a filter call
      filter_call <- call2(quote(filter), !!!(run_filters))
    }
  }

  # Check if the qc must be grouped
  group_by_call <- NULL
  group_by_chain <- chain_links[which(map_lgl(chain_links, ~ is_call(., "group_by")))]

  if(length(group_by_chain) == 1L){
    # Extract current grouping columns
    #grps <- env_get(qc_fn_envir, as.character(call_args(call_args(group_by_chain[[1]])[[1]])[[1]]))
    grps <- eval(group_by_chain[[1]][[-1]][[-1]][[-1]][[-1]], qc_fn_envir)

    if(length(grps) > 0){
      group_by_call <- call2(quote(group_by), !!!(syms(grps)))
    }
  }

  # Last link should be a pmxploit qc function calls
  pmxploit_chain <- last(chain_links)

  # Get the arguments of the function call
  fargs <- call_args(pmxploit_chain)

  # Evaluate the arguments to get the UI widgets values
  args_values <- map(fargs[names(fargs) != ""], ~ unname(eval(., envir = qc_fn_envir)))
  # args_values <- map(fargs[names(fargs) != ""], ~ eval(., envir = qc_fn_envir))

  edit_call <- function(cc, ...){
    args <- dots_list(...)
    call_modify(cc, !!!(args))
  }

  # Edit the call to integrate de arguments values
  # pmxploit_call <- edit_call(pmxploit_chain, !!!(args_values))

  # NEW: Remove default arguments that are not changed
  original_args <- formals(eval(first(pmxploit_chain)))

  args_to_skip <- map2_lgl(args_values, original_args[names(args_values)], function(a, b){
    if(is_missing(b)) return(FALSE)
    identical(unname(a), unname(eval(b)))
  })

  args_values <- args_values[!args_to_skip]
  pmxploit_call <- call2(first(pmxploit_chain), !!!(args_values))

  # browser()
  run <- req(rv$run)
  # Create a `load_nm_run` call with the run path
  load_run_call <- call2(quote(load_nm_run), run$info$path)

  if(identical(run, pmxploit::EXAMPLERUN)){
    load_run_call <- quote(pmxploit::EXAMPLERUN)
  }
  # Construct the full call:
  # load_nm_run %>% filter (if any) %>% group_by (if any) %>% pmxploit_call %>% theme_pmx())
  calls <- c(load_run_call, filter_call, group_by_call, pmxploit_call)
  txt <- map_chr(calls, ~ str_c(deparse(., width.cutoff = 150L), collapse = "\n"))
  full_text <- str_c(txt, collapse = " %>%\n\t")

  shinyAce::updateAceEditor(session, "qc_r_code", full_text)
}


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

  qc_s <- qc %>% select(one_of(qc_cols)) %>% unnest(standard) %>% rename("Observations" = n_observations,
                                                                 "Maximal Error" = max_err,
                                                                 "Absolute Average Fold Error" = aafe)

  datatable(qc_s, options = list(paging = FALSE, info = FALSE, searching = FALSE), rownames = FALSE)
})

output$qc_bias <- renderDataTable({
  qc <- req(run_quality_criteria())

  qc_cols <- intersect(colnames(qc), c("n_observations", "bias", plyr::mapvalues(input$qc_split_by,
                                                                                 rv$run$model$covariates$column,
                                                                                 rv$run$model$covariates$name,
                                                                                 warn_missing = FALSE)))

  qc_b <- qc %>% select(one_of(qc_cols)) %>% unnest(bias)

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

  qc_p <- qc %>% select(one_of(qc_cols)) %>% unnest(precision)

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
    unnest(t_test_obs) %>%
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
    filter(!map_lgl(t_test_res, is.null)) %>%
    unnest(t_test_res) %>%
    mutate(t.test = map(t.test, tidy)) %>%
    unnest(t.test) %>%
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
    unnest(correlation_test) %>%
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
    unnest(linear_regression)

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
                             params = list(run = filtered_run() %>% group_by(!!!(grps)),
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
                  rmd_params = list(run = filtered_run() %>% group_by(!!!(grps)),
                                    prediction = pred,
                                    log_data = input$qc_log_data,
                                    alpha =  as.numeric(input$qc_alpha)))
})
