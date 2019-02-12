
# diagnostics tab----

output$dv_vs_pred_type <- renderUI({

  run <- req(rv$run)
  types <- req(run$model$predictions)

  def_selection <- intersect(types, c("PRED", "IPRED"))

  checkboxGroupInput(
    "dv_vs_pred_type", "Prediction types",
    choices = types,
    selected = def_selection, inline = TRUE
  )
})


output$indiv_idv <- renderUI({
  run <- req(rv$run)

  selectInput("indiv_idv", "X-axis", choices = set_names(run$model$independent_variables$column, run$model$independent_variables$name))
})

output$indiv_cat_cov <- renderUI({
  run <- req(rv$run)

  cat_covs <- subset(run$model$covariates, type == "categorical")

  cat_choices <- setNames(cat_covs$column, nm = cat_covs$name)

  selectizeInput("indiv_cat_cov",
                 label = "Categorical covariate", multiple = FALSE,
                 choices = cat_choices, selected = NULL)
})

output$indiv_pred_type <- renderUI({
  run <- req(rv$run)
  types <- req(run$model$predictions)

  checkboxGroupInput("indiv_pred_type", "Prediction types",
                     choices = types,
                     selected = types, inline = TRUE)
})

output$res_x_type <- renderUI({
  run <- req(rv$run)

  res_x_names <- req(c(run$model$independent_variables$column, run$model$predictions))

  def_selection <- intersect(res_x_names, c("TIME", "PRED"))

  checkboxGroupInput(
    "res_x_type", "Residual regressors (X-axis)",
    choices = res_x_names,
    selected = def_selection, inline = TRUE, width = 400
  )
})

output$res_y_type <- renderUI({
  run <- req(rv$run)

  run_res_y <- req(c(run$model$residuals))

  checkboxGroupInput(
    "res_y_type", "Residual types (Y-axis)",
    choices = run_res_y,
    selected = ifelse("CWRES" %in% run_res_y, "CWRES",
                      ifelse("IWRES" %in% run_res_y, "IWRES",
                             ifelse("NPDE" %in% run_res_y, "IWRES", "WRES"))), inline = TRUE, width = 400
  )
})

output$selected_dv_vs_pred_data <- renderDataTable({

  data <- req(run_dv_vs_pred_plot()$data)

  ns <- NS("dv_vs_pred_plot")

  brush <- input[[ns("brush")]]

  n_var <- 2 + length(input$diagnostic_split_by)
  panel_names <- paste0("panelvar", seq_len(n_var))
  lhs <- (brush$mapping)[panel_names] %>% unlist
  rhs <- brush[panel_names] %>% unlist

  data <- data %>%
    filter_(.dots = sapply(sprintf("~ %s == \"%s\"", lhs, rhs), as.formula) %>% unname)

  brush$mapping[panel_names] <- NULL


  df <- brushedPoints(data, brush)

  if(nrow(df) > 0) {
    df <- df %>% mutate(n = row_number()) %>%  spread(X, X_Value) %>% spread(Y, Y_Value) %>% select(-n)
  } else {
    df <- tibble(ID = numeric(), TIME = numeric(), CMT = numeric())
  }

  datatable(df, options = list(pageLength = 20, dom = 'rtip'))

})
output$selected_spaghetti_data <- renderDataTable({

  data <- req(run_spaghetti_plot()$data)

  ns <- NS("spaghetti_plot")

  brush <- input[[ns("brush")]]

  df <- brushedPoints(data, brush)

  if(nrow(df) > 0) {
    if(length(input$diagnostic_split_by) > 0){
      df <- df %>% select(-one_of(paste(input$diagnostic_split_by, collapse = ".")))
    }
  }else {
    df <- tibble(ID = numeric(), TIME = numeric(), CMT = numeric())
  }

  datatable(df, options = list(pageLength = 20, dom = 'rtip'))

})
output$selected_residuals_data <- renderDataTable({

  df <- tibble(ID = numeric(), CMT = numeric())

  data<- req(run_residuals_plot()$data)

  ns <- NS("residuals_plot")

  brush <- input[[ns("brush")]]

  if(input$residuals_plot_type == "scatterplot"){

    if(!is.null(brush$mapping$x)){# ensure that it is scatterplot brush

      n_var <- 2 + length(input$diagnostic_split_by)
      panel_names <- paste0("panelvar", seq_len(n_var))
      lhs <- (brush$mapping)[panel_names] %>% unlist
      rhs <- brush[panel_names] %>% unlist

      data <- data %>%
        filter_(.dots = sapply(sprintf("~ %s == \"%s\"", lhs, rhs), as.formula) %>% unname)

      brush$mapping[panel_names] <- NULL

      df <- brushedPoints(data, brush)


      if(nrow(df)>0){
        df <- df %>% mutate(n = row_number()) %>%
          spread(Regressor, Regressor_Value) %>%
          spread(Residuals, Residuals_Value) %>%
          select(-n)
      }
    }
  } else if(input$residuals_plot_type == "qq") {

    if(is.null(brush$mapping$x)){ # ensure that it is qq plot brush

      n_var <- 1 + length(input$diagnostic_split_by)
      panel_names <- paste0("panelvar", seq_len(n_var))
      lhs <- (brush$mapping)[panel_names] %>% unlist
      rhs <- brush[panel_names] %>% unlist

      data <- data %>%
        group_by(CMT, Residuals) %>%
        arrange(Residuals_Value) %>%
        filter_(.dots = sapply(sprintf("~ %s == \"%s\"", lhs, rhs), as.formula) %>% unname)

      norm_quantiles <- data %>%
        summarise(fi = list((1:n() - 0.5) / n())) %>%
        unnest(fi) %>%
        mutate(QUANTILE_NORMALDIST = qnorm(fi))

      norm_quantiles$ID <- data$ID
      norm_quantiles$res <- data$Residuals_Value


      selected_subjects <- tibble(ID = numeric())

      brush$mapping[panel_names] <- NULL

      df <- brushedPoints(norm_quantiles, brush, yvar = "res", xvar = "QUANTILE_NORMALDIST")

      if(nrow(df)>0){
        cols <- c("ID", "CMT", input$diagnostic_split_by, "res")
        df <- df  %>%
          mutate_(.dots = setNames(rhs[-1], lhs[-1])) %>%
          select(one_of(cols)) %>%
          rename(!!!setNames("res", brush$panelvar1))
      }
    }
  }

  datatable(df, options = list(pageLength = 20, dom = 'rtip'))
})

output$individual_pages <- renderUI({
  run <- req(rv$run)

  n_subjects <- run$info$number_of_subjects

  if(!is.null(rv$app_filters)){
    filtered_ds <- req(temp_filtered_dataset())
    n_subjects <- filtered_ds$ID %>% unique %>% length()
  }

  n_subjects_per_page <- input$individual_layout %>% as.integer
  n_pages <- ceiling(n_subjects / n_subjects_per_page)

  sliderInput("individual_pages", "Pages", value = 1, min = 1, max = n_pages, step = 1, width = "100%")
})

#observeEvent(input$individual_pages, {
observe({
  req(input$individual_pages)
  req(input$individual_layout)

  rv$current_page <- input$individual_pages %>% as.numeric

  current_id <- rv$first_individual_id
  first_id <- (rv$current_page - 1) * rv$current_layout + 1
  last_id <- rv$current_page * rv$current_layout

  if(is.null(current_id) || !between(current_id, first_id, last_id))
    rv$first_individual_id <- first_id
})

observeEvent(input$individual_layout, {

  run <- rv$run # no req() here!!!

  n_subjects <- run$info$number_of_subjects

  if(!is.null(run)){
    if(!is.null(rv$app_filters)){
      filtered_ds <- req(temp_filtered_dataset())
      n_subjects <- filtered_ds$ID %>% unique %>% length()
    }
  }

  n_subjects_per_page <- input$individual_layout %>% as.integer
  n_pages <- ceiling(n_subjects / n_subjects_per_page)

  if(!is.null(rv$current_layout)){
    selected_id <- rv$first_individual_id

    selected_page <- ceiling(selected_id / n_subjects_per_page)
  } else{
    selected_page <- 1
  }

  rv$current_layout <- input$individual_layout %>% as.numeric
  rv$current_page <- selected_page

  updateSliderInput(session, "individual_pages",
                    value = selected_page,
                    min = 1, max = n_pages)
})

individuals_plot_range <- reactive({

  n_subjects <- rv$run$info$number_of_subjects

  if(!is.null(rv$app_filters)){
    filtered_ds <- req(temp_filtered_dataset())
    n_subjects <- filtered_ds$ID %>% unique %>% length()
  }

  n_subjects_per_page <- req(rv$current_layout %>% as.integer)

  selected_page <- req(rv$current_page)

  start <- (selected_page - 1) * n_subjects_per_page + 1
  end <- min(selected_page * n_subjects_per_page, n_subjects)

  c(start, end)
})

build_all_individual_profiles_plot <- function(filepath){
  n_subjects <- rv$run$info$number_of_subjects

  df <- rv$run$tables$pmxploitab

  if(!is.null(rv$app_filters)){
    df <- req(temp_filtered_dataset())
    n_subjects <- df$ID %>% unique %>% length()
  }

  all_ids <- df$ID %>% unique %>% sort
  n_subjects_per_page <- rv$current_layout %>% as.integer

  n_pages <- ceiling(n_subjects / n_subjects_per_page)

  pdf(filepath, paper = "a4r", width = 12, height = 9)

  withProgress(message = 'Generating individual profiles', value = 0, expr = {

    map(seq_len(n_pages), function(i){
      incProgress(1/n_pages, detail = sprintf("Page %s of %s", i, n_pages))

      start <- (i - 1) * n_subjects_per_page + 1
      end <- min(i * n_subjects_per_page, n_subjects)

      selected_ids <- all_ids[start:end] %>% as.character %>% as.integer()

      selected_cmt <- selected_compartment()

      cat_cov <- input$indiv_cat_cov

      if(cat_cov == "")
        cat_cov <- NULL

      source_run <- if(input$show_indiv_mdv) filtered_run_show_mdv() else filtered_run()

      g <- source_run %>%
        plot_individual_profiles(ids = selected_ids,
                                 compartment = selected_cmt,
                                 predictions = req(input$indiv_pred_type),
                                 categorical_covariate = cat_cov,
                                 log_dv = input$diag_log_dv,
                                 idv = req(input$indiv_idv),
                                 show_observations = input$show_observations,
                                 predictions_dots = as.logical(input$predictions_dots),
                                 x_scale = input$plots_x_scale,
                                 y_scale = input$plots_y_scale,
                                 facet_scales = input$facet_scales,
                                 keep_time_zero = !input$diag_avoid_zero)

      print(g)
    })
  })

  dev.off()

}

observeEvent(input$save_all_individual_profiles, {

  ### Generate profiles
  temp_file <- paste(app_temp_directory, "individual_plots.pdf", sep = "/")

  if(file.exists(temp_file))
    unlink(temp_file)

  build_all_individual_profiles_plot(temp_file)

  run_path <- rv$run_path

  is_directory <- file.info(run_path)$isdir

  source_type <- ifelse(is_directory, "folder", "archive")

  withProgress({
    if(!is_directory){
      run_parent_dir <- dirname(run_path)
      temp_dir <- paste(run_parent_dir, "pmxploitemp", sep = "/")

      if(dir.exists(temp_dir))
        unlink(temp_dir, recursive = TRUE)

      untar(run_path, exdir = temp_dir)
    }

    temp_subdir <- paste(ifelse(is_directory, run_path, temp_dir), "pmxploit", "plots", sep = "/")

    if(!dir.exists(temp_subdir))
      dir.create(temp_subdir, recursive = TRUE)

    file <- sprintf("%s-%s-%s.pdf",
                    rv$run$info$run_id,
                    "individual_profiles",
                    format(Sys.time(), "%Y%m%d-%H%M%S"))

    target_file <- paste(temp_subdir, file, sep = "/")

    file.copy(temp_file, target_file)
    unlink(temp_file)

    if(!is_directory){
      wd <- setwd(run_parent_dir)

      system2("tar", sprintf("-czvf '%s' -C '%s' .", basename(run_path), temp_dir), stdout = FALSE)

      setwd(wd)

      unlink(temp_dir, recursive = TRUE)
    }

    setProgress(1, detail = "Done !")
  }, message = sprintf("Saving profiles to run %s", source_type))
})

output$export_all_individual_profiles <- downloadHandler(
  filename = function() {
    run <- req(rv$run)
    sprintf("%s.individual_profiles.pdf", run$info$run_id)
  },
  content = function(file){
    temp_file <- paste(tempdir(), "individual_plots.pdf", sep = "/")

    if(file.exists(temp_file))
      unlink(temp_file)

    build_all_individual_profiles_plot(temp_file)

    file.copy(temp_file, file)
  }
)


# reactive plots ----
run_dv_vs_pred_plot <- reactive({
  run <- req(rv$run)

  selected_cmt <- req(selected_compartment())

  smooth <- NULL

  if(input$diag_smoothing_method != "none"){
    smooth  <- input$diag_smoothing_method
  }


  grps <- map(input$diagnostic_split_by, as.symbol)

  filtered_run() %>%
    group_by(UQS(grps)) %>%
    plot_dv_vs_predictions(compartment = selected_cmt, dv = "DV",
                           predictions = req(input$dv_vs_pred_type),
                           log_dv = input$diag_log_dv,
                           facetted = !input$dv_vs_pred_overlay,
                           smoothing_method = smooth,
                           facet_scales = input$facet_scales, x_scale = input$plots_x_scale, y_scale = input$plots_y_scale,
                           keep_time_zero = !input$diag_avoid_zero, transparency = input$transparency)
})

dv_vs_pred_plot_extra <- reactive({
  if(!input$stretch)
    return(coord_fixed())

  return(NULL)
})

output$spaghetti_individuals <- renderUI({
  run <- req(rv$run)

  ids <- c("All" = -1, unique(run$tables$dataset$ID %>% as.character %>% as.numeric))

  selectInput("spaghetti_individuals", "Individuals", choices = ids, selected = -1)
})

run_spaghetti_plot <- reactive({
  run <- req(rv$run)

  ids <- req(input$spaghetti_individuals)

  if(ids == -1){
    ids <- NULL
  } else {
    ids <- as.numeric(ids)
  }

  selected_cmt <- req(selected_compartment())

  grps <- map(input$diagnostic_split_by, as.symbol)

  filtered_run_show_mdv() %>%
    group_by(UQS(grps)) %>%
    plot_observed_profiles(compartment = selected_cmt,
                           ids = ids,
                           idv = req(input$spaghetti_idv),
                           log_dv = input$diag_log_dv,
                           facetted = input$spaghetti_split_facets,
                           facet_scales = input$facet_scales,
                           mean_profiles = input$spaghetti_split_means,
                           x_scale = input$plots_x_scale,
                           y_scale = input$plots_y_scale,
                           show_mdv = input$spaghetti_mdv,
                           transparency = input$transparency)
})

output$spaghetti_idv <- renderUI({
  run <- req(rv$run)

  selectInput("spaghetti_idv", "X-axis", set_names(run$model$independent_variables$column, run$model$independent_variables$name))
})

run_residuals_plot <- reactive({
  run <- req(rv$run)

  selected_cmt <- selected_compartment()
  abs_res <- input$res_absolute

  reference_value <- smooth <- NULL

  if(input$show_reference_value){
    reference_value <- input$res_reference_value
  }
  if(input$diag_smoothing_method != "none"){
    smooth  <- input$diag_smoothing_method
  }

  plot_type <- input$residuals_plot_type

  grps <- map(input$diagnostic_split_by, as.symbol)

  hist_dist <- (if(input$residuals_histogram_theoretical_distribution || plot_type != "histogram") {
    list(fun = dnorm, args = list(mean = 0, sd = 1))
  } else  {
    NULL
  })

filtered_run() %>%
    group_by(UQS(grps)) %>%
    plot_residuals(compartment = selected_cmt,
                   idv = req(input$res_x_type), residuals = req(input$res_y_type),
                   absolute_residuals = abs_res,
                   histogram_bins = input$residuals_hist_bins,
                   histogram_empirical_density = input$residuals_histogram_empirical_density,
                   histogram_reference_distribution = hist_dist,
                   facet_scales = input$facet_scales,
                   x_scale = input$plots_x_scale, y_scale = input$plots_y_scale,
                   smoothing_method = smooth,
                   transparency = input$transparency,
                   reference_value = reference_value,
                   keep_time_zero = !input$diag_avoid_zero,
                   type = plot_type)
})

residuals_plot_extra <- reactive({
  extras <- list()
  abs_res <- input$res_absolute

  if(input$residuals_plot_type == "scatterplot"){
    y_lim <- input$res_y_limit

    if(!is.na(y_lim))
      extras <- ylim(abs(y_lim) * c(ifelse(abs_res, 0, -1), 1))
  }

  extras
})

run_individuals_plot <- reactive({
  run <- req(rv$run)

  selected_cmt <- selected_compartment()

  all_ids <- filtered_run()$tables$pmxploitab$ID %>% unique %>% sort
  id_range <- individuals_plot_range()

  selected_ids <- all_ids[id_range[1]:id_range[2]] %>% as.character %>% as.numeric()

  # Check if ID is an integer
  # -> enables generating "ids = 1:n" when generating R code for pmxploit function call
  if(all(abs(selected_ids) <= .Machine$integer.max))
    selected_ids <- as.integer(selected_ids)

  cat_cov <- input$indiv_cat_cov

  if(cat_cov == "")
    cat_cov <- NULL

  source_run <- if(input$show_indiv_mdv) filtered_run_show_mdv() else filtered_run()

  source_run %>%
    plot_individual_profiles(ids = selected_ids,
                             compartment = selected_cmt,
                             predictions = req(input$indiv_pred_type),
                             categorical_covariate = cat_cov,
                             log_dv = input$diag_log_dv,
                             idv = req(input$indiv_idv),
                             show_observations = input$show_observations,
                             predictions_dots = input$predictions_dots,
                             x_scale = input$plots_x_scale,
                             y_scale = input$plots_y_scale,
                             facet_scales = input$facet_scales,
                             keep_time_zero = !input$diag_avoid_zero)
})

# sidebar----
run_dv_cmts <- reactive({

  run <- req(rv$run)

  dv_cmts <- run$model$compartments %>% filter(dv_target == TRUE)

  setNames(dv_cmts$cmt, nm = dv_cmts$name)
})

output$cmt <- renderUI({
  # keep currently selected compartment
  current_selection <- isolate(input$cmt)

  cmt_choices <- req(run_dv_cmts())

  if(length(cmt_choices) > 1)
    cmt_choices <- c(cmt_choices, setNames(-1, paste(names(cmt_choices), collapse = "/")))

  selectInput("cmt", "Selection", choices = cmt_choices, selected = current_selection)
})

selected_compartment <- reactive({
  s_cmt <- req(input$cmt)
  run <- req(rv$run)

  cmt <- run_dv_cmts()

  if(s_cmt != "-1")
    cmt <- cmt[cmt == s_cmt]

  cmt
})

