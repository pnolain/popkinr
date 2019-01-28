
  run_parameters_distributions_plot <- reactive({
    run <- req(rv$run)

    input$parameters_refresh

    parameters_selection <- req(isolate(input$selected_parameters))
    plot_type <- input$parameters_distributions_plot_type
    split_by <- input$parameters_distributions_split_by
    only_baseline <- ifelse(input$parameters_values_type == 0, TRUE, FALSE)
    hist_bins <- input$parameters_hist_bins
    overlay_splits <- input$overlay_parameters_distributions_splits
    drop_levels <- input$parameters_distributions_drop_unused_levels
    facetted_boxplot <- input$parameters_distributions_boxplot_facetted

    grps <- map(split_by, as.symbol)

    hist_dist <- (if(input$parameters_histogram_theoretical_distribution || plot_type != "histogram") {
      list(fun = dnorm, args = list(mean = 0, sd = 1))
      } else  {
        NULL
        })

    filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      plot_parameters_distributions(parameters = parameters_selection,
                                    baseline_only = only_baseline,
                                    type = plot_type,
                                    histogram_bins = hist_bins,
                                    boxplot_drop_unused = ifelse(is.null(drop_levels), FALSE, drop_levels),
                                    boxplot_facets = facetted_boxplot,
                                    histogram_empirical_density = input$parameters_histogram_empirical_density,
                                    histogram_reference_distribution = hist_dist,
                                    facet_scales = input$parameters_distributions_facet_scales,
                                    overlay_splits = overlay_splits)
  })

  run_parameters_summary_table <- reactive({

    run <- req(rv$run)

    input$parameters_refresh

    parameters_selection <- req(input$selected_parameters)
    split_by <- input$parameters_distributions_split_by
    only_baseline <- ifelse(input$parameters_values_type == 0, TRUE, FALSE)

    grps <- map(split_by, as.symbol)

    list(data = filtered_run_show_mdv() %>%
           group_by(UQS(grps)) %>%
           summarize_parameters_distributions(parameters = parameters_selection,
                                              baseline_only = only_baseline),
         formatting = function(x) x)
  })


  individual_parameters <- reactive({

    run <- req(rv$run)

    input$parameters_refresh

    parameters_selection <- req(input$selected_parameters)
    split_by <- input$parameters_distributions_split_by
    only_baseline <- ifelse(input$parameters_values_type == 0, TRUE, FALSE)

    req(all(parameters_selection %in% na.omit(run$model$parameters$id)))

    source_run <- if(only_baseline) filtered_run_reduced_baseline() else filtered_run_reduced()

    df <- source_run$tables$pmxploitab

    param_col_names <- plyr::mapvalues(parameters_selection,
                                       run$model$parameters$id,
                                       run$model$parameters$column,
                                       warn_missing = FALSE)
    cols <- c(split_by, param_col_names)

    df <- df %>%
      select(ID, TIME, one_of(cols))

    colnames(df) <- plyr::mapvalues(colnames(df),
                                    run$model$parameters$column,
                                    run$model$parameters$name, warn_missing = FALSE)

    df
  })

  run_parameters_correlations_plot <- reactive({
    run <- req(rv$run)

    input$parameters_refresh

    parameters_selection <- req(isolate(input$selected_parameters))
    plot_type <- input$parameters_correlations_plot_type
    only_baseline <- ifelse(input$parameters_values_type == 0, TRUE, FALSE)
    corr_method <- input$parameters_correlations_method
    smooth_method <- input$parameters_correlations_smooth

    if(smooth_method == "none")
      smooth_method <- NULL

    filtered_run_show_mdv() %>%
      plot_parameters_correlations(parameters = parameters_selection,
                                   type = plot_type,
                                   baseline_only = only_baseline,
                                   fixed_ratio = FALSE,
                                   correlation_method = corr_method,
                                   smoothing_method = smooth_method)
  })

  parameters_correlations_plot_extra <- reactive({
    if(input$parameters_correlations_plot_type == "heatmap" & !is.null(corr <- rv$selected_p_p_correlation)){
      g <- run_parameters_correlations_plot()

      return(geom_tile(data = g$data %>% filter(parameter1 == corr$a$name & parameter2 == corr$b$name),
                  colour = "steelblue", fill = NA, size = 4))
    }
    return(NULL)
  })

  observeEvent(input$parameters_correlations_plot_click, {
    run <- req(rv$run)
    selected_corr <- NULL
    selected_param_names <- NULL

    df <- run_parameters_correlations_plot()$data

    if(input$parameters_correlations_plot_type != "heatmap"){

      selected_corr <- nearPoints(df, input$parameters_correlations_plot_click,
                                  xvar = "xlab", yvar = "ylab", maxpoints = 1, threshold = 300)

      if(nrow(selected_corr) > 0)
        selected_param_names <- list(a = selected_corr$xlab %>% as.character,
                                     b = selected_corr$ylab %>% as.character)

    } else {
      selected_corr <- nearPoints(df, input$parameters_correlations_plot_click,
                                  xvar = "parameter1", yvar = "parameter2", maxpoints = 1, threshold = 300)

      if(nrow(selected_corr) > 0)
        selected_param_names <- list(a = selected_corr$parameter1 %>% as.character,
                                     b = selected_corr$parameter2 %>% as.character)
    }

    if(!is.null(selected_param_names)){
      selected_corr <- map(selected_param_names, function(x){
        subset(run$model$parameters, name == x)
      })

      ns <- NS("p_p_correlation_plot")
      updateTextInput(session, inputId = ns("filename"),
                      value = sprintf("correlation-%s-%s", selected_corr$a$name, selected_corr$b$name))

      rv$selected_p_p_correlation <- selected_corr
    } else {
      rv$selected_p_p_correlation <- NULL
    }
  })

  observeEvent(input$individuals_parameters_type_selection, {
    rv$selected_p_p_correlation <- NULL
  })

  output$parameters_correlations_table <- renderDataTable({
    run <- req(rv$run)

    input$parameters_refresh

    #param_type <- input$individuals_parameters_type_selection
    parameters_selection <- req(isolate(input$selected_parameters))
    only_baseline <- ifelse(input$parameters_values_type == 0, TRUE, FALSE)
    corr_method <- req(input$parameters_correlations_method)


    df <- filtered_run_show_mdv() %>%
      summarize_parameters_correlations(parameters = parameters_selection,
                                        baseline_only = only_baseline,
                                        correlation_method = corr_method)

    corr_table <- crossing(parameter1 = rownames(df),
                           parameter2 = colnames(df)) %>%
      mutate(key = map2_chr(parameter1, parameter2, ~ str_c(sort(c(..1, ..2)), collapse = ""))) %>%
      distinct(key, .keep_all = TRUE) %>%
      select(-key) %>%
      mutate(value = map2_dbl(parameter1, parameter2, ~ df[.x, .y]))

    datatable(corr_table, rownames = FALSE, options = list(pageLength = 20, dom = 'rtip'))
  })


  selected_p_p_data <- reactive({

    run <- req(rv$run)
    selected_corr <- rv$selected_p_p_correlation

    if(is.null(selected_corr))
      return(NULL)

    a <- subset(run$model$parameters, column == selected_corr$a$column)
    b <- subset(run$model$parameters, column == selected_corr$b$column)

    df <- run$tables$pmxploitab

    if(input$parameters_values_type == "0") {
      df <- df %>%
        group_by(ID) %>%
        slice(1) %>%
        ungroup()
    } else {
      df <- df %>%
        group_by(ID, TIME) %>%
        slice(1) %>%
        ungroup()
    }

    group_column <- setdiff(input$p_p_correlations_group, "")

    df <- df %>%
      select(ID, TIME, one_of(c(a$column, b$column, group_column))) %>%
      rename(!!!setNames(c(a$column, b$column), nm = c(a$name, b$name)))

    list(df = df,
         a = a,
         b = b)
  })

  selected_p_p_correlation_plot <- reactive({
    run <- req(rv$run)
    selected_corr <- req(rv$selected_p_p_correlation)
    only_baseline <- ifelse(input$parameters_values_type == 0, TRUE, FALSE)
    smooth_method <- input$p_p_correlations_smooth

    if(smooth_method == "none")
      smooth_method <- NULL

    selected_params <- map(selected_corr, function(x){
      subset(run$model$parameters, column == x$column)
    })

    grps <- map(setdiff(input$p_p_correlations_group, ""), as.symbol)

    filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      plot_parameters_correlations(parameters = c(selected_params$a$name, selected_params$b$name),
                                   type = "scatterplot",
                                   baseline_only = only_baseline,
                                   smoothing_method = smooth_method)
  })

  output$selected_p_p_points_table <- renderDataTable({

    data <- req(selected_p_p_data())

    ns <- NS("p_p_correlation_plot")

    brush <- input[[ns("brush")]]

    selected_subjects <- tibble(ID = numeric())

    if(!is.null(brush)){
      if(all(c(data$a$name, data$b$name) %in% c(brush$mapping$x, brush$mapping$y))){
        selected_subjects <- brushedPoints(data$df, brush)
      }
    }

    datatable(selected_subjects, options = list(pageLength = 20, dom = 'rtip'))
  })



  # Individuals - covariates tab----

  output$continuous_covariates_distributions_split_by <- renderUI({
    run <- req(rv$run)

    cat_covs <- subset(run$model$covariates, type == "categorical")

    cat_choices <- NULL
    if(nrow(cat_covs) > 0)
      cat_choices <- c(cat_choices, setNames(cat_covs$column, nm = cat_covs$name))

    selectizeInput("continuous_covariates_distributions_split_by", "Split by",
                   choices = cat_choices, multiple = TRUE)
  })

  output$categorical_covariates_distributions_split_by <- renderUI({
    run <- req(rv$run)

    cat_covs <- subset(run$model$covariates, type == "categorical")

    cat_choices <- NULL
    if(nrow(cat_covs) > 0)
      cat_choices <- c(cat_choices, setNames(cat_covs$column, nm = cat_covs$name))

    selectizeInput("categorical_covariates_distributions_split_by", "Split by",
                   choices = cat_choices, multiple = TRUE)
  })

  run_continuous_covariates_distributions_plot <- reactive({
    run <- req(rv$run)

    input$continuous_covariates_refresh

    plot_type <- input$continuous_covariates_distributions_plot_type
    split_by <- input$continuous_covariates_distributions_split_by
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)
    hist_bins <- input$covariates_hist_bins
    overlay_splits <- input$overlay_continuous_covariates_distributions_splits
    drop_levels <- input$continuous_covariates_distributions_drop_unused_levels
    cont_covs <- req(isolate(input$selected_continuous_covariates))
    bp_drop <- ifelse(is.null(drop_levels), FALSE, drop_levels)

    grps <- map(split_by, as.symbol)

    filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      plot_continuous_covariates_distributions(covariates = cont_covs,
                                               baseline_only = only_baseline,
                                               type = plot_type,
                                               histogram_bins = hist_bins,
                                               boxplot_drop_unused = bp_drop,
                                               facet_scales = input$continuous_covariates_distributions_facet_scales,
                                               overlay_splits = overlay_splits)
  })

  run_categorical_covariates_distributions_plot <- reactive({
    run <- req(rv$run)

    input$categorical_covariates_refresh

    split_by <- input$categorical_covariates_distributions_split_by
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)
    bar_adjustement <- input$categorical_covariates_bar_adjustment
    show_frequency <- input$categorical_covariates_show_frequency
    order <- input$categorical_covariates_order
    drop <- input$categorical_covariates_drop
    cat_covs <- req(isolate(input$selected_categorical_covariates))

    grps <- map(split_by, as.symbol)

    g <- filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      plot_categorical_covariates_distributions(covariates = cat_covs,
                                                baseline_only = only_baseline,
                                                order = order,
                                                drop = drop,
                                                bar_adjustment = bar_adjustement,
                                                frequency = show_frequency)
    g
  })


  run_continuous_covariates_summary_table <- reactive({
    run <- req(rv$run)

    input$continuous_covariates_refresh

    split_by <- input$continuous_covariates_distributions_split_by
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)

    grps <- map(split_by, as.symbol)

    df <- filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      summarize_continuous_covariates(covariates = req(input$selected_continuous_covariates),
                                      baseline_only = only_baseline)

    df #%>%
      #mutate(range = map_chr(range, ~ str_c(.[1], .[2], sep = " - ")))
  })

  continuous_covariates <- reactive({
    run <- req(rv$run)

    input$continuous_covariates_refresh

    cov_selection <- req(input$selected_continuous_covariates)
    split_by <- input$continuous_covariates_distributions_split_by
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)

    req(all(cov_selection %in% run$model$covariates$column))

    source_run <- if(only_baseline) filtered_run_reduced_baseline() else filtered_run_reduced()

    df <- source_run$tables$pmxploitab

    cols <- c(split_by, cov_selection)

    df <- df %>%
      select(ID, TIME, one_of(cols))

    colnames(df) <- plyr::mapvalues(colnames(df),
                                    run$model$covariates$column, run$model$covariates$name, warn_missing = FALSE)

    df
  })

  run_categorical_covariates_summary_table <- reactive({
    run <- req(rv$run)

    input$categorical_covariates_refresh

    split_by <- input$categorical_covariates_distributions_split_by
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)

    grps <- map(split_by, as.symbol)

    filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      summarize_categorical_covariates(covariates = req(input$selected_categorical_covariates),
                                     baseline_only = only_baseline)
  })

  categorical_covariates <- reactive({
    run <- req(rv$run)

    input$categorical_covariates_refresh

    cov_selection <- req(input$selected_categorical_covariates)
    split_by <- input$categorical_covariates_distributions_split_by
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)

    req(all(cov_selection %in% run$model$covariates$column))

    source_run <- if(only_baseline) filtered_run_reduced_baseline() else filtered_run_reduced()

    df <- source_run$tables$pmxploitab

    cols <- c(split_by, cov_selection)

    df <- df %>%
      select(ID, TIME, one_of(cols))

    colnames(df) <- plyr::mapvalues(colnames(df),
                                    run$model$covariates$column, run$model$covariates$name, warn_missing = FALSE)

    df
  })

  run_covariates_correlations_plot <- reactive({
    run <- req(rv$run)

    input$continuous_covariates_refresh

    plot_type <- input$covariates_correlations_plot_type
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)
    corr_method <- input$covariates_correlations_method
    cont_covs <- req(isolate(input$selected_continuous_covariates))

    filtered_run_show_mdv() %>%
      plot_covariates_correlations(covariates = cont_covs,
                                   type = plot_type,
                                   baseline_only = only_baseline,
                                   fixed_ratio = FALSE,
                                   correlation_method = corr_method)
  })

  covariates_correlations_plot_extra <- reactive({
    if(input$covariates_correlations_plot_type == "heatmap" & !is.null(corr <- rv$selected_c_c_correlation)){
      g <- run_covariates_correlations_plot()

      return(geom_tile(data = g$data %>% filter(covariate1 == corr$a$name & covariate2 == corr$b$name),
                  colour = "steelblue", fill = NA, size = 4))
    }
    return(NULL)
  })

  observeEvent(input$covariates_correlations_plot_click, {
    run <- req(rv$run)
    selected_corr <- NULL
    selected_param_names <- NULL

    df <- run_covariates_correlations_plot()$data

    if(input$covariates_correlations_plot_type != "heatmap"){

      selected_corr <- nearPoints(df, input$covariates_correlations_plot_click,
                                  xvar = "xlab", yvar = "ylab", maxpoints = 1, threshold = 300)

      if(nrow(selected_corr) > 0)
        selected_param_names <- list(a = selected_corr$xlab %>% as.character,
                                     b = selected_corr$ylab %>% as.character)

    } else {
      selected_corr <- nearPoints(df, input$covariates_correlations_plot_click,
                                  xvar = "covariate1", yvar = "covariate2", maxpoints = 1, threshold = 300)

      if(nrow(selected_corr) > 0)
        selected_param_names <- list(a = selected_corr$covariate1 %>% as.character,
                                     b = selected_corr$covariate2 %>% as.character)
    }

    if(!is.null(selected_param_names)){
      selected_corr <- map(selected_param_names, function(x){
        subset(run$model$covariates, name == x)
      })

      ns <- NS("c_c_correlation_plot")
      updateTextInput(session, inputId = ns("filename"),
                      value = sprintf("correlation-%s-%s", selected_corr$a$name, selected_corr$b$name))

      rv$selected_c_c_correlation <- selected_corr
    } else {
      rv$selected_c_c_correlation <- NULL
    }
  })

  observeEvent(input$individuals_covariates_type_selection, {
    rv$selected_c_c_correlation <- NULL
  })

  output$covariates_correlations_table <- renderDataTable({
    run <- req(rv$run)

    input$continuous_covariates_refresh

    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)
    corr_method <- req(input$covariates_correlations_method)
    cont_covs <- req(isolate(input$selected_continuous_covariates))

    df <- filtered_run_show_mdv() %>%
      summarize_covariates_correlations(covariates = cont_covs,
                                        baseline_only = only_baseline,
                                        correlation_method = corr_method)

    corr_table <- crossing(covariate1 = rownames(df),
                           covariate2 = colnames(df)) %>%
      mutate(key = map2_chr(covariate1, covariate2, ~ str_c(sort(c(..1, ..2)), collapse = ""))) %>%
      distinct(key, .keep_all = TRUE) %>%
      select(-key) %>%
      mutate(value = map2_dbl(covariate1, covariate2, ~ df[.x, .y]))

    datatable(corr_table, options = list(pageLength = 20, dom = 'rtip'))
  })

  selected_c_c_data <- reactive({

    run <- req(rv$run)
    selected_corr <- rv$selected_c_c_correlation

    if(is.null(selected_corr))
      return(NULL)

    a <- subset(run$model$covariates, column == selected_corr$a$column)
    b <- subset(run$model$covariates, column == selected_corr$b$column)

    df <- run$tables$pmxploitab

    if(input$covariates_values_type == "0") {
      df <- df %>%
        group_by(ID) %>%
        slice(1) %>%
        ungroup()
    } else {
      df <- df %>%
        group_by(ID, TIME) %>%
        slice(1) %>%
        ungroup()
    }

    group_column <- setdiff(input$c_c_correlations_group, "")

    df <- df %>%
      select(ID, TIME, one_of(c(a$column, b$column, group_column))) %>%
      rename(!!!setNames(c(a$column, b$column), nm = c(a$name, b$name)))

    list(df = df,
         a = a,
         b = b)
  })

  selected_c_c_correlation_plot <- reactive({
    run <- req(rv$run)
    selected_corr <- req(rv$selected_c_c_correlation)
    only_baseline <- ifelse(input$covariates_values_type == 0, TRUE, FALSE)
    smooth_method <- input$c_c_correlations_smooth

    if(smooth_method == "none")
      smooth_method <- NULL

    selected_covs <- map(selected_corr, function(x){
      subset(run$model$covariates, column == x$column)
    })

    grps <- map(setdiff(input$c_c_correlations_group, ""), as.symbol)

    filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      plot_covariates_correlations(covariates = c(selected_covs$a$name, selected_covs$b$name),
                                   type = "scatterplot",
                                   baseline_only = only_baseline,
                                   smoothing_method = smooth_method)
  })



  output$selected_c_c_points_table <- renderDataTable({

    data <- req(selected_c_c_data())

    ns <- NS("c_c_correlation_plot")

    brush <- input[[ns("brush")]]

    selected_subjects <- tibble(ID = numeric())

    if(!is.null(brush)){
      if(all(c(data$a$name, data$b$name) %in% c(brush$mapping$x, brush$mapping$y))){
        selected_subjects <- brushedPoints(data$df, brush)
      }
    }

    datatable(selected_subjects, options = list(pageLength = 20, dom = 'rtip'))
  })


  # Individuals parameters vs covariates tab----

  run_parameters_vs_categorical_covariates_plot <- reactive({

    # refresh click
    req(input$parameters_vs_covariates_refresh > 0)

    isolate({
      run <- req(rv$run)

      params <- input$p_c_selected_parameters
      covs <- input$p_c_selected_categorical_covariates

      if(is.null(params))
        params <- input$p_c_parameters_type_selection

      only_baseline <- ifelse(input$p_c_values_type == 0, TRUE, FALSE)
    })

    filtered_run_show_mdv() %>%
      plot_parameters_vs_categorical_covariates(parameters = params,
                                                covariates = covs,
                                                baseline_only = only_baseline)
  })

  run_parameters_covariates_correlations_plot <- reactive({
    run <- req(rv$run)

    # refresh click
    input$parameters_vs_covariates_refresh

    isolate({
      params <- req(input$p_c_selected_parameters)
      covs <- req(input$p_c_selected_continuous_covariates)

      if(is.null(params))
        params <- input$p_c_parameters_type_selection

      only_baseline <- ifelse(input$p_c_values_type == 0, TRUE, FALSE)
    })
    corr_method <- input$parameters_covariates_correlations_method

    smooth_method <- input$covariates_correlations_smooth

    if(smooth_method == "none")
      smooth_method <- NULL

    filtered_run_show_mdv() %>%
      plot_parameters_vs_continuous_covariates(parameters = params,
                                               covariates = covs,
                                               type = "heatmap",
                                               fixed_ratio = FALSE,
                                               baseline_only = only_baseline,
                                               correlation_method = corr_method,
                                               smoothing_method = smooth_method)
  })

  parameters_covariates_correlations_plot_extra <- reactive({
    if(!is.null(corr <- rv$selected_p_c_correlation)){
      g <- run_parameters_covariates_correlations_plot()

      return(geom_tile(data = g$data %>% filter(parameter == corr$parameter$name & covariate == corr$covariate$name),
                       colour = "steelblue", fill = NA, size = 4))
    }
    return(NULL)
  })


  observeEvent(input$parameters_covariates_correlations_plot_click, {
    run <- req(rv$run)
    selected_corr <- NULL
    selected_names <- NULL

    df <- run_parameters_covariates_correlations_plot()$data

    selected_corr <- nearPoints(df, input$parameters_covariates_correlations_plot_click,
                                xvar = "parameter", yvar = "covariate", maxpoints = 1, threshold = 300)

    if(nrow(selected_corr) > 0)
      selected_names <- list(parameter = selected_corr$parameter %>% as.character,
                             covariate = selected_corr$covariate %>% as.character)

    if(!is.null(selected_names)){
      selected_corr$parameter <- subset(run$model$parameters, name == selected_corr$parameter)
      selected_corr$covariate <- subset(run$model$covariates, name == selected_corr$covariate)

      ns <- NS("p_c_correlation_plot")
      updateTextInput(session, inputId = ns("filename"),
                      value = sprintf("correlation-%s-%s", selected_corr$parameter$name, selected_corr$covariate$name))

      rv$selected_p_c_correlation <- selected_corr
    } else {
      rv$selected_p_c_correlation <- NULL
    }
  })


  observeEvent(input$p_c_parameters_type_selection, {
    rv$selected_p_c_correlation <- NULL
  })

  output$parameters_covariates_correlations_table <- renderDataTable({
    run <- req(rv$run)

    # refresh click
    input$parameters_vs_covariates_refresh

    isolate({
      params <- req(input$p_c_selected_parameters)
      covs <- req(input$p_c_selected_continuous_covariates)
    })

    if(is.null(params))
      params <- input$p_c_parameters_type_selection

    only_baseline <- ifelse(input$p_c_values_type == 0, TRUE, FALSE)
    corr_method <- req(input$parameters_covariates_correlations_method)

    df <- filtered_run_show_mdv() %>%
      summarize_parameters_vs_continuous_covariates(parameters = params,
                                              covariates = covs,
                                              baseline_only = only_baseline,
                                              correlation_method = corr_method)


    corr_table <- crossing(parameter = rownames(df),
                           covariate = colnames(df)) %>%
      mutate(key = map2_chr(parameter, covariate, ~ str_c(sort(c(..1, ..2)), collapse = ""))) %>%
      distinct(key, .keep_all = TRUE) %>%
      select(-key) %>%
      mutate(value = map2_dbl(parameter, covariate, ~ df[.x, .y]))

    datatable(corr_table, options = list(pageLength = 20, dom = 'rtip'))
  })


  selected_p_c_data <- reactive({

    run <- req(rv$run)
    selected_corr <- rv$selected_p_c_correlation

    if(is.null(selected_corr))
      return(NULL)

    parameter <- subset(run$model$parameters, column == selected_corr$parameter$column)
    covariate <- subset(run$model$covariates, column == selected_corr$covariate$column)

    df <- run$tables$pmxploitab

    if(input$p_c_values_type == "0") {
      df <- df %>%
        group_by(ID) %>%
        slice(1) %>%
        ungroup()
    } else {
      df <- df %>%
        group_by(ID, TIME) %>%
        slice(1) %>%
        ungroup()
    }

    group_column <- setdiff(input$p_c_correlations_group, "")

    df <- df %>%
      select(ID, TIME, one_of(c(parameter$column, covariate$column, group_column))) %>%
      rename(!!!setNames(c(parameter$column, covariate$column), nm = c(parameter$name, covariate$name)))

    list(df = df,
         parameter = parameter,
         covariate = covariate)
  })

  selected_p_c_correlation_plot <- reactive({
    run <- req(rv$run)
    selected_corr <- req(rv$selected_p_c_correlation)

    only_baseline <- ifelse(input$p_c_values_type == 0, TRUE, FALSE)
    smooth_method <- input$p_c_correlations_smooth

    if(smooth_method == "none")
      smooth_method <- NULL

    selected_param <- subset(run$model$parameters, column == selected_corr$parameter$column)
    selected_cov <- subset(run$model$covariates, column == selected_corr$covariate$column)

    grps <- map(setdiff(input$p_c_correlations_group, ""), as.symbol)

    g <- filtered_run_show_mdv() %>%
      group_by(UQS(grps)) %>%
      plot_parameters_vs_continuous_covariates(parameters = selected_param$name,
                                             covariates = selected_cov$name,
                                             type = "scatterplot",
                                             baseline_only = only_baseline,
                                             smoothing_method = smooth_method)
    g
  })


  output$selected_p_c_points_table <- renderDataTable({

    data <- req(selected_p_c_data())

    ns <- NS("p_c_correlation_plot")

    brush <- input[[ns("brush")]]

    selected_subjects <- tibble(ID = numeric())

    if(!is.null(brush)){
      if(all(c(data$parameter$name, data$covariate$name) %in% c(brush$mapping$x, brush$mapping$y))){
        selected_subjects <- brushedPoints(data$df, brush)
      }
    }

    datatable(selected_subjects, options = list(pageLength = 20, dom = 'rtip'))
  })
