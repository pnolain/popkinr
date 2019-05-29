# clean up
session$onSessionEnded(function() {
  if(cleanup_when_ended & dir.exists(app_temp_directory)){
    unlink(app_temp_directory, recursive = TRUE)
  }

  save_xml_data()

  # stopApp()
})

save_xml_data <- function(){
  r_hist <- read_previous_runs()

  if(!is.null(r_hist) & nrow(r_hist) > 0){
    runs_history <- r_hist %>%
      filter(path != rv$run$info$path) %>%
      add_row(date = lubridate::now(), path = rv$run$info$path) %>%
      arrange(desc(date)) %>%
      slice(1:20)
  }

  root_node <- xml_new_root("popkinr")

  new_pmxploit_node <- root_node %>%
    xml_add_child("pmxploit")

  history_node <- new_pmxploit_node %>%
    xml_add_child("history")

  walk2(runs_history$date, runs_history$path,  function(date, path) {
    history_node %>%
      xml_add_child("run") %>%
      xml_set_attrs(c(date = as.character(lubridate::as_datetime(date)), path = path))
  })

  if(file.exists(app_xml_path)){
    doc <- read_xml(app_xml_path)

    pmxploit_node <- doc %>% xml_find_first("/popkinr/pmxploit")

    if(!is.na(pmxploit_node)){
      xml_replace(pmxploit_node, new_pmxploit_node)

      write_xml(doc, app_xml_path)
    } else {
      popkinr_node <-  doc %>% xml_find_first("//popkinr")

      if(!is.na(popkinr_node)){
        popkinr_node %>%
          xml_add_child(new_pmxploit_node)

        write_xml(doc, app_xml_path)
      } else {
        write_xml(root_node, app_xml_path)
      }
    }
  } else {
    write_xml(root_node, app_xml_path)
  }
}



# reactive values----
rv <- reactiveValues(run_path = initial_run_path,
                     previous_runs = startup_last_runs,
                     run = NULL,
                     run_initial_metadata = NULL,
                     first_individual_id = NULL,
                     current_page = NULL,
                     current_layout = NULL,
                     selected_estimation = NULL,
                     selected_p_p_correlation = NULL,
                     selected_p_c_correlation = NULL,
                     selected_c_c_correlation = NULL,
                     app_filters = NULL,
                     app_notifications = NULL)


output$open_last_run <- renderUI({
  req(nrow(rv$previous_runs) > 0)

  isolate({

    line_ui <- function(i, path){
      load_link <- paste0("loadlink", i)

      local({
        if(is.null(input[[load_link]])){
          observeEvent(eventExpr = input[[load_link]],
                       handlerExpr = {
                         app <- rv$previous_runs[i,]

                         if(!file.exists(app$path) && !dir.exists(app$path)){
                           toastr_error(paste("Run not found:", app$path), title = "Run loading",
                                        position = "top-center", timeOut = 10000)
                           rv$previous_runs <- rv$previous_runs %>%
                             filter(path != app$path)
                         } else {
                           run_browser()$reset(folder_selection = dirname(app$path),
                                               file_selection = app$path)
                           metadata_browser()$reset(folder_selection = dirname(app$path))

                           #open_run(app$path)
                           rv$run_path <- app$path
                         }
                       })
        }
      })

      tags$li(
        tags$span(actionLink(load_link, path)))
    }

    runs_ui <-  seq_along(rv$previous_runs$path) %>%
      map(function(i){
        path <- rv$previous_runs$path[i]

        line_ui(i, path)
      })
  })

  div(style = "overflow: auto;",
      tags$ol(rev(runs_ui)))
})

observe({

  selection_is_nm_run <- FALSE

  if(length(run_browser()$file) == 1){
    selection_is_nm_run <- str_detect(run_browser()$file, "(\\.tar\\.gz|\\.zip)$")
  } else {
    dir_path <- run_browser()$folder

    if(!is.null(dir_path)){
      dir_files <- list.files(dir_path, ignore.case = TRUE)

      selection_is_nm_run <- all(c("xml", "ext") %in% safe_file_ext(dir_files)$result)
    }
  }

  shinyjs::toggleState("load_run", condition = selection_is_nm_run)
})

observeEvent(input$load_run, {
  if(length(run_browser()$file) == 1){
    rv$run_path <- run_browser()$file
  } else {
    rv$run_path <- run_browser()$folder
  }
})

observeEvent(rv$run_path, {
  path <- req(rv$run_path)

  if(file.exists(path)) # do not run for Demo run
    open_run(path)
})

add_run_to_history <- function(run){
  # register current run as last opened run
  if(!is.null(run) & !is.null(run$info$path)){
    run_path <- run$info$path

    # if run is in history, remove it
    if(run_path %in% rv$previous_runs$path)
      rv$previous_runs <- rv$previous_runs %>% filter(path != run_path)

    rv$previous_runs <-  rv$previous_runs %>%
      add_row(date = lubridate::now(), path = run_path)

    # keep only 20 runs in history
    if(nrow(rv$previous_runs) > 20){
      rv$previous_runs <- rv$previous_runs %>%
        slice((n()-19):n())
    }

  }
}



open_run <- function(run_path){
  notifs <- NULL

  is_directory <- file.info(run_path)$isdir

  # safely & quietly
  safe_load <- safely(load_nm_run)
  quiet_load <- quietly(safe_load)

  progress <- shiny::Progress$new(session)
  progress$set(message = "Loading run", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())


  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }

  if(!is_directory){
    run_load <- quiet_load(path = run_path,
                           temp_directory = app_temp_directory,
                           keep_tempfiles = TRUE,
                           verbose = TRUE,
                           update_progress = updateProgress)
  } else {
    run_load <- quiet_load(path = run_path,
                           verbose = TRUE,
                           update_progress = updateProgress)
  }


  if(length(run_load$warnings) > 0){
    app_warnings <- map_df(run_load$warnings, function(x){
      tibble(message = x, status = "warning")
    }, .id = NULL)

    notifs <- app_warnings
  }

  if(length(run_load$result$error) > 0){
    app_errors <- map_df(run_load$result$error$message, function(x){
      tibble(message = x, status = "error")
    }, .id = NULL)

    notifs <- bind_rows(notifs, app_errors)
  }

  comparison_browser()$reset(folder_selection = dirname(run_path))

  if(!is.null(run_load$result$error)) {
    updateProgress(value = 1, detail = "Error !")

    rv$run <- NULL
    rv$run_initial_metadata <- NULL

  } else {
    rv$run <- run_load$result$result
    rv$run_initial_metadata <- rv$run$model

    save_xml_data()

    rv$previous_runs <- read_previous_runs()

    # perform some run checks
    if(nrow(rv$run$model$covariates) == 0) {
      notifs <- bind_rows(notifs, list(status = "warning", message = "No covariate found in tables."))
    } else {
      if(nrow(subset(rv$run$model$covariates, type == "categorical")) == 0)
        notifs <- bind_rows(notifs, list(status = "message", message = "No categorical covariate found in tables."))
      if(nrow(subset(rv$run$model$covariates, type == "continuous")) == 0)
        notifs <- bind_rows(notifs, list(status = "message", message = "No continuous covariate found in tables."))
    }
    if(nrow(subset(rv$run$model$parameters, !is.na(column))) == 0) {
      notifs <- bind_rows(notifs, list(status = "warning", message = "No parameter found in tables."))
    } else {
      if(nrow(subset(rv$run$model$parameters, type == "eta" & !is.na(column))) == 0)
        notifs <- bind_rows(notifs, list(status = "warning", message = "No random (ETA) parameter found in tables."))
    }

    # session$sendCustomMessage("collapse_box", message = "recent_box")
    shinyjs::runjs("$('#recent_box').closest('.box').find('[data-widget=collapse]').click();")

    removeModal()

    add_run_to_history(rv$run)
  }


  if(!is.null(notifs)) {
    walk2(notifs$message, notifs$status, function(message, status){
      toastr_func <- switch(status,
                            "warning" = { shinytoastr::toastr_warning },
                            "error" = { shinytoastr::toastr_error },
                            "message" = { shinytoastr::toastr_info })

      toastr_func(message, position = "bottom-right", timeOut = 10000)
    })

    rv$app_notifications <- notifs
  } else {
    shinytoastr::toastr_success("Run successfully loaded",
                                title = "Done",
                                position = "bottom-right")
    rv$app_notifications <- NULL
  }

  reset_ui()
}


observe({
  run <- req(rv$run)

  cat_covs <- subset(run$model$covariates, type == "categorical")
  cont_covs <- subset(run$model$covariates, type == "continuous")

  cat_choices <- setNames(cat_covs$column, nm = cat_covs$name)
  cont_choices <- setNames(cont_covs$column, nm = cont_covs$name)

  # Params vs Covariates tab
  params <- subset(run$model$parameters, type == input$p_c_parameters_type_selection & !is.na(column))
  params_choices <- setNames(as.character(params$id), nm = params$name)

  cont_selection <- seq_along(cont_choices)
  cat_selection <- seq_along(cat_choices)
  params_selection <- seq_along(params_choices)

  # if(input$covariates_type_tab == "categorical_tab"){
  #   if(length(cat_choices) > 3){
  #     cat_selection <- 1:3
  #     params_selection <- 1:3
  #   }
  # }

  updateSelectizeInput(session, "p_c_selected_parameters", server = TRUE,
                       choices = params_choices, selected = params_choices[params_selection])
  updateSelectizeInput(session, "p_c_selected_continuous_covariates", server = TRUE,
                       choices = cont_choices, selected = cont_choices[cont_selection])
  updateSelectizeInput(session, "p_c_selected_categorical_covariates", server = TRUE,
                       choices = cat_choices, selected = cat_choices[cat_selection])
  updateSelectizeInput(session, "p_c_correlations_group", server = TRUE,
                       choices = cat_choices)

  # Parameters tab
  params_list <- subset(run$model$parameters, type == input$individuals_parameters_type_selection & !is.na(column))
  selected_parameters <- setNames(as.character(params_list$id), nm = params_list$name)
  updateSelectizeInput(session, "selected_parameters", server = TRUE,
                       choices = selected_parameters, selected = selected_parameters)
  updateSelectizeInput(session, "parameters_distributions_split_by", server = TRUE,
                       choices = cat_choices)
  updateSelectizeInput(session, "p_p_correlations_group", server = TRUE,
                       choices = cat_choices)

  # Covariates tab
  updateSelectizeInput(session, "selected_continuous_covariates", server = TRUE,
                       choices = cont_choices, selected = cont_choices)
  updateSelectizeInput(session, "continuous_covariates_distributions_split_by", server = TRUE,
                       choices = cat_choices)
  updateSelectizeInput(session, "categorical_covariates_distributions_split_by", server = TRUE,
                       choices = cat_choices)
  updateSelectizeInput(session, "selected_categorical_covariates", server = TRUE,
                       choices = cat_choices, selected = cat_choices)
  updateSelectizeInput(session, "c_c_correlations_group", server = TRUE,
                       choices = cat_choices)

  # updateCheckboxInput(session, "diag_avoid_zero", value = TRUE)

  updateCheckboxInput(session, "transparency", value = run$info$number_of_observations > 1000)

  cat_cmt_choices <- cat_choices

  if(nrow(filter(run$model$compartments, dv_target)) > 1)
    cat_cmt_choices = c(CMT = "CMT", cat_cmt_choices)

  # QC tab
  updateSelectizeInput(session, "qc_split_by", server = TRUE,
                       choices = cat_cmt_choices, selected = NULL)

  # Diagnostics tab
  diag_cat_cmt_choices <- cat_choices
  diag_cat_cmt_choices_selection <- NULL
  if(!is.null(input$cmt) && input$cmt == -1){
    diag_cat_cmt_choices <-  cat_cmt_choices
    diag_cat_cmt_choices_selection <- "CMT"
  }

  updateSelectizeInput(session, "indiv_cat_cov", server = TRUE,
                       choices = cat_choices, selected = '')

  updateSelectizeInput(session, "diagnostic_split_by", server = TRUE,
                       choices = diag_cat_cmt_choices, selected = diag_cat_cmt_choices_selection)
})


observe({
  shinyjs::toggle("res_histogram_widgets", condition = input$residuals_plot_type == "histogram")
  shinyjs::toggle("res_scatterplot_widgets", condition = input$residuals_plot_type == "scatterplot")
  shinyjs::toggle("res_reference_value", condition = input$show_reference_value)
})


observe({
  run <- req(rv$run)

  has_rights <- unname(file.access(run$info$path, mode = 2)) == 0
  shinyjs::toggleState("save_metadata", condition = has_rights)
  shinyjs::toggleState("save_all_individual_profiles", condition = has_rights)
  shinyjs::toggleState("estimation_save_report", condition = has_rights)
  shinyjs::toggleState("qc_save_report", condition = has_rights && !is.null(input$qc_pred_type))
  shinyjs::toggleState("outliers_save_report", condition = has_rights && !is.null(input$outliers_restype))

  shinyjs::toggleState("compute_qc_comparison", condition = length(run$model$predictions) > 0)
})

filtered_run <- reactive({
  run <- req(rv$run)

  run %>%
    filter(UQS(rv$app_filters))
})

filtered_run_show_mdv <- reactive({
  run <- req(rv$run)

  no_mdv_filters <- discard(rv$app_filters, ~ all.equal(., quo(MDV == 0)) == TRUE)

  run %>%
    filter(UQS(no_mdv_filters))
})

filtered_run_reduced_baseline <- reactive({
  temp_run <- req(filtered_run_show_mdv())

  temp_run %>%
    group_by(ID) %>%
    slice(1) %>%
    ungroup()
})

filtered_run_reduced <- reactive({
  temp_run <- req(filtered_run_show_mdv())

  temp_run %>%
    group_by(ID, TIME) %>%
    slice(1) %>%
    ungroup()
})

reset_filters <- function(){
  rv$app_filters <- list(quo(MDV == 0))
}

reset_ui<-function(){
  run <- rv$run
  rv$selected_estimation <- NULL

  if(!is.null(run))
    rv$selected_estimation <- last(run$estimations)

  rv$first_individual_id <- NULL
  rv$current_page <- 1
  rv$selected_p_p_correlation <- NULL
  rv$selected_p_c_correlation <- NULL
  rv$selected_c_c_correlation <- NULL

  rv_vpc$future <- NULL
  rv_vpc$progress_bar <- NULL
  rv_vpc$n_simulations <- NULL
  rv_vpc$control_stream <- NULL
  rv_vpc$results <- NULL

  reset_filters()
}



output$application_top_text <- renderUI({
  req(rv$run)

  tags$span(style = "line-height: 50px; color: white",
            tags$strong(sprintf("%s", rv$run$info$path)))
})


observeEvent(input$example, {
  rv$run <- pmxploit:::EXAMPLERUN

  run_browser()$reset()
  metadata_browser()$reset()

  rv$run_path <- pmxploit:::EXAMPLERUN$info$path

  rv_comp$runs_to_compare <- NULL

  reset_ui()
})

# filters----

observeEvent(input$show_filters, {
  showModal(modalDialog(
    title = "Filters",
    size = "l",
    flowLayout(uiOutput("filter_columns"),
               selectInput("filter_operator", "Condition",
                           choices = c("Equal to (==)" = "==",
                                       "Different than (!=)" = "!=",
                                       "Greater than (>)" = ">",
                                       "Greater than or equal to (>=)" = ">=",
                                       "Lower than (<)" = "<",
                                       "Lower than or equal to (<=)" = "<=")),
               uiOutput("filter_value")),
    div(align = "center", actionButton("add_filter", "Add filter")),
    hr(),
    h4("Current filters"),
    dataTableOutput("filters_table"),
    h6("* Click on rows to remove filters"),
    hr(),
    h4("Filtered data"),
    dataTableOutput("filtered_dataset"),
    footer = NULL,
    easyClose = TRUE)
  )
})


observeEvent(input$filters_table_rows_selected, {
  selected_row <- input$filters_table_rows_selected

  if(length(selected_row) == 1){
    selected_row <- as.numeric(selected_row)
    # row_out <- rv$app_filters %>% slice(selected_row)
    #
    # updateSelectInput(session, "filter_columns", selected = row_out$column)
    # updateSelectInput(session, "filter_operator", selected = row_out$operator)

    rv$app_filters <- rv$app_filters[-selected_row]
  }

})

output$filter_columns <- renderUI({
  run <- req(rv$run)

  filter_col_names <- colnames(run$tables$pmxploitab)

  selectInput("filter_columns", "Variable (column)", choices = filter_col_names)
})

observeEvent(input$add_filter,{
  filters <- rv$app_filters

  val <- req(input$filter_value)

  # conversion
  val <- ifelse(val == "NA", NA, as.numeric(val))
  op <- input$filter_operator
  col <- input$filter_columns

  filter_formula <- switch(as.character(val),
                           `NA` = {
                             switch(as.character(op),
                                    "==" = {
                                      sprintf("is.na(%s)", col)
                                    },
                                    "!=" = {
                                      sprintf("!is.na(%s)", col)
                                    },
                                    {
                                      stop("Operator incompatible with filtering on NA values.")
                                    })
                           }, {
                             sprintf("%s %s %s", col, op, val)
                           })

  new_filter <- parse_quo(filter_formula, env = caller_env())

  if (is.null(filters)) {
    filters <- list(new_filter)
  }else{
    filters <- c(filters, new_filter)
  }

  rv$app_filters <- filters
})

output$filter_value <- renderUI({
  run <- req(rv$run)
  col <- req(input$filter_columns)

  if(is.factor(run$tables$pmxploitab[[col]])){
    cat_levels <- NULL

    if(!(col %in% c("MDV", "EVID"))){
      cat_levels <- run$model$categorical_covariates_levels[[col]]
    } else {
      cat_levels <- levels(run$tables$pmxploitab[[col]])
    }

    if(!is.null(cat_levels)) {
      selectInput("filter_value", "Value", choices = setNames(cat_levels, nm = names(cat_levels)))
    } else if(col == "CMT"){
      cmt_dv <- run$model$compartments %>% filter(dv_target)
      selectInput("filter_value", "Value", choices = setNames(cmt_dv$cmt, nm = cmt_dv$name))
    } else {
      vals <- unique(run$tables$pmxploitab[[col]])
      if(is.factor(vals)){
        if(all(str_detect(as.character(vals), "^[0-9\\.]+"))){
          vals <- as.numeric(as.character(vals))
        }
      }
      selectInput("filter_value", "Value", choices = sort(vals))
    }

  } else {
    numericInput("filter_value", "Value", value = NA)
  }
})

output$filters_table <- renderDataTable({
  filters <- rv$app_filters

  if(is.null(filters)){
    filters <- data.frame(condition = character())#, operator = character(), value = character())
  } else {
    filters <- data.frame(condition = as.character(filters))
    # run <- req(rv$run)
    # filters$value <- as.character(filters$value)
    #
    # filters <- filters %>%
    #   mutate(row = row_number()) %>%
    #   split(.$row) %>%
    #   map_df(function(x){
    #     cat_levels <- run$model$categorical_covariates_levels[[x$column]]
    #     if(!is.null(cat_levels)) {
    #       x$value <- plyr::mapvalues(x$value, from = cat_levels, to = names(cat_levels), warn_missing = FALSE)
    #     } else if(x$column == "CMT") {
    #       cmt_dv <- run$model$compartments %>% filter(dv_target)
    #       x$value <- plyr::mapvalues(x$value, from = as.character(cmt_dv$cmt), to = cmt_dv$name, warn_missing = FALSE)
    #     }
    #     x
    #   }) %>% select(-row)
  }

  datatable(filters, options = list(paging = FALSE, info = FALSE, searching = FALSE,
                                    language = list(emptyTable = "No filter")))
})




temp_filtered_dataset <- reactive({
  req(filtered_run())$tables$pmxploitab
})

output$filtered_dataset <- renderDataTable({

  filtered_ds <- req(temp_filtered_dataset())

  datatable(filtered_ds,
            options = list(pageLength = 10, scrollX = TRUE, dom = 'rtip'))
})


save_rmd_to_run <- function(output_filename, rmd_file, format, rmd_params){
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

    temp_subdir <- paste(ifelse(is_directory, run_path, temp_dir), "pmxploit", "reports", sep = "/")

    if(!dir.exists(temp_subdir))
      dir.create(temp_subdir, recursive = TRUE)

    temp_file <- paste(temp_subdir, output_filename, sep = "/")

    setProgress(detail = "Report generation...")

    out <- rmarkdown::render(rmd_file,
                             output_file = temp_file,
                             output_format = format,
                             params = rmd_params)

    setProgress(detail = sprintf("Updating run %s...", source_type))

    if(!is_directory){
      wd <- setwd(run_parent_dir)

      system2("tar", sprintf("-czvf '%s' -C '%s' .", basename(run_path), temp_dir), stdout = FALSE)

      setwd(wd)

      unlink(temp_dir, recursive = TRUE)
    }

    setProgress(1, detail = "Done !")
  }, message = sprintf("Saving report to run %s", source_type))
}
