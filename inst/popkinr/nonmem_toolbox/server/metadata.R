
# metadata tab----
output$compartments_metadata <- renderRHandsontable({
  run <- req(rv$run)
  df <- req(run$model$compartments) %>%
    mutate(dv_target = ifelse(dv_target, "yes", "no"))

  rhandsontable(df) %>%
    hot_col(c("cmt", "dv_target", "type"), readOnly = TRUE)
})

output$idv_metadata <- renderRHandsontable({
  run <- req(rv$run)
  df <- req(run$model$independent_variables)

  rhandsontable(df, readOnly = TRUE) %>%
    hot_col(c("column"), readOnly = TRUE)
})

output$thetas_metadata <- renderRHandsontable({
  run <- req(rv$run)
  df <- run$model$parameters %>% filter(type == "theta")

  rhandsontable(select(df, id, name), readOnly = TRUE)
})

output$etas_metadata <- renderRHandsontable({
  run <- req(rv$run)
  df <- run$model$parameters %>% filter(type == "eta")

  rhandsontable(select(df, id, name, column), readOnly = TRUE)
})

output$parameters_metadata <- renderRHandsontable({
  run <- req(rv$run)
  df <- run$model$parameters %>% filter(type == "individual")

  rhandsontable(select(df, id, name, column)) %>%
    hot_col(c("id", "column"), readOnly = TRUE)
})

output$covariates_metadata <- renderRHandsontable({
  run <- req(rv$run)
  df <- run$model$covariates

  rhandsontable(as.data.frame(df)) %>%
    hot_col(c("column"), readOnly = TRUE)
})

output$metadata_categorical_covariates <- renderUI({
  run <- req(rv$run)

  cat_covs <- subset(run$model$covariates, type == "categorical")

  cat_choices <- c("-")
  if(nrow(cat_covs) > 0)
    cat_choices <- c(cat_choices, setNames(cat_covs$column, nm = sprintf("%s (%s)", cat_covs$name, cat_covs$column)))

  selectInput("metadata_categorical_covariates",
              "Categorical covariates",
              choices = cat_choices)
})

output$selected_categorical_covariate_metadata <- renderRHandsontable({
  run <- req(rv$run)

  selected_cov <- req(input$metadata_categorical_covariates)

  req(selected_cov != "-")

  values <- run$model$categorical_covariates_levels[[selected_cov]]
  labels <- names(values)

  df <- tibble(value = values,
                   label = labels) %>%
    arrange(as.character(value))

  rhandsontable(df) %>%
    hot_col("value", readOnly = TRUE)
})

observeEvent(input$update_categorical_levels, {
  cov_df <- req(input$selected_categorical_covariate_metadata) %>% hot_to_r
  selected_cov <- req(input$metadata_categorical_covariates)

  if(selected_cov != "-" & !(anyDuplicated(cov_df$name))){

    col_data <- cov_df %>%
      split(.$label) %>%
      map_df(function(x) x$value) %>% unlist()

    col <- list(col_data)
    names(col) <- selected_cov

    new_run <- recode_categorical_covariates(run = rv$run,
                                             covariates_levels = col)

    rv$run <- new_run
  }
})

observeEvent(input$compartments_metadata,{
  cmts_df <- req(input$compartments_metadata) %>% hot_to_r

  if(!(anyDuplicated(cmts_df$name)))
    rv$run$model$compartments$name <- cmts_df$name
})

observeEvent(input$parameters_metadata,{
  req(length(input$parameters_metadata$data) > 0)
  params_df <- input$parameters_metadata %>% hot_to_r

  mixed_params_ids <- which(rv$run$model$parameters$type == "individual")

  if(!(anyDuplicated(params_df$name)))
    rv$run$model$parameters$name[mixed_params_ids] <- params_df$name
})

observeEvent(input$covariates_metadata,{
  req(length(input$covariates_metadata$data) > 0)
  covs_df <- input$covariates_metadata %>% hot_to_r

  if(!(anyDuplicated(covs_df$name))) {
    rv$run$model$covariates$name <- covs_df$name

    changed_type_cols <- which(rv$run$model$covariates$type !=
                                 factor(covs_df$type, levels = levels(rv$run$model$covariates$type), ordered= FALSE))

    # update run pmxploitab
    if(length(changed_type_cols) > 0){
      rv$run <- switch_covariates_type(rv$run, covariates = rv$run$model$covariates[changed_type_cols,]$column)
    }
  }
})

observeEvent(input$browse_metadata, {
  metadata_browser()$initialize_ui(force = TRUE)

  showModal(modalDialog(
    title = "Select a run to read metadata from",
    size = "l",
    popkinr::serverBrowserUI("metadata_browser"),
    footer = list(modalButton("Close"),
                  actionButton("import_metadata", "Import metadata")),
    easyClose = TRUE)
  )
})

observeEvent(input$import_metadata, {
  source_path <- metadata_browser()$file

  if(length(source_path) == 0)
    source_path <- metadata_browser()$folder

  req(source_path)

  is_directory <- file.info(source_path)$isdir

  model_mapping <- NULL

  if(!is_directory){

    files <- untar(source_path, list = TRUE)

    if(length(mdata <- files[files == "./pmxploit_metadata.rds"]) > 0){
      untar(source_path, files = mdata, exdir = str_c(local_app_folder, "temp", sep = "/"))
      model_mapping <- readRDS(str_c(local_app_folder, "temp/pmxploit_metadata.rds", sep = "/"))
      unlink(str_c(local_app_folder, "temp", sep = "/"), recursive = TRUE)
    }
  } else {
    metadata_path <- str_c(source_path, "pmxploit_metadata.rds", sep = "/")

    if(file.exists(metadata_path))
      model_mapping <- readRDS(str_c(source_path, "pmxploit_metadata.rds", sep = "/"))
  }

  if(is.null(model_mapping)){
    toastr_error("No metadata file (pmxploit_metadata.rds) found in run files", title = "Metadata import",
                 position = "top-center", timeOut = 10000)

    return(NULL)
  } else {
    if(!identical(attr(model_mapping, "pmxploit_version"), packageDescription("pmxploit")$Version)){
      toastr_warning("Run metadata were saved with a different version of pmxploit package, you may need to clear it.",
                     title = "Metadata import", timeOut = 10000)
    }
  }

  # old version : replace all mapping----
  #rv$run$model <- model_mapping

  # new version : selective replacement----
  up_cmt <- rv$run$model$compartments %>%
    full_join(model_mapping$compartments, by = c("cmt")) %>%
    rename(dv_target = dv_target.x) %>%
    mutate(name = ifelse(!is.na(name.y), name.y, name.x)) %>%
    select(cmt, name, dv_target)

  rv$run$model$compartments <- up_cmt

  up_ip <- rv$run$model$parameters %>%
    filter(type == "individual") %>%
    full_join(filter(model_mapping$parameters, type == "individual"), by = c("id", "type", "column", "n")) %>%
    filter(!is.na(name.x) & !is.na(name.y))

  if(nrow(up_ip) > 0){
    rv$run$model$parameters$name <- plyr::mapvalues(rv$run$model$parameters$name, up_ip$name.x, up_ip$name.y)
  }

  up_cov <- rv$run$model$covariates %>%
    full_join(model_mapping$covariates, by = c("column")) %>%
    mutate(name = ifelse(!is.na(name.y), name.y, name.x),
           type = factor(ifelse(!is.na(type.y), as.character(type.y), as.character(type.x)),
                         levels = c("categorical", "continuous"))) %>%
    select(column, type, name)

  rv$run$model$covariates <- up_cov

  up_cat_cov <- filter(up_cov, type == "categorical") %>% select(column) %>% pull

  up_cat_cov_levels <- model_mapping$categorical_covariates_levels[up_cat_cov]

  rv$run$model$categorical_covariates_levels <- map(names(rv$run$model$categorical_covariates_levels), function(x){
    current_lvls <- rv$run$model$categorical_covariates_levels[[x]]
    if(is.null(up_cat_cov_levels[[x]])){
      current_lvls
    } else {
      up_levels <- up_cat_cov_levels[[x]]
      new_lvls <- current_lvls

      for(i in seq_along(up_levels)){
        if(up_levels[[i]] %in% current_lvls){
          names(current_lvls)[current_lvls == up_levels[[i]]] <- names(up_levels)[i]
        }
      }
      current_lvls
    }
  })

  removeModal()
})

observeEvent(input$save_metadata, {
  run <- req(rv$run)
  run_path <- req(rv$run_path)

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

    temp_subdir <- paste(ifelse(is_directory, run_path, temp_dir), #"pmxploit",
                         sep = "/")

    if(!dir.exists(temp_subdir))
      dir.create(temp_subdir)

    model_mapping <- req(rv$run$model)
    saveRDS(model_mapping, file = paste(temp_subdir, "pmxploit_metadata.rds", sep = "/"))

    if(!is_directory){
      wd <- setwd(run_parent_dir)

      system2("tar", sprintf("-czvf '%s' -C '%s' .", basename(run_path), temp_dir), stdout = FALSE)

      setwd(wd)

      unlink(temp_dir, recursive = TRUE)
    }

    setProgress(1, detail = "Done !")
  }, message = sprintf("Saving metadata to run %s", source_type), detail = "Updating run...")
})


observeEvent(input$clear_metadata, {
  orig_metadata <- (if(!is.null(rv$run$model$internal)) rv$run$model$internal else rv$run_initial_metadata)

  rv$run$model <- orig_metadata
})

observeEvent(input$reset_metadata, {
  rv$run$model <- req(rv$run_initial_metadata)
})


output$unknown_metadata <- renderUI({
  run <- req(rv$run)

  df <- req(run$tables$pmxploitab)

  c_names <- colnames(df)

  unknown <- setdiff(c_names,
                     c(run$model$covariates$column,
                       na.omit(run$model$parameters$column),
                       run$model$independent_variables$column,
                       run$model$residuals,
                       run$model$predictions,
                       pmxploit::nm_reserved_names))

  checkboxGroupInput("unknown_metadata", "Select", choices = unknown)
})

observeEvent(input$add_to_categorical_cov, {
  u_sel <- req(input$unknown_metadata)

  rv$run$model$covariates <- rv$run$model$covariates %>%
    add_row(column = u_sel, type = "categorical", name = u_sel)

  rv$run$tables$pmxploitab <- rv$run$tables$pmxploitab %>%
    mutate_at(u_sel, as.factor)

  for(cn in u_sel){
    lvls <- levels(rv$run$tables$pmxploitab[[cn]])
    rv$run$model$categorical_covariates_levels[[cn]] <- set_names(lvls, lvls)
  }
})

observeEvent(input$add_to_continuous_cov, {
  u_sel <- req(input$unknown_metadata)

  rv$run$model$covariates <- rv$run$model$covariates %>%
    add_row(column = u_sel, type = "continuous", name = u_sel)
})

observeEvent(input$add_to_parameters, {
  u_sel <- req(input$unknown_metadata)

  rv$run$model$parameters <- rv$run$model$parameters %>%
    add_row(id = u_sel, type = "individual", name = u_sel, column = u_sel)
})


observeEvent(input$add_to_idv, {
  u_sel <- req(input$unknown_metadata)

  rv$run$model$independent_variables <- rv$run$model$independent_variables %>% add_row(column = u_sel,
                                                                 name = u_sel,
                                                                 unit = NA_character_)
})
