rv_vpc <- reactiveValues(future = NULL,
                         progress_bar = NULL,
                         n_simulations = NULL,
                         control_stream = NULL,
                         results = NULL)

vpc_output_file <- str_c(app_temp_vpc_directory, "run", "OUTPUT", sep = "/")

output$nm_exe <- renderUI({
  nm_exe <- env_nm_exe
  textInput("nm_exe", "NONMEM executable", nm_exe)
})

output$nmcheck_exe <- renderUI({
  nmcheck_exe <- env_nmcheck_exe
  textInput("nmcheck_exe", "NM-TRAN check executable", nmcheck_exe)
})

output$vpc_cmt <- renderUI({
  # keep currently selected compartment
  current_selection <- isolate(input$cmt)

  cmt_choices <- req(run_dv_cmts())

  selectInput("vpc_cmt", "Selection", choices = cmt_choices, selected = current_selection)
})

strat_columns <- reactive({
  run <- req(rv$run)

  run_cat_covs <- run$model$covariates %>% filter(type == "categorical") %>% pull(column)

  c("None", run_cat_covs)
})

output$vpc_strat1 <- renderUI({
  selection <- if(!is.null(input$vpc_strat1)) input$vpc_strat1 else NULL
  selectInput("vpc_strat1", "Stratification column 1", choices = strat_columns(), selected = selection)
})

output$vpc_strat2 <- renderUI({
  selection <- if(!is.null(input$vpc_strat2)) input$vpc_strat2 else NULL
  selectInput("vpc_strat2", "Stratification column 2", choices = strat_columns(), selected = selection)
})

output$vpc_strat1_levels <- renderUI({
  s1 <- req(input$vpc_strat1)
  req(s1 != "None")
  run <- req(rv$run)

  # s1_levels <- run$model$categorical_covariates_levels[[s1]]
  s1_levels <- levels(run$tables$pmxploitab[[s1]])

  checkboxGroupInput("vpc_strat1_levels", label = s1, choices = s1_levels, selected = s1_levels, inline = TRUE)
})

output$vpc_strat2_levels <- renderUI({
  s2 <- req(input$vpc_strat2)
  req(s2 != "None")
  run <- req(rv$run)

  # s2_levels <- run$model$categorical_covariates_levels[[s2]]
  s2_levels <- levels(run$tables$pmxploitab[[s2]])

  checkboxGroupInput("vpc_strat2_levels", label = s2, choices = s2_levels, selected = s2_levels, inline = TRUE)
})

observeEvent(input$generate_control_stream, {
  run <- req(rv$run)

  safe_vpc_cs <- safely(nm_simulation)
  vpc_cs <- safe_vpc_cs(run,
                        n_simulations = req(input$vpc_n_simulations),
                        seed = req(input$vpc_seed))

  if(!is.null(vpc_cs$error)){
    session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                  title = "Error generating control stream",
                                                                                  description = vpc_cs$error$message)))
  } else {
    showModal(modalDialog(title = "VPC Control stream",
                          tags$strong("Generated control stream"),
                          aceEditor("vpc_control_stream_code", debounce = 100, height = "600px", value = vpc_cs$result),
                          tags$strong("Check that the control stream was correctly generated"),
                          size = "l",
                          easyClose = TRUE,
                          footer = list(modalButton("Close"),
                                        actionButton("vpc_run", "Run NONMEM"))))
  }
})


observeEvent(input$vpc_run, {
  nm_exe <- input$nm_exe
  nm_check_exe <- input$nmcheck_exe

  if(length(system(str_c("command -v ", nm_exe), intern = TRUE)) == 0){
    session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                  title = "Error",
                                                                                  description = "NONMEM command not found")))
    req(FALSE)
  }

  can_check <- (length(system(str_c("command -v ", nm_check_exe), intern = TRUE)) != 0)

  if(!can_check){
    session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                  title = "Error",
                                                                                  description = "NM-TRAN check command not found")))
    req(FALSE)
  }

  shinyjs::toggleState("vpc_run", condition = FALSE)

  run <- req(rv$run)

  cs_code <- req(input$vpc_control_stream_code)

  safe_parse <- safely(parse_nm_control_stream)

  cs_data <- safe_parse(content = cs_code)

  if(!is.null(cs_data$error)){
    session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                  title = "Error",
                                                                                  description = "PMXploit encountered an error while parsing the control stream.")))
    return(NULL)
  }

  cs_data <- cs_data$result

  rv_vpc$progress_bar <- Progress$new(min = 0, max = input$vpc_n_simulations)

  rv_vpc$progress_bar$set(message = "VPC computation in progress",
                          detail = sprintf("Starting NONMEM at %s", app_temp_vpc_directory))

  if(dir.exists(app_temp_vpc_directory))
    unlink(app_temp_vpc_directory, recursive = TRUE)

  dir.create(app_temp_vpc_directory, recursive = TRUE)


  #option 1
  # file.copy(str_c(app_temp_directory, run$info$dataset_file, sep = "/"),
  #           str_c(app_temp_vpc_directory, run$info$dataset_file, sep = "/"))

  # option 2
  write_csv(select(run$tables$dataset, one_of(cs_data$input$column)),
            path = str_c(app_temp_vpc_directory, run$info$dataset_file, sep = "/"),
            na = ".",
            col_names = FALSE)


  vpc_cs_file <- str_c(app_temp_vpc_directory, "vpc.ctl", sep = "/")
  write_lines(cs_code, vpc_cs_file)

  # Run VPC using another core
  plan(multiprocess)
  n_nodes <- 1L

  rv_vpc$future <-  future({
    check <- nm_check(vpc_cs_file,
                      nmcheck_exe = nm_check_exe,
                      call_template = env_nmcheck_call)

    if(check$error){
      return(check$message)
    }

    # Move original files: dataset and extra files (custom subroutines)
    extra_files <- c(run$control_stream$subroutine$FILES, run$control_stream$extra_files)

    files_to_copy <- c(run$control_stream$dataset_file, extra_files)

    if(length(files_to_copy) > 0){
      if(file.info(run$info$path)$isdir){
        file.copy(str_c(run$info$path, files_to_copy, sep = "/"),
                  app_temp_vpc_directory)
      } else {
        archive_files <- untar(run$info$path, list = TRUE)

        archive_files <- str_subset(archive_files, fixed(files_to_copy))

        untar(run$info$path, files = archive_files,
              exdir = app_temp_vpc_directory)
      }
    }

    pmxecute::nm_exec(control_file = str_c(app_temp_vpc_directory, "vpc.ctl", sep = "/"),
                   run_directory = str_c(app_temp_vpc_directory, "run", sep = "/"),
                   nonmem_exe = nm_exe,
                   call_template = env_nm_call,
                   archive = FALSE,
                   cleanup = TRUE)

    return("")
  })

  rv_vpc$n_simulations <- input$vpc_n_simulations
  rv_vpc$control_stream <- cs_data

  removeModal()
})

observe({

  shinyjs::toggleState("vpc_run",
                       condition = (is.null(rv_vpc$future) || resolved(rv_vpc$future)) &&
                         (!is.null(input$vpc_control_stream_code) && str_trim(input$vpc_control_stream_code) != ""))

  if(!is.null(rv_vpc$future)){

    if (file.exists(vpc_output_file)){

      lines <- read_lines(vpc_output_file)

      pattern <- ".+SUBPROBLEM NO\\.:\\s*([0-9]+)$"
      subpb_lines <- lines %>% str_subset(pattern)

      if(length(subpb_lines) > 0){
        last_sim_done <- last(subpb_lines) %>% str_match(pattern) %>% .[,2] %>% as.integer()

        rv_vpc$progress_bar$set(value = last_sim_done, detail = sprintf("%s/%s", last_sim_done, rv_vpc$n_simulations))
      }

    }

    if(resolved(rv_vpc$future)){
      rv_vpc$progress_bar$close()
      nmtran_message <- value(rv_vpc$future)
      rv_vpc$future <- NULL

      if(nmtran_message != ""){ # an error occured with NM-TRAN check
        session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                      title = "Error",
                                                                                      description = "NM-TRAN check error:", detail = nmtran_message)))
        return(NULL)
      }

      tab_path <- str_c(app_temp_vpc_directory, "run", "simtab", sep = "/")

      if(!file.exists(tab_path)){ # simulations failed
        err_message <- ""

        xml_path <- str_c(app_temp_vpc_directory, "run", "vpc.xml", sep = "/")

        if(file.exists(xml_path)){
          xml_file <- xml2::read_xml(xml_path)

          sim_info <- xml2::xml_find_all(xml_file, "//nm:simulation_information") %>% last

          if(length(sim_info) > 0){
            err_message <- xml2::xml_text(sim_info)
          }
        }

        if(err_message == ""){
          err_message <- str_c("No output table was generated by NONMEM.\nLook at the running folder to analyze what happened: ", app_temp_vpc_directory)
        }

        session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                      title = "Simulation failed",
                                                                                      detail = str_trim(err_message))))
        return(NULL)
      }

      run <- req(rv$run)

      cs_simtab <- rv_vpc$control_stream$tables %>% filter(file == "simtab")

      simtab <- read_csv(tab_path,
                         col_names = unlist(cs_simtab$columns),
                         col_types = cols(.default = col_double())) %>%
        mutate(REP = ntile(row_number(), rv_vpc$n_simulations))

      common_cols <- intersect(colnames(simtab), colnames(run$tables$dataset))

      obs_df <- run$tables$dataset %>%
        mutate_if(is.factor, funs(as.numeric(as.character(.))))

      if(nrow(simtab) != (nrow(obs_df) * (rv_vpc$n_simulations))){
        session$sendCustomMessage(type = "popup_msg", message = toJSON(list(type = "error",
                                                                                      title = "Something went wrong !")))
        return(NULL)
      }

      temp_obs_df <- obs_df %>%
        select(-one_of(common_cols))

      if("PRED" %in% colnames(run$tables$pmxploitab)){
        temp_obs_df <- temp_obs_df %>%
          bind_cols(PRED = run$tables$pmxploitab$PRED)
      }

      sim_df <- simtab %>%
        nest(-REP, .key = "sim") %>%
        mutate(obs = list(temp_obs_df)) %>%
        unnest() %>%
        select(REP, everything())

      rv_vpc$results <- list(n_simulations = rv_vpc$n_simulations,
                             obs = obs_df,
                             sim = sim_df)
    }

    invalidateLater(1000)
  }
})

vpc_theme_pmxploit <- new_vpc_theme(update = list(
  obs_color = "black",
  obs_ci_color = "black",
  obs_ci_fill = "#80808033",
  obs_shape = 19,
  sim_median_fill = "#5eb5ffff",
  sim_median_alpha = 0.7,
  sim_pi_fill = "#ea5d5dfc", # 56df81fc
  sim_pi_size = 2,
  sim_pi_alpha = 0.3
))

run_vpcdb <- reactive({
  vpc_data <- req(rv_vpc$results)

  vpc_cmt <- req(input$vpc_cmt)

  strat_cols <- setdiff(c(input$vpc_strat1, input$vpc_strat2), "None")

  vpc_filters <- NULL

  if(length(strat_cols) > 0){
    req(!is.null(input$vpc_strat1_levels) || !is.null(input$vpc_strat2_levels))

    if(input$vpc_strat1 != "None"){
      s1_lvls <- req(input$vpc_strat1_levels)

      s1_form <- str_c(input$vpc_strat1, " %in% c(", str_c(s1_lvls, collapse = ", "), ")")

      vpc_filters <- parse_quo(s1_form, env = caller_env())
    }

    if(input$vpc_strat2 != "None"){
      s2_lvls <- req(input$vpc_strat2_levels)

      s2_form <- str_c(input$vpc_strat2, " %in% c(", str_c(s2_lvls, collapse = ", "), ")")

      vpc_filters <- c(vpc_filters, parse_quo(s2_form, env = caller_env()))
    }

  } else {
    strat_cols <- NULL
  }

  sim_df <- vpc_data$sim %>% filter(CMT == vpc_cmt) %>% filter(UQS(vpc_filters))
  obs_df <- vpc_data$obs %>% filter(CMT == vpc_cmt) %>% filter(UQS(vpc_filters))

  if(length(strat_cols) > 0){
    lvls <- rv$run$model$categorical_covariates_levels[strat_cols]

    for(i in seq_along(lvls)){
      col_name <- names(lvls)[i]
      col_values <- lvls[[i]]
      col_levels <- names(lvls[[i]])

      sim_df <- sim_df %>% mutate(!!col_name := plyr::mapvalues(!!as.name(col_name), from = col_values, col_levels))
      obs_df <- obs_df %>% mutate(!!col_name := plyr::mapvalues(!!as.name(col_name), from = col_values, col_levels))
    }
  }


  is_time <- is.hms(obs_df$TIME)

  # prevent TIME column from being in "time" format before calling vpc function
  if (is_time) {
    # Fix TIME column with dataset times
    sim_df <- sim_df %>%
      select(-TIME) %>%
      left_join(select(obs_df, ID, TIME), by = "ID") %>%
      select(REP, ID, TIME, everything())

    # Convert TIME to numeric
    sim_df <- sim_df %>% mutate(TIME = as.numeric(TIME))
    obs_df <- obs_df %>% mutate(TIME = as.numeric(TIME))
  }


  vpcdb <- vpc(sim = sim_df,
               obs = obs_df,
               stratify = strat_cols,
               bins = input$vpc_binning,
               n_bins = ifelse(is.na(input$vpc_n_bins), "auto", input$vpc_n_bins),
               pi = input$vpc_pi,
               ci = input$vpc_ci,
               bin_mid = input$vpc_bin_mid,
               pred_corr = input$vpc_correction,
               lloq = if(!is.na(input$vpc_lloq)) input$vpc_lloq else NULL,
               vpcdb = TRUE)

  # Convert back TIME
  if(is_time){
    vpcdb$bins <- as.hms(vpcdb$bins)
    vpcdb$sim$idv <- as.hms( vpcdb$sim$idv)
    vpcdb$obs$idv <- as.hms( vpcdb$obs$idv)
    vpcdb$vpc_dat <- vpcdb$vpc_dat %>% mutate_at(vars(starts_with("bin_")), as.hms)
    vpcdb$aggr_obs <- vpcdb$aggr_obs %>% mutate_at(vars(starts_with("bin_")), as.hms)
  }
  vpcdb$labeller <- label_both

  vpcdb$n_simulations <- vpc_data$n_simulations

  vpcdb
})

run_vpc_plot <- reactive({

  vpcdb <- req(run_vpcdb())

  g <- plot_vpc(vpcdb,
                show = list(obs_dv = input$vpc_obs_dv,
                            obs_ci = input$vpc_obs_ci,
                            pi = input$vpc_show_pi,
                            pi_as_area = input$vpc_pi_as_area,
                            pi_ci = input$vpc_pi_ci,
                            obs_median = input$vpc_obs_median,
                            sim_median = input$vpc_sim_median,
                            sim_median_ci = input$vpc_sim_median_ci),
                log_x = input$vpc_x_scale == "log",
                log_y = input$vpc_y_scale == "log",
                smooth = input$vpc_smooth,
                vpc_theme = vpc_theme_pmxploit)

  if(!("FacetNull" %in% class(g$facet))){
    g$facet$params$free <- switch(input$vpc_facet_scale,
                                  "free_x" = { list(x = TRUE, y = FALSE) },
                                  "free_y" = { list(x = FALSE, y = TRUE) },
                                  "free" = { list(x = TRUE, y = TRUE) },
                                  { list(x = FALSE, y = FALSE) })
  }

  g <- g + labs(title = "Visual Predictive Checks",
                subtitle = sprintf("Generated from %s simulations", vpcdb$n_simulations))

  g
})

vpc_sim_table <- reactive({
  req(rv_vpc$results$sim)
})

vpc_obs_table <- reactive({
  vpcdb <- req(run_vpcdb())

  vpcdb$obs
})

vpc_db_table <- reactive({
  vpcdb <- req(run_vpcdb())

  vpcdb$vpc_dat
})

