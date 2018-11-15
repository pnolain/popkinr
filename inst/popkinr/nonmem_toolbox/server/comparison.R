options(shiny.deprecation.messages = FALSE)
watcher_file <- paste(app_temp_comparison_directory, str_c("watcher_file_", stringi::stri_rand_strings(1, 5), ".txt"), sep = "/")

rv_comp <- reactiveValues(
  do_compute = 0,
  parallel_f = NULL,
  parallel_start = NULL,
  comparison_data = NULL,
  progress_bar = NULL,
  progression = 0,
  n_runs = 0,
  diffr_run_index = NULL,
  diffr_value = NULL,
  filters_n_est = NULL,
  filters_status = NULL,
  filters_method = NULL
)

load_run_temp <- function(path, i, write_progression = TRUE) {
  name <- basename(path)

  is_directory <- file.info(path)$isdir

  safe_load <- safely(load_nm_run)

  if(is_directory){
    temp_run <- safe_load(path = path,
                          load_tables = FALSE,
                          read_initial_values = FALSE,
                          verbose = FALSE)
  } else {
    temp_run <- safe_load(path = path,
                          temp_directory = paste(app_temp_comparison_directory, i, sep = "/"),
                          load_tables = FALSE,
                          read_initial_values = FALSE,
                          verbose = FALSE)
  }

  if(write_progression)
    write_lines(path, path = watcher_file, append = TRUE)

  if (!is.null(temp_run$error)) {
    shinytoastr::toastr_error(
      title = name,
      message = temp_run$error$message,
      timeOut = 10000
    )

    return(NULL)
  }

  temp_run$result
}

safe_value <- safely(future::value)


observe({
  has_cp_data <- (!is.null(rv_comp$comparison_data) && nrow(rv_comp$comparison_data) > 0)
  shinyjs::toggle(id = "comparison-content", condition =  has_cp_data)
  shinyjs::toggleState("clear_comparison_table", condition = has_cp_data)
  shinyjs::toggleState("select_runs_to_compare", condition = is.null(rv_comp$parallel_f))
  shinyjs::toggleState("export_comparison_table", condition = !is.null(selected_estimations()))
})

observe({
  isolate({

    if(!is.null(rv_comp$parallel_f)){
      print("Computing......")

      if(resolved(rv_comp$parallel_f)){
        print("Computing done, returning results...")

        safe_data <- safe_value(rv_comp$parallel_f)

        if(!is.null(safe_data$error)){
          if(input$parallel_comparison){
            err_msg <- "Error retrieving results. You may try to disable parallel processing before loading runs."
            toastr_error(message = err_msg, title = "Error", timeOut = 0, position = "bottom-right", closeButton = TRUE)
          } else {
            err_msg <- "Error retrieving results. Something went wrong."
            toastr_error(message = err_msg, title = "Error", timeOut = 0, position = "bottom-right", closeButton = TRUE)
          }

        } else {
          new_data <- safe_data$result$data %>%
            filter(!map_lgl(run, is.null))

          rv_comp$comparison_data <- rv_comp$comparison_data %>%
            bind_rows(new_data)

          # refresh comparison
          rv_comp$do_compute <- rv_comp$do_compute + 1

          diff_mins <- difftime(lubridate::now(), rv_comp$parallel_start, units = "mins")
          diff_secs <- difftime(lubridate::now(), rv_comp$parallel_start, units = "secs")

          toastr_success(message = sprintf("Runs loaded in %s",
                                           ifelse(diff_mins < 1,
                                                  str_c(round(diff_secs, 2), " seconds"),
                                                  str_c(round(diff_mins, 2), " minutes"))),
                         title = "Done", timeOut = 10000, position = "bottom-right", closeButton = TRUE)

        }
        rv_comp$parallel_f <- NULL
        rv_comp$progress_bar$close()
      } else {
        n_lines <- ifelse(file.exists(watcher_file), length(read_lines(watcher_file)), 0)

        delta <- n_lines - rv_comp$progression

        rv_comp$progress_bar$inc(amount = delta, detail = sprintf("Runs: %s/%s",
                                                                  rv_comp$progress_bar$getValue(),
                                                                  rv_comp$n_runs))

        print(paste("N delta:", delta))
        print(paste("N done:", n_lines))

        rv_comp$progression <- n_lines
      }
    }

  })

  invalidateLater(200)
})


# Comparison tab----

observeEvent(input$select_runs_to_compare, {
  comparison_browser()$initialize_ui(force = TRUE)

  showModal(
    modalDialog(
      title = "Select runs to be compared to the current run",
      size = "l",
      popkinr::serverBrowserUI("comparison_browser"),
      checkboxInput("parallel_comparison", "Parallel processing"),
      easyClose = TRUE,
      footer = list(modalButton("Close"),
                    actionButton("comp_load_folder", "Load all runs from folder"),
                    actionButton("comp_load_run", "Load selection"))
    )
  )
})


comparison_results <- reactive({
  #req(rv_comp$do_compute > 0)

  #isolate({

  run <- req(rv$run)
  req(!is.null(rv_comp$comparison_data) && nrow(rv_comp$comparison_data) > 0)

  cp_runs <- rv_comp$comparison_data$run %>% compact()

  withProgress({
    comp_table <-
      compare_runs(c(list(run), cp_runs))

    setProgress(value = 1, detail = "Done !")
  }, message = "Computing comparison...", value = 0.5)

  comp_table %>%
    mutate(INDEX_RUN = row_number() - 1L) %>%
    select(INDEX_RUN, everything())

  #})
})

estimation_results <- reactive({

  cp_res <- req(comparison_results())

  run_indexes <- cp_res$INDEX_RUN

  est_res <- cp_res %>%
    unnest(ESTIMATION, .drop = FALSE) %>%
    mutate(STATUS = map_chr(SUMMARY, ~.$STATUS)) %>%
    group_by(INDEX_RUN) %>%
    mutate(INDEX_EST = row_number()) %>%
    ungroup() %>%
    select(INDEX_RUN, INDEX_EST, RUN_ID, everything())

  no_est_runs <- cp_res %>%
    filter(!(INDEX_RUN %in% est_res$INDEX_RUN))

  if(nrow(no_est_runs) > 0)
    toastr_error(message = str_c("No estimation information for run(s): ", str_c(no_est_runs$RUN_ID, collapse = ", ")), timeOut = 0)

  est_res
})

output$comparison_estimation_n <- renderUI({

  all_runs <- req(comparison_results())

  est_seq <- all_runs %>% summarise(n_estimations = max(map_int(ESTIMATION, nrow))) %>% pull() %>% seq_len()

  e_sel <- isolate(if(!is.null(rv_comp$filters_n_est)) intersect(est_seq, rv_comp$filters_n_est) else est_seq)

  checkboxGroupInput("comparison_estimation_n", "Estimation number",
                     choices = est_seq, selected = e_sel, inline = TRUE)
})

output$comparison_estimation_method <- renderUI({

  all_runs <- req(comparison_results())

  e_methods <- all_runs %>% select(ESTIMATION) %>% unnest() %>% unnest(SUMMARY) %>% pull(METHOD) %>% unique()

  e_sel <- isolate(if(!is.null(rv_comp$filters_method)) intersect(e_methods, rv_comp$filters_method) else e_methods)

  checkboxGroupInput("comparison_estimation_method", "Estimation method",
                     choices = e_methods, selected = e_sel, inline = TRUE)
})

output$comparison_estimation_status <- renderUI({

  all_runs <- req(comparison_results())

  e_status <- all_runs %>% select(ESTIMATION) %>% unnest() %>% unnest(SUMMARY) %>% pull(STATUS) %>% unique()

  e_sel <- isolate(if(!is.null(rv_comp$filters_status)) intersect(e_status, rv_comp$filters_status) else e_status)

  checkboxGroupInput("comparison_estimation_status", "Termination status",
                     choices = e_status, selected = e_sel, inline = TRUE)
})

observeEvent(input$comparison_estimation_n, {
  rv_comp$filters_n_est <- input$comparison_estimation_n
})
observeEvent(input$comparison_estimation_method, {
  rv_comp$filters_method <- input$comparison_estimation_method
})
observeEvent(input$comparison_estimation_status, {
  rv_comp$filters_status <- input$comparison_estimation_status
})


selected_estimations <- reactive({
  est_n <- as.integer(req(input$comparison_estimation_n))
  est_meth <- req(input$comparison_estimation_method)
  est_stat <- req(input$comparison_estimation_status)

  est_res <- req(estimation_results())

  est_res %>%
    filter(INDEX_EST %in% est_n) %>%
    filter(map_lgl(SUMMARY, ~ .$STATUS %in% est_stat))%>%
    filter(map_lgl(SUMMARY, ~ .$METHOD %in% est_meth))
})

not_failed_estimations <- reactive({
  cp_ests <- req(selected_estimations())

  cp_ests %>%
    filter(STATUS != "Failed")
})


comparison_termination_summary_table <- reactive({
  run <- req(rv$run)
  cp_details <- req(input$comparison_details)

  cp_table <- selected_estimations()

  cp_table %>%
    mutate(METHOD = map_chr(SUMMARY, ~ .$METHOD)) %>%
    count(METHOD, STATUS)
})

output$comparison_status_summary <- renderDataTable({
  df <- req(comparison_termination_summary_table())

  datatable(df) %>%
    formatStyle("STATUS",
                target = "row",
                backgroundColor = styleEqual(levels = c("Failed", "Terminated", "Successful"),
                                             values = c("#F96969", "#F9B269", "#89D3FF")))
})

comparison_summary_table <- reactive({
  run <- req(rv$run)
  cp_details <- req(input$comparison_details)

  cp_table <- selected_estimations()

  req(nrow(cp_table) > 0)

  cp_details <- intersect(cp_details, colnames(cp_table))

  df_ok <- df_failed <- NULL

  df_ok <- cp_table %>%
    filter(STATUS != "Failed")

  if(nrow(df_ok) > 0){
    df_ok <- df_ok %>%
      select(INDEX_RUN, INDEX_EST, RUN_ID, one_of(cp_details)) %>%
      # mutate_if(is.list, function(x) {
      #   if(is.null(x[[1]])) return(list(tibble(NA)))
      #   x
      # }) %>%
      unnest()
  }


  df_failed <- cp_table %>%
    filter(STATUS == "Failed")

  if(nrow(df_failed) > 0){
    df_failed <- df_failed %>%
      select(INDEX_RUN, INDEX_EST, RUN_ID, STATUS, one_of(intersect(c("SUMMARY", "INFO", "FILES"), cp_details)))

    if(any(map_lgl(df_failed, is.list)))
      df_failed <- unnest(df_failed)
  }


  is_not_all_null <- function(x) !all(is.null(x))

  df <- bind_rows(df_ok, df_failed)%>%
    select(INDEX_RUN, INDEX_EST, everything()) %>%
    select_if(function(x) all(!map_lgl(x, is.null)))

  if("STATUS1" %in% colnames(df)) # clean up duplicated column
    df <- df %>% select(-STATUS1)

  df <- df %>%
    arrange(INDEX_RUN, INDEX_EST)

  df
})

run_comparison_table <- reactive({

  df <- req(comparison_summary_table()) %>%
    mutate(ACTIONS = ifelse(
      INDEX_RUN != 0,
      sprintf(
        str_c("<a href=\"#/\" onclick=\"open_run('%s');\">Open</a> - ",
              "<a href=\"#/\" onclick=\"remove_run('%s');\">Remove</a><br/>",
              "<a href=\"#/\" onclick=\"compare_control_stream('%s');\">Compare control stream</a>"),
        INDEX_RUN, INDEX_RUN, INDEX_RUN
      ),
      NA)) %>%
    mutate(RUN_EST = str_c(INDEX_RUN, ".", INDEX_EST)) %>%
    select(-INDEX_RUN, -INDEX_EST) %>%
    select(RUN_EST, RUN_ID, ACTIONS, everything()) %>%
    mutate_if(hms::is.hms, ~ map_chr(., function(x){
      if(is.na(x)) return("")

      p <- lubridate::as.period(x)

      sprintf("%02d d %02d h %02d min", p@day, p@hour, p@minute)
    }))

  if ("Filepath" %in% colnames(df)) {
    df <- df %>%
      mutate(Filepath = basename(Filepath)) %>%
      rename(Path = Filepath)
  }

  if(all(c("INFO", "SUMMARY") %in% input$comparison_details)){
    df <- df %>%
      rename(RUN_DURATION = DURATION,
             EST_DURATION = DURATION1)
  }

  statuses <- selected_estimations() %>%
    select(INDEX_RUN, INDEX_EST, RUN_ID, SUMMARY) %>%
    unnest() %>%
    select(INDEX_RUN, INDEX_EST, RUN_ID, STATUS) %>%
    mutate(RUN_EST = str_c(INDEX_RUN, ".", INDEX_EST)) %>%
    mutate(COLOR = plyr::mapvalues(
      STATUS,
      c("Failed", "Terminated", "Successful"),
      c("#F96969", "#F9B269", "#89D3FF"),
      warn_missing = FALSE
    ))

  current_run_est_index <- str_subset(statuses$RUN_EST, "^0")

  # remove empty columns (e.g. EVALS or SIGDIGITS which depend on estimation methods)
  df <- df %>%
    select_if(~ !all(is.na(.)))

  dt <- datatable(
    df,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 50,
      autoWidth = TRUE,
      scrollX = TRUE,
      dom = 'ip'
    )
  ) %>%
    formatStyle(
      "RUN_EST",
      target = "row",
      backgroundColor = styleEqual(levels = statuses$RUN_EST,
                                   values = statuses$COLOR)
    )

  if(length(current_run_est_index) > 0){
    dt <- dt %>%
      formatStyle("RUN_EST",
                  target = "row",
                  fontWeight = styleEqual(levels = current_run_est_index,
                                          values = rep("bold", length(current_run_est_index))))
  }

  rse_colnames <- colnames(df) %>% str_subset("^RSE\\(")

  if (length(rse_colnames) > 0)
    dt <- dt %>% formatPercentage(rse_colnames, 2)

  dt
})

comparison_statistics <- reactive({
  run <- req(rv$run)

  cp_sel <- req(input$comparison_stats_table_detail)
  cp_table <- not_failed_estimations()

  req(nrow(cp_table) > 0)

  cp_details <- intersect(cp_sel, colnames(cp_table))

  df <- cp_table %>%
    select(one_of(cp_details)) %>%
    unnest() %>%
    gather(parameter, value)

  if(!input$comparison_stats_table_rse){
    df <- df %>% filter(!str_detect(parameter, "^RSE\\("))
  }

  quantiles <- c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975)

  q_dots <- map(quantiles, function(x) {
    ~ quantile(value, x, na.rm = TRUE)
  })
  names(q_dots) <- scales::percent(quantiles)

  dots <- c(
    n = ~ length(value),
    n_distinct = ~ n_distinct(value, na.rm = TRUE),
    n_missing = ~ sum(is.na((value))),
    mean = ~ mean(value, na.rm = TRUE),
    median = ~ median(value, na.rm = TRUE),
    q_dots,
    sd = ~ sd(value, na.rm = TRUE),
    min = ~ min(value, na.rm = TRUE),
    max = ~ max(value, na.rm = TRUE)
  ) %>% map(rlang::as_quosure)

  df <- df %>%
    group_by(parameter) %>%
    summarise(!!!dots)
})

output$comparison_plots_parameters <- renderUI({
  run <- req(rv$run)

  cr <- req(not_failed_estimations()[[input$comparison_plots_details]])

  p_choices <- cr %>% map(colnames) %>% unlist() %>% unique %>% sort %>%
    .[!str_detect(., "^RSE\\(")]

  checkboxGroupInput("comparison_plots_parameters", "Selection",
               choices = p_choices,
               selected = p_choices, inline = TRUE)
})

run_comparison_param_plot <- reactive({
  run <- req(rv$run)

  cp_type <- isolate(req(input$comparison_plots_details))
  cp_selection <- req(input$comparison_plots_parameters)

  cp_table <- not_failed_estimations()

  req(nrow(cp_table) > 0)

  df <- cp_table %>%
    select(RUN_ID, one_of(cp_type)) %>%
    unnest() %>%
    select(RUN_ID, one_of(cp_selection)) %>%
    gather(param, value, -RUN_ID)

  list_elem <- switch(cp_type,
                      "THETA" = "thetas",
                      "OMEGA" = "omega",
                      "SIGMA" = "sigma")

  run_p <- switch(cp_type,
                  "THETA" = {
                    map_df(run$estimations, "thetas", .id = "EST") %>%
                      rename(param = name)
                  },
                  "OMEGA" = {
                    map_df(run$estimations, "omega", .id = "EST") %>%
                      unite(param, eta1, eta2, sep = ":")
                  },
                  "SIGMA" = {
                    map_df(run$estimations, "sigma", .id = "EST") %>%
                      unite(param, epsilon1, epsilon2, sep = ":")
                  })


  sum_df <- df %>%
    group_by(param) %>%
    summarise(p_low = quantile(value, probs = 0.025, na.rm = TRUE),
              p_high = quantile(value, probs = 0.975, na.rm = TRUE),
              median = median(value, na.rm = TRUE)) %>%
    gather(stat, value, p_low, p_high, median) %>%
    mutate(stat = ifelse(stat == "median", "Median", "95% confidence interval"))

  if (input$comparison_distribution_plot == "histogram") {
    ggplot(df, aes(x = value)) +
      geom_histogram()+
      geom_vline(data = sum_df, aes(xintercept = value, linetype = stat), inherit.aes = FALSE)+
      geom_vline(data = run_p, aes(xintercept = estimate, color = EST), inherit.aes = FALSE)+
      scale_color_discrete(name = "Run estimate")+
      scale_linetype_manual(limits = c("95% confidence interval", "Median"), values = c("dashed", "solid"),
                            name = "Statistic")+
      facet_wrap( ~ param, scales = "free") +
      labs(x = "Parameter estimate")
  } else if (input$comparison_distribution_plot == "density") {
    ggplot(df, aes(x = value)) +
      geom_density()+
      geom_vline(data = sum_df, aes(xintercept = value, linetype = stat), inherit.aes = FALSE)+
      geom_vline(data = run_p, aes(xintercept = estimate, color = EST), inherit.aes = FALSE)+
      scale_color_discrete(name = "Run estimate")+
      scale_linetype_manual(limits = c("95% confidence interval", "Median"), values = c("dashed", "solid"),
                            name = "Statistic")+
      facet_wrap( ~ param, scales = "free")+
      labs(x = "Parameter estimate")
  } else {
    ggplot(df, aes(x = param, y = value))+
      geom_boxplot()+
      geom_hline(data = sum_df, aes(yintercept = value, linetype = stat), inherit.aes = FALSE)+
      geom_hline(data = run_p, aes(yintercept = estimate, color = EST), inherit.aes = FALSE)+
      scale_color_discrete(name = "Run estimate")+
      scale_linetype_manual(limits = c("95% confidence interval", "Median"), values = c("dashed", "solid"),
                            name = "Statistic")+
      facet_wrap( ~ param, scales = "free")+
      labs(x = "Parameter estimate")
  }
})

run_comparison_ofv_plot <- reactive({
  run <- req(rv$run)
  res <- req(selected_estimations())

  df <- res %>%
    mutate(RUN_NAME = map_chr(INFO, "RUN_NAME")) %>%
    select(RUN_NAME, INDEX_EST, SUMMARY) %>%
    mutate(METHOD = map_chr(SUMMARY, "METHOD"),
           OBJ = map_dbl(SUMMARY, "OBJ")) %>%
    select(-SUMMARY) %>%
    arrange(OBJ) %>%
    mutate(ord = as.factor(row_number()),
           label = str_c(RUN_NAME, " (", INDEX_EST, ")"))


  g <- ggplot(df, aes(x = ord, ymin = min(OBJ, na.rm = TRUE), ymax = OBJ, y = OBJ))+
    geom_pointrange()+
    # coord_flip()+
    labs(x = "RUNS", y = "Objective Function")+
    scale_x_discrete(labels = df$label)

  # if(length(unique(df$METHOD)) > 1)
  #   g <- g + facet_wrap(~METHOD, scales = "free")

  g
})

output$export_comparison_table <- downloadHandler(
  filename = function() {
    run <- req(rv$run)
    sprintf("%s.comparison.csv", run$info$run_id)
  },
  content = function(file) {
    df <- req(comparison_summary_table())

    write_csv(df, file, na = ".")
  }
)

observeEvent(input$clear_comparison_table, {
  req(!is.null(rv_comp$comparison_data) && nrow(rv_comp$comparison_data) > 0)

  rv_comp$comparison_data <- rv_comp$comparison_data %>% slice(0)
})

observeEvent(input$click_remove_run_from_list, {
  js <- jsonlite::fromJSON(input$click_remove_run_from_list)

  cp_res <- req(comparison_results())#req(rv_comp$comparison_data)

  r_index <- as.integer(js$run_index)

  rv_comp$comparison_data <- rv_comp$comparison_data %>%
    slice(-r_index)
})

observeEvent(input$click_compare_control_stream, {
  js <- jsonlite::fromJSON(input$click_compare_control_stream)

  cp_res <- req(comparison_results())
  r_index <- as.integer(js$run_index)

  # if (is.null(rv_comp$diffr_run_index)) {#} || r_index != rv_comp$diffr_run_index) {

    cp_run <- rv_comp$comparison_data$run[[r_index]]

    cs1 <- rv$run$control_stream$code
    cs2 <- cp_run$control_stream$code

    if (identical(cs1, cs2)) {
      session$sendCustomMessage(type = "popup_msg",
                                message = jsonlite::toJSON(
                                  list(
                                    type = "info",
                                    title = "Info",
                                    description = sprintf(
                                      "Control streams of runs %s and %s are identical.",
                                      rv$run$info$run_name,
                                      cp_run$info$run_name
                                    )
                                  )
                                ))
      return(NULL)
    } else {
      file1 = tempfile()
      writeLines(cs1, con = file1)
      file2 = tempfile()
      writeLines(cs2, con = file2)

      rv_comp$diffr_value <- diffr::diffr(
        file1,
        file2,
        before = str_c("Current run: ", rv$run$info$run_name),
        after  = str_c("Comparison run: ", cp_run$info$run_name)
      )
    }

  #   rv_comp$diffr_run_index <- r_index
  #
  # }

  showModal(
    modalDialog(
      title = "Differences between 2 control streams",
      diffr::diffrOutput("control_stream_diffr", height = "100%"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
})

observeEvent(input$comp_load_run, {
  removeModal()

  start_comparison()
})

observeEvent(input$comp_load_folder, {
  removeModal()

  start_comparison(folder = TRUE)
})

is_nm_run <- function(dir_path){
  map_lgl(dir_path, function(x){
    dir_files <- dir(x, ignore.case = TRUE)

    selection_is_nm_run <- all(c("xml", "ext") %in% tools::file_ext(dir_files))
  })
}

start_comparison <- function(folder = FALSE){
  run <- req(rv$run)
  paths_to_load <- comparison_browser()$file

  if(length(paths_to_load) == 0 || folder)
    paths_to_load <- comparison_browser()$folder

  req(paths_to_load)

  loaded_paths <- rv_comp$comparison_data$path

  if(folder){
    parent_folder <- req(comparison_browser()$folder)

    # archives children
    archives_to_load <- dir(parent_folder, pattern = "\\.tar\\.gz$", full.names = TRUE, recursive = TRUE) %>%
      normalizePath()

    # folders children
    dirs_to_load <- list.dirs(path = parent_folder, full.names = TRUE, recursive = TRUE) %>%
      .[is_nm_run(.)]

    paths_to_load <- c(archives_to_load, dirs_to_load)
  }

  in_files <- setdiff(paths_to_load, normalizePath(c(loaded_paths, rv$run_path)))

  rv_comp$n_runs <- length(in_files)

  go_parallel <- TRUE

  if(input$parallel_comparison){

    n_master_workers <- ifelse(availableCores() == 1L, 1L, 2L)
    n_slave_workers <- ifelse(n_master_workers == 1L, 1L, min(length(in_files), round(availableCores() * 0.5)))
    #round(availableCores() / 2)))

    print(paste("Slave workers:", n_slave_workers ))

    plan(list(tweak(multiprocess,
                    workers = n_master_workers),
              tweak(multiprocess,
                    workers = n_slave_workers)))

  # } else if(rv_comp$n_runs > 25L) {
  #   plan(list(multiprocess, sequential))
  } else {
    go_parallel <- FALSE
  }

  rv_comp$progression <- 0

  if(go_parallel){
    rv_comp$progress_bar <- Progress$new(min = 0, max = rv_comp$n_runs)

    rv_comp$progress_bar$set(message = "Loading runs...", detail = "Preparing CPUs for parallel processing...")
    rv_comp$parallel_start <- lubridate::now()

    if(file.exists(watcher_file)) file.remove(watcher_file)

    rv_comp$parallel_f <- future({
      start_time <- lubridate::now()

      data <-  tibble(path = in_files)%>%
        mutate(i = row_number()) %>%
        mutate(fut = map2(path, i, ~ future(load_run_temp(.x, .y)))) %>%
        mutate(safe_run = map(fut, safe_value)) %>%
        mutate(run = map(safe_run, "result")) %>%
        select(path, run)

      end_time <- lubridate::now()

      list(pid = Sys.getpid(),
           data = data,
           start = start_time,
           end = end_time)
    })
  } else {
    local_progress <- Progress$new(min = 0, max = rv_comp$n_runs)

    local_progress$set(message = "Loading runs...")

    start_time <- lubridate::now()

    comp_data <-  tibble(path = in_files) %>%
      mutate(i = row_number()) %>%
      mutate(run = map2(path, i,  function(x, y){
        local_progress$inc(1, detail = sprintf("(%s/%s): %s", y, rv_comp$n_runs, basename(x)))
        load_run_temp(x, y, write_progression = FALSE)
      })) %>%
      filter(!map_lgl(run, is.null)) %>%
      select(path, run)

    local_progress$close()

    end_time <- lubridate::now()

    rv_comp$comparison_data <- rv_comp$comparison_data %>%
      bind_rows(comp_data)

    # refresh comparison
    rv_comp$do_compute <- rv_comp$do_compute + 1

    diff_mins <- difftime(end_time, start_time, units = "mins")
    diff_secs <- difftime(end_time, start_time, units = "secs")

    toastr_success(message = sprintf("Runs loaded in %s",
                                     ifelse(diff_mins < 1,
                                            str_c(round(diff_secs, 2), " seconds"),
                                            str_c(round(diff_mins, 2), " minutes"))),
                   title = "Done", timeOut = 10000, position = "bottom-right", closeButton = TRUE)

  }
}

output$control_stream_diffr <- diffr::renderDiffr({
  req(rv_comp$diffr_value)
})

observeEvent(input$click_open_run_to_compare, {
  js <- jsonlite::fromJSON(input$click_open_run_to_compare)

  cp_res <- req(comparison_results())

  r_index <- as.integer(js$run_index)

  r_path <- rv_comp$comparison_data %>%
    slice(r_index) %>%
    pull(path)

  if (!file.exists(r_path)) {
    toastr_error(
      paste("Run not found:", r_path),
      title = "Run loading",
      position = "top-center",
      timeOut = 10000
    )
  } else {
    current_run <- rv$run

    updateTabItems(session, "main_menu", selected = "home")

    rv$run_path <- r_path
    run_browser()$reset(folder_selection = dirname(r_path),
                        file_selection = r_path)
    metadata_browser()$reset(folder_selection = dirname(r_path),
                             file_selection = r_path)


    rv_comp$comparison_data <- rv_comp$comparison_data %>%
      slice(-r_index) %>%
      add_row(path = current_run$info$path, run = list(current_run))
  }
})
