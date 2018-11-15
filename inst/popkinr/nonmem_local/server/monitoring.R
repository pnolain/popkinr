local_nonmem_pids <- reactivePoll(100, session,
                                  checkFunc = function(){
                                    q_system2 <- quietly(system2)
                                    pidof_cmd <- q_system2("pidof", "nonmem", stdout = TRUE)

                                    if(length(pidof_cmd$warnings) > 0)
                                      return("")

                                    pidof_cmd$result
                                  },
                                  valueFunc = function(){
                                    pidof_cmd <- system2("pidof", "nonmem", stdout = TRUE)

                                    if(length(pidof_cmd) == 0) return(NULL)

                                    pids <- pidof_cmd %>%
                                      str_extract_all("\\d+") %>%
                                      unlist() %>%
                                      as.integer()
                                  })

local_nonmem_cpu_usage <- reactive({

  pids <- local_nonmem_pids()

  if(is.null(pids)) return(NULL)

  tibble(PID = pids) %>%
    mutate(usage = map(PID, get_process_usage)) %>%
    filter(!map_lgl(usage, is.null)) %>%
    unnest()
})

local_nonmem_jobs <- reactive({
  input$refresh
  usage <- local_nonmem_cpu_usage()

  if(is.null(usage)) return(NULL)

  usage %>%
    mutate(path = map_chr(PID, function(x){
      wdir <- get_process_wdir(x)
      ifelse(is.null(wdir), "", wdir)
    })) %>%
    #filter(path != "") %>%
    arrange(path) %>%
    mutate(is_master = !str_detect(path, "worker\\d+$")) %>%
    mutate(run_id = cumsum(is_master)) %>%
    rename(start_time = START) %>%
    group_by(PPID) %>%
    summarise(user = USER[1],
              PIDS = list(PID),
              start_time = start_time[1],
              n_cpu = n(),
              mean_cpu_usage = mean(`%CPU`),
              path = path[1]) %>%
    arrange(desc(start_time))
})

observe({
  global_local_jobs <<- local_nonmem_jobs()
})

observeEvent(input$refresh, {
  print(str_c("refresh: ", input$refresh))
})

observeEvent(input$kill_all_runs, {
  jobs <- req(local_nonmem_jobs())

  ppids <- jobs$PIDS %>% unlist()

  walk(ppids, kill_run)
})

output$queued_list <- renderDataTable({
  input$refresh

  local_list_trigger$depend()

  queue <- req(rv$run_queue)

  df <- tibble()

  if(!is.null(queue) && queue$size() > 0){
    df <- queue$queue() %>%
      map_df("task") %>%
      select(control_file, n_nodes)

    print(str_c("queued jobs:", nrow(df)))
  }

  datatable(df, options = list(dom = "rtip"))
})

local_nonmem_jobs_ppids <- reactive({
  local_nonmem_jobs()$PPID
})

output$local_run_list <- renderDataTable({
  # depend on ppids to refresh
  local_nonmem_jobs_ppids()

  # input$refresh
  #local_list_trigger$depend()

  jobs <- isolate(local_nonmem_jobs())

  if(!is.null(jobs)){
    print(str_c("running jobs:", nrow(jobs)))
    df <- jobs %>%
      mutate(details = sprintf("<a href=\"#/\" onclick=\"show_run_details('%s', true);\">View</a>", PPID),
             PIDS = map_chr(PIDS, str_c, collapse = ", "),
             stop = pmap_chr(list(user = user, path = path, PPID = PPID),
                             function(user, path, PPID){
                               ifelse(any(list.files(path) == "sig.stop"),
                                      "Stopping...",
                                      ifelse(user == Sys.info()["user"],
                                             sprintf("<a href=\"#/\" onclick=\"stop_run('%s', true);\">Stop</a>", PPID),
                                             ""))
                             }),
             start_time = map_chr(start_time, as.character))

    dt <- datatable(df, escape = FALSE, selection = "none",
                    options = list(paging = FALSE, scrollX = TRUE, com = "rt"))
  } else {
    print("no running job")
    dt <- datatable(tibble())
  }

  dt
})

my_promise_error_handling <- function(error){
  err_msg <- error$message

  shinyjs::html("dialog_msg", str_c("<span style='color: red; font-weight: bold;'>", err_msg, "</span>"))
  print(err_msg)

  return(NULL)
}

observe({
  invalidateLater(3000)

  if(!is.null(input$auto_refresh) && input$auto_refresh){

    isolate({

      req(!is.null(rv$run_number))

      r_number <- rv$run_number
      s_time <- now()

      future({ refresh_run_check(r_number, s_time) }) %>%
        then(
          onFulfilled = function(value) {
            rv_futures$details <- value
            shinyjs::html("dialog_msg", "")
          },
          onRejected = function(err) {
            warning("An error occurred: ", err)
            my_promise_error_handling(err)
          }
        )
    })
  }
})

observeEvent(input$show_run_details, {
  js <- jsonlite::fromJSON(input$show_run_details)

  r_number <-  js$run_number

  rv$run_number <- js$run_number

  s_time <- now()

  browser()
  future({refresh_run_check(r_number, s_time) }) %>%
    then(
      onFulfilled = function(value) {
        # browser()
        rv_futures$details <- value
        shinyjs::html("dialog_msg", "")
      },
      onRejected = function(err) {
        warning("An error occurred: ", err)
        my_promise_error_handling(err)
      }
    )

  unlink(str_c(app_temp_directory, "rundata", sep = "/"), recursive = TRUE)

  pbs_tab <- NULL

  showModal(modalDialog(title = sprintf("Run details: %s", sprintf("local (process: %s)", r_number)),
                        size = "l",
                        shinyjs::hidden(
                          div(id = "run_details_box",
                              fluidRow(
                                tabBox(
                                  width = 12,
                                  tabPanel(title = "OFV",
                                           plotOutput("ofv_plot", height = "550px")),
                                  tabPanel(title = "Parameters",
                                           flowLayout(
                                             selectInput("params_type", "Select parameters type", choices = c("THETA", "OMEGA", "SIGMA")),
                                             selectInput("params_value", "Visualize", choices = c("Estimate" = 1, "Change from initial value" = 2)),
                                             conditionalPanel(condition = "input.params_value != 1",
                                                              checkboxInput("params_facetted", "Facetted", value = TRUE)
                                             )
                                           ),
                                           plotOutput("parameters_plot", height = "550px")),
                                  tabPanel(title = "Iterations",
                                           dataTableOutput("iterations_table")),
                                  tabPanel(title = "Control file",
                                           verbatimTextOutput("control_file_code"))
                                )))
                        ),
                        shinyjs::hidden(div(id = "job_is_not_running", sprintf("Job %s is not running", r_number))),
                        footer = fluidRow(
                          column(6,
                                 checkboxInput("auto_refresh", "Auto-refresh", value = TRUE),
                                 style = "text-align: left;",
                                 span(id = "dialog_msg", ""),
                                 br(),
                                 span("Last check: "), span(id = "dialog_check", ""),
                                 span("Last request duration:"), span(id = "dialog_request_duration"),
                                 span("Last update: "), span(id = "dialog_update", "")),
                          column(4,
                                 div(id = "job_actions",
                                     tags$button(class = "btn btn-default", onclick = sprintf("next_estimation('%s');", r_number), icon("step-forward"), "Move to next estimation mode/step"),
                                     tags$button(class = "btn btn-default", onclick = sprintf("stop_run('%s');", r_number), icon("stop-circle"), "Stop"),
                                     tags$button(class = "btn btn-default", onclick = sprintf("kill_run('%s');", r_number), icon("close"), "Kill")
                                 )
                          ),
                          column(2,
                                 actionButton("close_dialog", "Close")))))
})

observeEvent(input$close_dialog, {
  removeUI(selector = "#ofv_plot")

  rv$run_number <- NULL
  rv$temp_run_details <- NULL

  removeModal()
})

observeEvent(input$next_estimation, {
  js <- jsonlite::fromJSON(input$next_estimation)

  r_number <- js$run_number

  rv$future$enqueue(future({
    next_estimation(r_number)
  }), start_message = sprintf("Move to next estimation %s", r_number))
})

observeEvent(input$stop_run, {
  js <- jsonlite::fromJSON(input$stop_run)

  r_number <- js$run_number

  rv$future$enqueue(future({
    stop_run(r_number)

    local_list_trigger$trigger()
  }), start_message = sprintf("Stop %s", r_number))
})

observeEvent(input$kill_run, {
  js <- jsonlite::fromJSON(input$kill_run)

  r_number <- js$run_number

  current_jobs <- req(local_nonmem_jobs())

  rv$future$enqueue(future({
    pids <- current_jobs %>%
      filter(PPID == r_number) %>%
      pull(PIDS) %>%
      unlist()

    walk(pids, kill_run)

    local_list_trigger$trigger()
  }), start_message = sprintf("Kill %s", r_number))

  rv$run_number <- NULL

  removeModal()
})

output$iterations_table <- renderDataTable({
  run <- req(rv$temp_run_details)

  data <- req(run$iterations)

  req(nrow(data) > 0)

  estimation <- data %>%
    unnest() %>%
    rename(estimation_number = number) %>%
    select(-goal_function, -problem, -subproblem, -superproblem1, -iteration1, -superproblem2, -iteration2)

  if(!is.null(run$control_stream)){
    thetas <- run$control_stream$parameters$definition %>%
      filter(type == "theta")

    colnames(estimation) <- plyr::mapvalues(colnames(estimation),
                                            from = thetas$id,
                                            to = thetas$name, warn_missing = FALSE)
  }

  dt <- datatable(estimation, rownames = FALSE, options = list(dom = "rtp",
                                                               scrollY = "600px",
                                                               scrollX = TRUE,
                                                               scrollCollapse = !(nrow(estimation) == 0),
                                                               paging = F,
                                                               language = list(emptyTable = "No iteration reported yet")))

  dt
})

output$ofv_plot <- renderPlot({
  run <- req(rv$temp_run_details)

  data <- req(run$iterations)

  if(nrow(data) == 0)
    return

  estimation <- data %>%
    slice(n())

  if(is.null(estimation$iterations) || length(estimation$iterations) == 0 || nrow(estimation$iterations[[1]]) == 0)
    return(NULL)

  iterations_data <- estimation %>%
    .$iterations %>%
    .[[1]]

  it_df <- iterations_data %>%
    filter(ITERATION > -1000000000) %>%
    select(-starts_with("THETA"), -starts_with("OMEGA"), -starts_with("SIGMA")) %>%
    mutate(n = row_number())

  # ofv_col <- switch(estimation$method,
  #                   "Stochastic Approximation Expectation-Maximization" = "SAEMOBJ",
  #                   "NUTS Bayesian Analysis" = "MCMCOBJ",
  #                   "OBJ")
  ofv_col <- last(colnames(iterations_data))

  is_saem <- ofv_col == "SAEMOBJ"

  x_col <- "n"

  my_geom <- ifelse(nrow(it_df) == 1, geom_point, geom_line)

  g <- ggplot(it_df, aes_string(x_col, y = ofv_col))+
    my_geom()

  n_x_breaks <- 50
  rounding_step <- ifelse(nrow(it_df) <= 300, 10, ifelse(nrow(it_df) <= 1000, 50, 100))

  if(is_saem){
    indices <- round(seq(1, nrow(it_df), length.out = min(n_x_breaks, nrow(it_df))))

    iterations_edges <- it_df %>%
      mutate(step = ITERATION - lag(ITERATION),
             breaking = is.na(step) | (step != lag(step))) %>%
      mutate(breaking = ifelse(!is.na(lag(breaking)) & lag(breaking), FALSE, breaking))  %>%
      filter(breaking | row_number() == n())

    g <- g + geom_vline(data = filter(iterations_edges, !is.na(step) & breaking), mapping = aes(xintercept = n),
                        linetype = "dashed", colour = "red")

    all_indices <- c(indices, iterations_edges$n) %>% unique %>% sort

    selected_indices  <-  all_indices

    if(length(selected_indices) > n_x_breaks)
      selected_indices <- unique(plyr::round_any(all_indices, rounding_step)) + 1

    phase_breaks <- it_df %>%
      slice(selected_indices)

    g <- g + scale_x_continuous(breaks = phase_breaks$n, labels = phase_breaks$ITERATION)
  } else {
    if(nrow(it_df) > n_x_breaks){
      step <- ifelse(nrow(it_df) <= 300, 10, ifelse(nrow(it_df) <= 1000, 50, 100))
      indices <- round(seq(1, nrow(it_df), length.out = n_x_breaks))
      selected_indices  <- (unique(indices) %>% sort %>% plyr::round_any(step))+1

      it_df <- it_df %>%
        slice(selected_indices)
    }

    g <- g + scale_x_continuous(breaks = it_df$n, labels = it_df$ITERATION)
  }

  g <- g+
    labs(title = sprintf("Local run (process: %s)", run$number),
         subtitle = sprintf("Estimation %s: %s", estimation$number, estimation$method),
         x = "Iterations", y = ofv_col)


  g

})

output$parameters_plot <- renderPlot({
  run <- req(rv$temp_run_details)

  data <- req(run$iterations)

  req(nrow(data) > 0)

  estimation <- data %>%
    slice(n())

  req(nrow(estimation$iterations[[1]]) > 1)

  it_df <- estimation %>%
    .$iterations %>%
    .[[1]] %>%
    filter(ITERATION > -1000000000) %>%
    mutate(n = row_number())

  g_df <- it_df %>%
    select(ITERATION, n, starts_with(input$params_type)) %>%
    gather(parameter, estimate, -ITERATION, -n)

  if(input$params_value == "2"){
    g_df <- g_df %>%
      group_by(parameter) %>%
      mutate(estimate = (estimate - estimate[1]) / estimate[1]) %>%
      ungroup()
  }

  if(!is.null(run$control_stream)){
    thetas <- run$control_stream$parameters$definition %>%
      filter(type == "theta")

    g_df$parameter <- plyr::mapvalues(g_df$parameter,
                                      from = thetas$id,
                                      to = thetas$name, warn_missing = FALSE)
  }

  g <- ggplot(g_df, aes(x = n, y = estimate))+
    geom_line()

  if(input$params_value == "1" || input$params_facetted){
    g <- g+
      facet_wrap(~parameter, scales = "free")
  } else {
    g$mapping$colour <- quote(parameter)
    gg2 <- g_df %>% group_by(parameter) %>% slice(n())
    g <- g +
      #geom_text(data = gg2, aes(x = n, y = estimate, label = parameter), hjust = "left", inherit.aes = FALSE)
      ggrepel::geom_text_repel(
        data = gg2,
        aes(label = parameter), colour = "black",
        arrow = arrow(length = unit(0.01, "npc")),
        box.padding = 1, size = 5)
  }

  is_saem <- str_detect(estimation$method, "Stochastic Approximation Expectation-Maximization")

  n_x_breaks <- 50
  rounding_step <- ifelse(nrow(it_df) <= 300, 10, ifelse(nrow(it_df) <= 1000, 50, 100))

  if(is_saem){
    indices <- round(seq(1, nrow(it_df), length.out = min(n_x_breaks, nrow(it_df))))

    iterations_edges <- it_df %>%
      mutate(step = ITERATION - lag(ITERATION),
             breaking = is.na(step) | (step != lag(step))) %>%
      mutate(breaking = ifelse(!is.na(lag(breaking)) & lag(breaking), FALSE, breaking))  %>%
      filter(breaking | row_number() == n())

    g <- g + geom_vline(data = filter(iterations_edges, !is.na(step) & breaking), mapping = aes(xintercept = n),
                        linetype = "dashed", colour = "red")

    all_indices <- c(indices, iterations_edges$n) %>% unique %>% sort

    selected_indices  <-  all_indices

    if(length(selected_indices) > n_x_breaks)
      selected_indices <- unique(plyr::round_any(all_indices, rounding_step)) + 1

    phase_breaks <- it_df %>%
      slice(selected_indices)

    g <- g + scale_x_continuous(breaks = phase_breaks$n, labels = phase_breaks$ITERATION)
  } else {
    if(nrow(it_df) > n_x_breaks){
      step <- ifelse(nrow(it_df) <= 300, 10, ifelse(nrow(it_df) <= 1000, 50, 100))
      indices <- round(seq(1, nrow(it_df), length.out = n_x_breaks))
      selected_indices  <- (unique(indices) %>% sort %>% plyr::round_any(step))+1

      it_df <- it_df %>%
        slice(selected_indices)
    }

    g <- g + scale_x_continuous(breaks = it_df$n, labels = it_df$ITERATION)
  }


  if(input$params_value == "2"){
    g <- g +
      scale_y_continuous(labels = scales::percent, name = "Change from initial value (%)")+
      scale_color_discrete(guide = guide_legend(ncol = 12))
  }

  g+
    labs(title = sprintf("Run %s", run$number),
         subtitle = sprintf("Estimation %s: %s", estimation$number, estimation$method),
         x = "Iterations")

})


output$control_file_code <- renderPrint({
  run <- req(rv$temp_run_details)

  writeLines(run$control_stream$code)
})

outputOptions(output, "ofv_plot", suspendWhenHidden = FALSE)
outputOptions(output, "parameters_plot", suspendWhenHidden = FALSE)
outputOptions(output, "iterations_table", suspendWhenHidden = FALSE)
outputOptions(output, "control_file_code", suspendWhenHidden = FALSE)

outputOptions(output, "local_run_list", suspendWhenHidden = FALSE)
outputOptions(output, "queued_list", suspendWhenHidden = FALSE)
