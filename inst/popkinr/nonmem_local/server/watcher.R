rv_futures <- reactiveValues(details = NULL)

print_time <- function(t){
  sprintf("%02d:%02d:%02d", hour(t), minute(t), round(second(t)))
}

safe_value <- safely(future::value)

rv_test <- reactiveValues(wait = FALSE,
                          current_ppids = NULL,
                          ppid_history = NULL,
                          prev_ended = NULL,
                          current_max_cpu = NULL,
                          current_max_runs = NULL)

observe({
  invalidateLater(1000)

  isolate({
    if(rv$run_queue$size() > 0){
      print("=========================================")
      queue <- rv$run_queue$queue()
      print(lubridate::now())

      settings_changed <- FALSE

      if(!identical(rv_test$current_max_cpu, input$max_cpu) ||
         !identical(rv_test$current_max_runs, input$max_runs)){
        settings_changed <- TRUE

        print("Settings changed:")
        print(str_c("MAX CPU : ", rv_test$current_max_cpu," -> ", input$max_cpu))
        print(str_c("MAX RUNS : ", rv_test$current_max_runs," -> ", input$max_runs))
      }

      rv_test$current_max_cpu <- input$max_cpu
      rv_test$current_max_runs <- input$max_runs

      current_jobs <- local_nonmem_jobs()
      ppids <- NULL

      if(!is.null(current_jobs))
        ppids <- current_jobs$PPID

      print(str_c("New jobs: ", str_c(setdiff(ppids, rv_test$ppid_history), collapse = ", ")))
      rv_test$ppid_history <- dplyr::union(rv_test$ppid_history, ppids)
      print(str_c("History: ", str_c(rv_test$ppid_history, collapse = ", ")))

      new_ended <- rv_test$ppid_history[which(!(rv_test$ppid_history %in% ppids) & !(rv_test$ppid_history %in% rv_test$prev_ended))]
      rv_test$prev_ended <- unique(c(rv_test$prev_ended, new_ended))
      print(str_c("All Ended: ", str_c(rv_test$prev_ended, collapse = ", ")))
      print(str_c("New Ended: ", str_c(new_ended, collapse = ", ")))

      print(str_c("Running: ", str_c(ppids, collapse = ", ")))

      n_runs <- length(ppids)
      current_load <- ifelse(n_runs > 0, sum(current_jobs$n_cpu), 0L)

      if(rv_test$wait){
        if(settings_changed || length(new_ended) > 0 || rv$future$size() == 0){
          print("STOP WAITING ===========")
          rv_test$wait <- FALSE
        } else {
          return(NULL)
        }
      }

      new_run_count <- n_runs
      new_cpu_load <- current_load

      shift <- 0
      for(i in seq_along(queue)){

        to_start <- queue[[i]]

        run_args <- to_start$task

        # prevent overloading
        # do not start run if MAX_LOAD already achieved
        if((new_run_count >= req(input$max_runs)) ||
           (new_cpu_load + run_args$n_nodes) > (req(input$max_cpu))){
          rv_test$wait <- TRUE
          next
        }

        print(str_c("Starting: ", run_args$control_file))


        rv$future$enqueue({
          future({
            do.call(nm_exec, args = run_args)
          })
        },
        start_message = sprintf("Starting %s (%s %s)",
                                basename(run_args$control_file),
                                run_args$n_nodes,
                                ifelse(run_args$n_nodes > 1, "CPUs", "CPU")),
        end_message = sprintf("Completed run: %s", basename(run_args$control_file)))

        # Remove run request from queue
        rv$run_queue$remove_at(i - shift)
        shift <- shift + 1
        new_run_count <- new_run_count + 1
        new_cpu_load <- new_cpu_load + run_args$n_nodes

        local_list_trigger$trigger()

      }
    }
  })
})

### Job submission queue
observe({
  isolate({
    if(rv$future$size() == 0) {
      #cat("Nothing scheduled\n")
    }  else{
      current_queue <- rv$future$queue()

      is_resol <- map_lgl(current_queue, ~resolved(.$task))

      resolved_q <- current_queue[is_resol]
      unresolved_q <- current_queue[!is_resol]

      if(length(resolved_q) > 0){
        resol_id <- which(is_resol)

        if(length(resol_id) > 1) {

          resol_id <- resol_id - seq(0, length(resol_id) - 1)
        }

        walk(resol_id, function(i){
          x <- rv$future$remove_at(i)

          val <- safe_value(x$task)

          if(!is.null(val$error)){
            toastr_error(message = val$error$message, title = "Error", timeOut = 0, position = "bottom-right", closeButton = TRUE)
          } else if(!is.null(x$message)){
            toastr_success(message = x$message, title = "Done", timeOut = 0, position = "bottom-right", closeButton = TRUE)

            submission_trigger$trigger()
          }
        })
      }
    }
  })

  invalidateLater(1000) # Wait 1s
})

### Job viewing details
observe({
  req(rv_futures$details)

  res <- rv_futures$details

  isolate({
    shinyjs::html("dialog_check", print_time(now()))

    shinyjs::toggle("run_details_box", condition = !is.null(res))
    shinyjs::toggle("job_is_not_running", condition = is.null(res))
    shinyjs::toggle("job_actions", condition = !is.null(res))

    if(is.null(res)) return(NULL)

    shinyjs::html("dialog_request_duration", round(res$request_duration, 2))

    rv$temp_run_details <- list(number = res$number,
                                iterations = res$iterations,
                                control_stream = res$control_stream)

    shinyjs::html("dialog_update", print_time(now()))
  })

  invalidateLater(1000) # Wait 1s
})

