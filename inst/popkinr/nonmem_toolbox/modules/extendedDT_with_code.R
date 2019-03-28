extendedDT_with_code_UI <- function(id, title = NULL){
  ns <- NS(id)

  dig_name <- ns("digits")

  div(
    h4(title),
    tags$script(HTML(sprintf('$(document).on("input", "#%s", function () { Shiny.onInputChange("%s", this.value); })',
                             dig_name, dig_name, dig_name))),
    DT::dataTableOutput(ns("table")),
    div(id = ns("r_code_section"),
        h4("R code"),
        shinyAce::aceEditor(ns("r_code"), mode = "r", height = "100px", readOnly = TRUE, wordWrap = TRUE)))
}

# reactive_table: either a data frame or a DT::datatable object (for custom formatting)
extendedDT_with_code <- function(input, output, session, reactive_table, filename = "table", digits = 4,
                          buttons = TRUE, r_code = TRUE, ...){

  toolbar_id <- str_c(session$ns("toolbar"))
  export_id <- str_c(session$ns("export"))
  dig_widget <- str_c(session$ns("digits"))
  buttons_top_id <- session$ns("buttons_top")


  build_pmxploit_table_call <- function(){
    # Get reactive_table function content
    reactive_wrapper_envir <- get_env(reactive_table)
    reactive_wrapper_fn <- reactive_wrapper_envir$.origFunc
    reactive_wrapper_first_line <- as.list(reactive_wrapper_fn)[[1]]

    # Get the inner reactive function content
    table_reactive <- call_fn(reactive_wrapper_first_line, get_env(reactive_wrapper_fn))
    table_reactive_envir <- get_env(table_reactive)
    table_reactive_fn <- table_reactive_envir$.origFunc

    # Evaluate the tableting function
    table_fn_envir <- get_env(table_reactive_fn)
    table_fn_body <- body(table_reactive_fn)

    eval(table_fn_body, table_fn_envir)

    # Extract the last line of the function and get the expression set to the first argument
    # -> supposed to be a call to a pmxploit summarizing function
    return_line <- as.list(table_fn_body) %>% keep(is_call) %>% last() %>% .[[2]]

    # Extract the links of a pipe chain (eg. filter %>% group_by %>% table_fn)
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

    # Extract current filters from the application reactiveValues `rv`
    main_rv <- env_get(table_fn_envir, "rv")
    if(length(filter_link) == 1L){
      filter_fn <- call_name(filter_link[[1]])
      run_filters <- main_rv$app_filters

      if(filter_fn == "filtered_run_show_mdv")
        run_filters <- discard(run_filters, ~ all.equal(., quo(MDV == 0)) == TRUE)

      if(length(run_filters) > 0L){
        # remove `~` operator
        run_filters <- run_filters %>% map(~as.list(.)[[2]])

        # Create a filter call
        filter_call <- call2(quote(filter), UQS(run_filters))
      }
    }

    # Check if the table must be grouped/facetted
    group_by_call <- NULL
    group_by_chain <- chain_links[which(map_lgl(chain_links, ~ is_call(., "group_by")))]

    if(length(group_by_chain) == 1L){
      # Extract current grouping columns
      grps <- env_get(table_fn_envir, as.character(call_args(call_args(group_by_chain[[1]])[[1]])[[1]]))

      if(length(grps) > 0){
        group_by_call <- call2(quote(group_by), UQS(syms(grps)))
      }
    }

    # Last link should be a pmxploit table function calls
    pmxploit_chain <- last(chain_links)

    # Get the arguments of the function call
    fargs <- call_args(pmxploit_chain)

    # Evaluate the arguments to get the UI widgets values
    args_values <- map(fargs[names(fargs) != ""], ~ unname(eval(., envir = table_fn_envir)))
    # args_values <- map(fargs[names(fargs) != ""], ~ eval(., envir = table_fn_envir))

    edit_call <- function(cc, ...){
      args <- dots_list(...)
      call_modify(cc, UQS(args))
    }

    # Edit the call to integrate de arguments values
    # pmxploit_call <- edit_call(pmxploit_chain, UQS(args_values))

    # NEW: Remove default arguments that are not changed
    # browser()
    original_args <- formals(eval(first(pmxploit_chain)))

    args_to_skip <- map2_lgl(args_values, original_args[names(args_values)], function(a, b){
      if(is_missing(b)) return(FALSE)
      identical(unname(a), unname(eval(b)))
    })

    args_values <- args_values[!args_to_skip]
    pmxploit_call <- call2(first(pmxploit_chain), UQS(args_values))

    # Create a `load_nm_run` call with the run path
    load_run_call <- call2(quote(load_nm_run), main_rv$run$info$path)

    if(identical(main_rv$run, pmxploit::EXAMPLERUN)){
      load_run_call <- quote(pmxploit::EXAMPLERUN)
    }

    # Construct the full call:
    # load_nm_run %>% filter (if any) %>% group_by (if any) %>% pmxploit_call %>% theme_pmx())
    calls <- c(load_run_call, filter_call, group_by_call, pmxploit_call)
    txt <- map_chr(calls, ~ str_c(deparse(., width.cutoff = 150L), collapse = "\n"))
    full_text <- str_c(txt, collapse = " %>%\n\t")

    shinyAce::updateAceEditor(session, session$ns("r_code"), full_text)
  }




  rv <- reactiveValues(n_digits = digits)

  safe_stuff <- safely(function(){
    df <- inner_table()

    if(is(df, "datatables"))
      df <- df$x$data

    (nrow(df) > 0)
  })

  observe({
    output[[export_id]] <- downloadHandler( filename = function() {
      export_id
    }, content =  function(file){
      write_excel_csv(mtcars, file, na = ".")
    })

    # Enable export button
    check <- safe_stuff()

    enabled <- ifelse(!is.null(check$error), FALSE, check$result)

    shinyjs::toggleState("button", condition = enabled)


    shinyjs::toggle("r_code_section", condition = r_code)
  })

  inner_table <- reactive({

    val <- req(reactive_table())
    #attr(reactive_table, "observable")$getValue()

    dt <- val$formatting(val$data)

    if(r_code)
      build_pmxploit_table_call()

    dt
  })

  observeEvent(input$digits, {
    rv$n_digits <- as.integer(input$digits)
  })

  output$table <- renderDataTable({
    df <- req(inner_table())

    dt <- NULL

    if(is(df, "datatables")) {
      dt <- df
    } else {
      dt <- datatable(df, extensions = "Buttons", ...)
    }

    n_digits <- rv$n_digits

    if(!is.null(n_digits) && !is.na(n_digits)){
      dt$x$data <- dt$x$data %>%
        mutate_if(is.double, ~ signif(., digits = n_digits))
    }

    if(buttons){

      # Edit DOM to integrate buttons and digits selection in toolbar
      dt$x$options$dom <- str_c(sprintf('rt<"row"<"col-sm-2"<"#%s">><"col-sm-2"<"#%s">><"col-sm-8"<"#%s"><B>>>',
                                        toolbar_id, export_id, buttons_top_id),
                                str_replace_all(dt$x$options$dom, "r|t", "")) # remove r ("processing") and t ("table") from current DOM

      dt$x$options$buttons <- list(list(extend = 'csv', filename = filename),
                                   list(extend = 'excel', filename = filename),
                                   list(extend = 'pdf', filename = filename),
                                   "copy")

      toolbar_content <-  sprintf('<label for="%s">Significant digits</label><input id="%s" type="number" class="form-control" value="%s" />',
                                  dig_widget, dig_widget, n_digits)
      export_content <- sprintf('<label for="%s">All data</label><br /><a id="%s" class="btn btn-default shiny-download-link " href="%s" target="_blank"><i class="fa fa-download" />&nbsp;Export (*.csv)</a>',
                                export_id, export_id,
                                session$registerDownload(name = export_id,
                                                         contentType = NA,
                                                         filename = function() {paste0(filename, ".csv")
                                                         },
                                                         function(file){
                                                           df <- req(inner_table())

                                                           if(is(df, "datatables"))
                                                             df <- df$x$data

                                                           write_excel_csv(df, file, na = ".")
                                                         }))

      dt$x$options$drawCallback <- JS(sprintf("function() {$('#%s').html('%s'); $('#%s').html('<label>Current view</label>'); $('#%s').html('%s');}",
                                              toolbar_id,
                                              toolbar_content,
                                              buttons_top_id,
                                              export_id,
                                              export_content))

    }

    dt
  })
}
