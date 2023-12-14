extendedPlotUI <- function(id, title = "plot", exportable = TRUE, zoomable = TRUE, verbose = FALSE,
                           export_width = 300, export_height = 170, customizable = TRUE, ...){
  ns <- NS(id)



  if(zoomable){
    zplot <- plotOutput(ns("zplot"), ...,
                        brush = brushOpts(id = ns("brush"), resetOnNew = TRUE),
                        dblclick = ns("brush_dblclick"))
  } else{
    zplot <- plotOutput(ns("zplot"), ...)
  }

  verb_text <- NULL

  if(verbose) {
    verb_text <- uiOutput(ns("plot_info_text"))
  }

  export <- NULL

  if(exportable){
    export <-list(
      fluidRow(
        column(2,
               flowLayout(uiOutput(ns("facetting_output")),
                          (if(customizable) actionButton(ns("customize"), label = "Customize") else NULL))),
        column(2, selectInput(ns("file_type"), "Export file type",
                              choices = c("png", "pdf", "tex (pictex)" = "tex", "svg"))),
        column(2, numericInput(ns("width"), "Width (mm)", value = export_width)),
        column(2, numericInput(ns("height"), "Height (mm)", value = export_height)),
        column(4, textInput(ns("filename"), "File name", value = title, width = "100%"))),
      fluidRow(
        column(11, div(align = "center",
                       downloadButton(ns("download_plot"), label = "Export plot"),
                       actionButton(ns("save_plot"), "Save in run files"),
                       downloadButton(ns("download_ggplot"), label = "Export ggplot2 object (*.rds)")))),

      div(id = ns("r_code_section"),
          h4("R code"),
          shinyAce::aceEditor(ns("r_code"), mode = "r", height = "80px", readOnly = TRUE, wordWrap = TRUE)))
  }

  div(
    withSpinner(zplot, color = "#008d4c"),
    verb_text,
    export)
}

extendedPlot <- function(input, output, session,
                         reactive_run,
                         reactive_plot = NULL,
                         plot_extra = NULL,
                         r_code = TRUE){

  build_pmxploit_plot_call <- function(){
    # browser()

    # Get reactive_plot function content
    reactive_wrapper_envir <- get_env(reactive_plot)
    reactive_wrapper_fn <- reactive_wrapper_envir$.origFunc
    reactive_wrapper_first_line <- as.list(reactive_wrapper_fn)[[1]]

    # Get the inner reactive function content
    plot_reactive <- call_fn(reactive_wrapper_first_line, get_env(reactive_wrapper_fn))
    plot_reactive_envir <- get_env(plot_reactive)
    plot_reactive_fn <- plot_reactive_envir$.origFunc

    # Evaluate the plotting function
    plot_fn_envir <- get_env(plot_reactive_fn)
    plot_fn_body <- body(plot_reactive_fn)

    eval(plot_fn_body, plot_fn_envir)

    # Extract the last line of the function
    # -> supposed to be a call to a pmxploit plotting function
    return_line <- as.list(plot_fn_body) %>% keep(is_call) %>% last()

    # Extract the links of a pipe chain (eg. filter %>% group_by %>% plot_fn)
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
      main_rv <- env_get(plot_fn_envir, "rv", inherit = TRUE)
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

    # Check if the plot must be grouped/facetted
    group_by_call <- NULL
    group_by_chain <- chain_links[which(map_lgl(chain_links, ~ is_call(., "group_by")))]

    if(length(group_by_chain) == 1L){
      # Extract current grouping columns
      # grps <- env_get(plot_fn_envir, as.character(call_args(call_args(group_by_chain[[1]])[[1]])[[1]]))
      grps <- eval(group_by_chain[[1]][[-1]][[-1]][[-1]][[-1]], plot_fn_envir)

      if(length(grps) > 0){
        group_by_call <- call2(quote(group_by), !!!(syms(grps)))
      }
    }

    # Last link should be a pmxploit plot function calls
    pmxploit_chain <- last(chain_links)

    # Get the arguments of the function call
    fargs <- call_args(pmxploit_chain)

    # Evaluate the arguments to get the UI widgets values
    args_values <- map(fargs[names(fargs) != ""], ~ unname(eval(., envir = plot_fn_envir)))
    # args_values <- map(fargs[names(fargs) != ""], ~ eval(., envir = plot_fn_envir))

    edit_call <- function(cc, ...){
      args <- dots_list(...)
      call_modify(cc, !!!(args))
    }

    # Edit the call to integrate de arguments values
    # pmxploit_call <- edit_call(pmxploit_chain, !!!(args_values))

    # NEW: Remove default arguments that are not changed
    # browser()
    original_args <- formals(eval(pmxploit_chain[[1]]))

    args_to_skip <- map2_lgl(args_values, original_args[names(args_values)], function(a, b){
      if(is_missing(b)) return(FALSE)
      identical(unname(a), unname(eval(b)))
    })

    args_values <- args_values[!args_to_skip]
    pmxploit_call <- call2(pmxploit_chain[[1]], !!!(args_values))

    # Create a `load_nm_run` call with the run path
    load_run_call <- call2(quote(load_nm_run), inner_run()$info$path)

    if(identical(inner_run(), pmxploit::EXAMPLERUN)){
      load_run_call <- quote(pmxploit::EXAMPLERUN)
    }

    # Construct the full call:
    # load_nm_run %>% filter (if any) %>% group_by (if any) %>% pmxploit_call %>% theme_pmx())
    calls <- c(load_run_call, filter_call, group_by_call, pmxploit_call)
    txt <- map_chr(calls, ~ str_c(deparse(., width.cutoff = 150L), collapse = "\n"))
    full_text <- str_c(txt, collapse = " %>%\n\t")

    print(writeLines(str_c(full_text, "+\n\ttheme_pmx()")))

    shinyAce::updateAceEditor(session,
                              editorId = "r_code",
                              value = str_c(full_text, "+\n\ttheme_pmx()"))
  }


  observeEvent(input$save_plot, {
    run <- req(inner_run())
    run_path <- run$info$path

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

      g <-  req(zoomed_plot())$plot

      file <- sprintf("%s-%s-%s.%s", inner_run()$info$run_id, input$filename,
                      format(Sys.time(), "%y%m%d-%H%M%S"),
                      input$file_type)

      target_file <- paste(temp_subdir, file, sep = "/")

      # Workaround when no graphics device is available:
      # (Error: cannot open file 'Rplots.pdf')
      # http://github.com/ropensci/plotly/494

      ggsave(plot = g, filename = target_file,
             units = "mm", width = input$width, height = input$height)

      if(!is_directory){
        wd <- setwd(run_parent_dir)

        system2("tar", sprintf("-czvf '%s' -C '%s' .", basename(run_path), temp_dir), stdout = FALSE)

        setwd(wd)

        unlink(temp_dir, recursive = TRUE)
      }

      setProgress(1, detail = "Done !")
    }, message = sprintf("Saving image to run %s", source_type),
    detail = sprintf("Updating %s...", source_type))
  })

  rv <- reactiveValues(x = NULL,
                       y = NULL,
                       text = NULL,
                       plot_theme = "Default",
                       plot_title = NULL,
                       plot_subtitle = NULL,
                       plot_x_label = NULL,
                       plot_y_label = NULL,
                       wrap_rows = NULL,
                       n_facets = NULL,
                       axis_label_rotation = 0)

  observeEvent(input$brush_dblclick, {
    brush <- input$brush

    if (!is.null(brush)) {
      rv$x <- c(brush$xmin, brush$xmax)
      rv$y <- c(brush$ymin, brush$ymax)

    } else {
      rv$x <- NULL
      rv$y <- NULL
    }
  })

  plot_function <- function(){
    g <- req(reactive_plot())

    if(r_code)
      build_pmxploit_plot_call()

    g
  }

  is_null_or_empty <- function(text){
    is.null(text) || text == ""
  }

  observe({
    rr <- inner_run()

    rv$plot_title <- NULL
    rv$plot_subtitle <- NULL
    rv$plot_x_label <- NULL
    rv$plot_y_label <- NULL
    rv$wrap_rows <- NULL

    if(!is.null(rr)){
      has_rights <- unname(file.access(rr$info$path, mode = 2)) == 0
      shinyjs::toggleState("save_plot", condition = has_rights)
    }

    shinyjs::toggle("r_code_section", condition = r_code)
  })

  inner_run <- reactive({
    reactive_run()
  })

  zoomed_plot <- reactive({

    draw_plot <- purrr::quietly(plot_function)

    res <- draw_plot()

    g <- res$result

    if(!is.null(plot_extra)){
      ext <- plot_extra()

      g <- g + ext
    }

    z <- list(x = rv$x, y = rv$y)

    if(!is.null(z$x) & !is.null(z$y))
      g <- g + coord_cartesian(xlim = z$x, ylim = z$y)

    if(!is_null_or_empty(rv$plot_title))
      g <- g + labs(title = rv$plot_title)
    if(!is_null_or_empty(rv$plot_subtitle))
      g <- g + labs(subtitle = rv$plot_subtitle)
    if(!is_null_or_empty(rv$plot_x_label))
      g <- g + labs(x = rv$plot_x_label)
    if(!is_null_or_empty(rv$plot_y_label))
      g <- g + labs(y = rv$plot_y_label)
    if(!is_null_or_empty(rv$wrap_rows) && !is.na(rv$wrap_rows))
      g$facet$params$nrow <- rv$wrap_rows

    if(!is.null(rv$plot_theme)){
      selected_theme <- switch(rv$plot_theme,
                               "Default" = pmxploit::theme_pmx,
                               "Grey" = ggplot2::theme_grey,
                               "White" = ggplot2::theme_bw,
                               "Minimal" = ggplot2::theme_minimal)

      g <- g + selected_theme()

      if(rv$axis_label_rotation != 0)
        g <- g + theme(axis.text = element_text(angle = rv$axis_label_rotation, hjust = 1))

      g
    }
    list(plot = g,
         text = res$messages)
  })

  output$plot_info_text <- renderUI({
    msg <- req(zoomed_plot())$text %>% str_c(collapse = "\n")

    req(msg)

    tags$pre(msg)
  })

  output$zplot <- renderPlot({
    g <- req(zoomed_plot())$plot
    g
  })

  output$download_plot <- downloadHandler( filename = function() {
    file <- sprintf("%s-%s.%s", inner_run()$info$run_id, input$filename, input$file_type)
    file
  }, content =  function(file){
    g <-  req(zoomed_plot())$plot

    # Workaround when no graphics device is available:
    # (Error: cannot open file 'Rplots.pdf')
    # http://github.com/ropensci/plotly/494
    pdf(NULL)
    ggsave(plot = g, filename = file, units = "mm", width = input$width, height = input$height,
           dpi = 100)
  })

  output$download_ggplot <- downloadHandler( filename = function() {
    file <- sprintf("%s-%s.rds", inner_run()$info$run_id, input$filename)
    file
  }, content =  function(file){
    g <- req(zoomed_plot())$plot

    saveRDS(g, file)
  })

  observeEvent(input$customize, {
    showModal(plotModel())
  })

  observeEvent(input$title, {
    rv$plot_title <- input$title
  })

  observeEvent(input$subtitle, {
    rv$plot_subtitle <- input$subtitle
  })

  observeEvent(input$x_label, {
    rv$plot_x_label<- input$x_label
  })

  observeEvent(input$y_label, {
    rv$plot_y_label<- input$y_label
  })

  observeEvent(input$theme, {
    rv$plot_theme <- input$theme
  })

  observeEvent(input$axis_label_rotation, {
    rv$axis_label_rotation  <- input$axis_label_rotation
  })

  observeEvent(input$wrap_nrow, {
    val <- NULL

    if(input$wrap_nrow != "")
      val <- as.integer(input$wrap_nrow)

    rv$wrap_rows  <- val
  })


  observeEvent(input$reset, {
    rv$plot_title <- NULL
    rv$plot_subtitle <- NULL
    rv$plot_x_label <- NULL
    rv$plot_y_label <- NULL
    updateNumericInput(session, "axis_label_rotation", value = 0)
  })

  plotModel <- function(){
    my_plot <- req(zoomed_plot())$plot

    subtit <- my_plot$labels$subtitle

    if(!is.null(subtit))
      subtit <- ifelse(is.call(subtit), "", subtit)
    modalDialog(
      fluidRow(column(12, textInput(session$ns("title"), "Title", width = "100%", value = my_plot$labels$title))),
      fluidRow(column(12, textInput(session$ns("subtitle"), "Sub-title", width = "100%", value = subtit))),
      fluidRow(column(6, textInput(session$ns("x_label"), "X-axis label", width = "100%", value = my_plot$labels$x)),
               column(6, textInput(session$ns("y_label"), "Y-axis label", width = "100%", value = my_plot$labels$y))),
      fluidRow(column(3, selectInput(session$ns("theme"), "Theme", choices = c("Default", "Grey", "White", "Minimal"), selected = rv$plot_theme))),
      fluidRow(column(6,
                      numericInput(session$ns("axis_label_rotation"), "Axis labels rotation (°)", value = rv$axis_label_rotation))),
      br(),
      fluidRow(column(12, actionButton(session$ns("reset"), "Reset"))),
      footer = NULL,
      easyClose = TRUE
    )
  }

  # n_facets_test <- reactive({
  #   my_plot <- req(zoomed_plot())$plot
  #
  #     # browser()
  #     facet_covs <- intersect(names(my_plot$facet$params$facets), names(my_plot$data))
  #
  #     facet_cols <- syms(facet_covs)
  #
  #     # browser()
  #     req(is.data.frame(my_plot$data))
  #
  #    n_facets <- my_plot$data %>%
  #       group_by(!!!(facet_cols)) %>%
  #       summarize() %>%
  #       nrow()
  #
  #     if(n_facets == 0 || (!is.null(rv$wrap_rows) && n_facets < rv$wrap_rows))
  #       rv$wrap_rows <- NULL
  #
  #    n_facets
  # })
  #
  # output$facetting_output <- renderUI({
  #
  #   # browser()
  #   n_facets <- req(n_facets_test())
  #
  #   req(n_facets > 0)
  #
  #   isolate({
  #     # keep current selection
  #     r_selected <- rv$wrap_rows
  #
  #     possible_n_rows <- c(seq_len(n_facets / 2), n_facets)
  #
  #     fluidRow(column(6, selectInput(session$ns("wrap_nrow"), "Rows layout",
  #                                    choices = c("", possible_n_rows), selected = r_selected)))
  #   })
  # })
}
