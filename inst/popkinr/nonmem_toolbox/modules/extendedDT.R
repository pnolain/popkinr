extendedDTUI <- function(id, title = NULL){
  ns <- NS(id)

  dig_name <- ns("digits")

  div(
    h4(title),
    tags$script(HTML(sprintf('$(document).on("input", "#%s", function () { Shiny.onInputChange("%s", this.value); })',
                             dig_name, dig_name, dig_name))),
    DT::dataTableOutput(ns("table")))
}

# reactive_table: either a data frame or a DT::datatable object (for custom formatting)
extendedDT <- function(input, output, session, reactive_table, filename = "table", digits = 4, buttons = TRUE, ...){

  toolbar_id <- str_c(session$ns("toolbar"))
  export_id <- str_c(session$ns("export"))
  dig_widget <- str_c(session$ns("digits"))
  buttons_top_id <- session$ns("buttons_top")

  rv <- reactiveValues(n_digits = digits)

  safe_stuff <- safely(function(){
    df <- inner_table()

    if(is(df, "datatables"))
      df <- df$x$data

    enabled <- nrow(df) > 0
  })

  observe({
    output[[export_id]] <- downloadHandler( filename = function() {
      export_id
    }, content =  function(file){
      write_excel_csv(mtcars, file, na = ".")
    })

    check <- safe_stuff()

    enabled <- ifelse(!is.null(check$error), FALSE, check$result)

    shinyjs::toggleState("button", condition = enabled)
  })

  inner_table <- reactive({
    reactive_table()#attr(reactive_table, "observable")$getValue()
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
