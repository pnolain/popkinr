controlStreamUI <- function(id){
  ns <- NS(id)

  div(id = ns("submission_cs_details"),

      uiOutput(ns("problem")),

      fluidRow(
        column(3,
               h4("Compartments"),
               tableOutput(ns("compartments"))),
        column(9,
               h4("Parameters"),
               fluidRow(
                 column(6,
                        h5("THETA"),
                        tableOutput(ns("thetas"))),
                 column(6,
                        h5("OMEGA"),
                        tableOutput(ns("etas")))))),
      fluidRow(
        column(4,
               h4("Estimations"),
               tableOutput(ns("estimations"))),
        column(4,
               h4("Covariance step"),
               uiOutput(ns("covariance"))),
        column(4,
               h4("Simulation step"),
               uiOutput(ns("simulation")))),
      h4("Output tables"),
      DT::dataTableOutput(ns("tables")))


}

controlStreamUIModule <- function(input, output, session, control_file = NULL) {

  output$problem <- renderUI({
    (control_file)$problem
  })

  output$compartments <- renderTable({
    (control_file)$model_compartments
  })

  output$thetas <- renderTable({
    (control_file)$parameters$definition %>% filter(type == "theta") %>% select(id, name)
  })

  output$etas <- renderTable({
    (control_file)$parameters$definition %>% filter(type == "eta") %>% select(id, name)
  })

  output$estimations <- renderTable({
    (control_file)$estimations
  })

  output$covariance <- renderUI({
    has_cov_step <- (control_file)$code %>% str_detect("^\\$(COV|COVARIANCE)") %>% any()

    ifelse(has_cov_step, "Yes", "No")
  })

  output$simulation <- renderUI({
    has_sim_step <- (control_file)$code %>% str_detect("^\\$(SIM|SIMULATION)") %>% any()

    ifelse(has_sim_step, "Yes", "No")
  })

  output$tables <- renderDataTable({
    tabs <- (control_file)$tables

    if(!is.null(tabs)){
      df <- tabs %>%
        select(file, columns) %>%
        mutate(columns = map_chr(columns, ~str_c(., collapse = " ")))
    } else {
      df <- tibble(file = character(), columns = character())
    }

    datatable(df,
              options = list(language = list(emptyTable = "No output table")))
  })
}
