library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(R6)
library(shinytoastr)
library(shinyAce)

library(ggplot2)
library(tidyr)
library(dplyr)
library(xml2)
library(stringr)
library(purrr)
library(readr)
library(lubridate)
library(hms)

library(pmxploit)
library(pmxecute)

library(future)
library(promises)
#plan(multicore) # forked R processes
#plan(multisession) # background R sessions
plan(multiprocess) # multicore if supported, otherwise multisession
# plan(sequential)

source("modules/controlStreamUI.R")
# source("modules/serverbrowser.R")

app_title <- "PopkinR - NONMEM Monitor"

dashboardPage(
  skin = "red",
  title = app_title,
  dashboardHeader(title = app_title, titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Run", icon = icon("play-circle"), tabName = "run"),
      menuItem("Monitoring", icon = icon("search"), tabName = "monitor"),
      menuItem("Misc", icon = icon("code"), tabName = "misc"),
      menuItem("About", icon = icon("question-circle"), tabName = "about")
    ),
    hr(),
    tags$label("CPU Settings"),
    uiOutput("max_cpu"),
    uiOutput("max_runs"),
    tags$label("Current use"),
    uiOutput("usage_stats"),
    hr(),
    div(
      align = "center",
      img(
        src = "media/pmxecute.svg",
        width = 175,
        height = 175
      ),
      h6(
        sprintf(
          "PMXecute version: %s (%s)",
          packageDescription("pmxecute")$Version,
          packageDescription("pmxecute")$Date
        )
      ),
      h6("Patrick Nolain, M&S Montpellier")
    )
  ),
  dashboardBody(
    includeScript("www/js/script.js"),
    shiny::includeCSS("www/css/styles.css"),
    includeScript("www/js/sweetalert2.min.js"),
    shiny::includeCSS("www/css/sweetalert2.min.css"),

    useShinyjs(),
    useToastr(),
    div(
      id = "loading-content",
      img(
        src = "media/pmxecute.svg",
        width = 175,
        height = 175
      ),
      br(),
      br(),
      img(src = "media/balls.svg")
    ),
    tabItems(
      tabItem(tabName = "monitor",
              fluidRow(
                box(
                  title = "NONMEM activity",
                  width = 12,
                  flowLayout(actionButton("refresh", "Refresh"),
                             HTML('<button id="kill_all" type="button" class="btn btn-default action-button" onclick=\"kill_all_runs()">Kill all</button>')
                  ),
                  h3("Running"),
                  dataTableOutput("local_run_list"),
                  h3("Queue"),
                  dataTableOutput("queued_list")
                ))),
      tabItem(tabName = "run",
              fluidRow(
                tabBox(id = "run_box",
                       width = 12,
                       tabPanel(title = "Run",
                                value = "run",
                                actionLink("browse_control_file", "Select control file(s)..."),
                                dataTableOutput("control_files_table"),
                                actionLink("clear_control_file_selection", "Clear list")
                       ),
                       tabPanel(title = "Evaluation",
                                value = "evaluation",
                                actionLink("browse_evaluation_run", "Select a run..."),
                                verbatimTextOutput("evaluation_run_info"),
                                radioButtons("evaluation_method", "Method",
                                             choices = c("Initial values" = "chain",
                                                         "Bootstrap" = "bootstrap",
                                                         "Jacknife" = "jacknife"),
                                             selected = "chain",
                                             inline = TRUE),
                                hidden(div(id = "chain_content",
                                           flowLayout(
                                             selectInput("chain_ctype", "Initial values sampling (CTYPE)",
                                                         choices = set_names(0:2,
                                                                             c("0: Uniform distribution between lower and upper thetas",
                                                                               "1: Uniform distribution between +/- IACCEPT * THETA",
                                                                               "2: Multivariate normal distribution")),
                                                         selected = 2),
                                             numericInput("chain_iaccept", "IACCEPT", value = 0.5, step = 0.05),
                                             numericInput("chain_nsample", "Samples", value = 25, min = 1),
                                             numericInput("chain_seed", "Seed", value = 1000),
                                             uiOutput("chain_df")))
                                ),
                                hidden(div(id = "bootstrap_content",
                                           flowLayout(numericInput("bootstrap_nsample", "Samples", value = 25, min = 1),
                                                      numericInput("bootstrap_seed", "Seed", value = 1000)))),
                                hidden(div(id = "jacknife_content",
                                           verbatimTextOutput("jacknife_n"))))),
                box(id = "start_box",
                    width = 12,
                    flowLayout(
                      uiOutput("nodes"),
                      uiOutput("results_subdir_name"),
                      checkboxInput("compress", "Compress results to a *.tar.gz archive", value = TRUE),
                      checkboxInput("cleanup", "Remove NONMEM temporary files", value = TRUE)),
                    div(align = "center",
                        actionButton("perform_nmcheck", "Perform NMTRAN check"),
                        actionButton("start_run", "Start"))
                ))),
      tabItem(tabName = "misc",
              fluidRow(
                box(title = "Control stream generation",
                    value = "misc",
                    width = 12,
                    actionLink("browse_misc_run", "Select a run..."),
                    verbatimTextOutput("misc_run_info"),
                    radioButtons("misc_method", "Generate control file for",
                                 choices = c("Prior-based analysis" = "prior",
                                             "Simulations" = "simulation"), inline = TRUE),
                    hidden(div(id = "prior_content",
                               # Custom HTML radio button, in order to integrate MathJax (impossible with `radioButtons` choices)
                               HTML('<div id="prior_df_formula" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline shinyjs-resettable shiny-bound-input" data-shinyjs-resettable-id="prior_df_formula" data-shinyjs-resettable-type="RadioButtons" data-shinyjs-resettable-value="0">
                      <label class="control-label" for="prior_df_formula">Degrees of freedom for OMEGA and SIGMA</label>
                      <div class="shiny-options-group">
                        <div class="radio-inline">
                          <label>
                            <input type="radio" name="prior_df_formula" value="0" checked="checked">'),
                               withMathJax('$$DF=2\\times\\left[\\frac{\\Omega}{SE(\\Omega)}\\right]^2$$'),
                               HTML('</label>
                        </div>
                        <div class="radio-inline">
                          <label>
                            <input type="radio" name="prior_df_formula" value="1">'),
                               withMathJax('$$DF=2\\times\\left[\\frac{\\Omega}{SE(\\Omega)}\\right]^2+1$$'),
                               HTML('</label>
                        </div>
                      </div>
                    </div>')
                    )),
                    hidden(div(id = "simulation_content",
                               flowLayout(
                                 numericInput("simulation_n_sample", "N simulations", value = 100, min = 1),
                                 numericInput("simulation_seed", "Seed", value = 100, min = 1)))),
                    shinyAce::aceEditor("misc_control_file_editor",
                                        debounce = 100,
                                        height = "400px"),
                    div(align = "center",
                        downloadButton("download_generated_cs", "Download"))))),
      tabItem(tabName = "about",
              fluidRow("NONMEM Monitor"))
    )
  )
)
