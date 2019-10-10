suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(shinyjs)
  library(shinyAce)
  library(shinytoastr)
  library(rhandsontable)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(hms)
  library(broom)
  library(future)
  library(pmxploit)
  library(rlang)
  library(xml2)
  library(vpc)
  library(shinycssloaders)
  library(shinyTree)
  library(jsonlite)
})

source("modules/extendedPlot.R")
source("modules/extendedDT.R")
source("modules/extendedDT_with_code.R")

# Show VPC tab only if a NONMEM executable path is available
vpc_tab <- (if(Sys.getenv("NM_EXE") != "") {
  menuItem("VPC", tabName = "vpc", icon = icon("area-chart"))
} else {
  NULL
})

app_title <- "PopkinR - NONMEM Toolbox"

smoothing_methods <- c(
  "None" = "none",
  "Linear model" = "lm",
  "Local Polynomial Regression" = "loess"
)

# Edit header bar to integrate run path
db_header <-
  dashboardHeader(title = app_title, titleWidth = 400)
db_header$children[[3]]$children[[3]] <-
  uiOutput("application_top_text")

dashboardPage(
  skin = "green",
  title = app_title,
  db_header,
  dashboardSidebar(
    sidebarMenu(
      id = "main_menu",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home"),
        selected = TRUE
      ),
      menuItem("Metadata", tabName = "metadata", icon = icon("info")),
      menuItem("Population", tabName = "population", icon = icon("globe")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("search")),
      menuItem(
        "Individuals",
        icon = icon("users"),
        menuSubItem("Parameters", tabName = "parameters_tab", icon = icon("cogs")),
        menuSubItem(
          "Covariates",
          tabName = "covariates_tab",
          icon = icon("pie-chart")
        ),
        menuSubItem(
          "Parameters vs Covariates",
          tabName = "params_vs_covs_tab",
          icon = icon("table")
        )
      ),
      menuItem(
        "Quality criteria",
        tabName = "quality_criteria",
        icon = icon("check")
      ),
      menuItem(
        "Outliers detection",
        tabName = "outliers",
        icon = icon("user-secret")
      ),
      vpc_tab,
      menuItem(
        "Compare to...",
        tabName = "comparison",
        icon = icon("sort-numeric-asc")
      ),
      menuItem(
        "Files",
        icon = icon("files-o"),
        menuSubItem(
          "Control stream",
          tabName = "control_stream_file",
          icon = icon("file-code-o")
        ),
        menuSubItem("Report", tabName = "report_file", icon = icon("file-text")),
        menuSubItem("Tables", tabName = "table_files", icon = icon("database"))
      ),
      br(),
      div(align = "center", actionButton("show_filters", "Filter data")),
      hr(),
      div(
        align = "center",
        img(
          src = "media/pmxploit.svg",
          width = 175,
          height = 175
        ),
        h6(
          sprintf(
            "PopkinR version: %s (%s)",
            packageDescription("popkinr")$Version,
            packageDescription("popkinr")$Date
          )
        ),
        h6("Patrick Nolain"),
        tags$a("patrick.nolain@sanofi.com", href="mailto:patrick.nolain@sanofi.com"),
        hr(),
        actionLink("example", "Demo")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    includeScript("www/js/sweetalert2.min.js"),
    shiny::includeCSS("www/css/sweetalert2.min.css"),
    includeScript("www/js/script.js"),
    shiny::includeCSS("www/css/styles.css"),
    useToastr(),
    div(
      id = "loading-content",
      img(
        src = "media/pmxploit_animated.svg",
        width = 200,
        height = 200
      )
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  id = "browsing_box",
                  width = 12,
                  title = "Select a run",
                  strong(actionLink("select_run", "Browse..."))
                ),
                box(
                  id = "recent_box",
                  title = "History",
                  collapsible = TRUE,
                  width = 12,
                  uiOutput("open_last_run")
                ),
                shinyjs::hidden(
                  div(id = "summary_box",
                  box(
                    title = "Summary",
                    width = 12,
                    h4("Estimation"),
                    fluidRow(
                      column(12,
                      DT::dataTableOutput("estimations_table"))
                      ),
                    h4("Run information"),
                    fluidRow(
                      column(7,
                             uiOutput("run_information")),
                      column(
                        5,
                        fluidRow(
                          valueBoxOutput(width = 6, "subjects_box"),
                          valueBoxOutput(width = 6, "observations_box")
                        ),
                        fluidRow(
                          valueBoxOutput(width = 6, "duration_box"),
                          valueBoxOutput(width = 6, "cpu_box")
                        ),
                        fluidRow(valueBoxOutput(width = 6, "nm_version_box"))
                      )
                    )
                  ))
                )
              )),
      tabItem(
        tabName = "metadata",
        title = "Metadata",
        fluidRow(
          box(
            width = 12,
            title = "Metadata",
            fluidRow(column(6,
                            tags$div(
                              tags$p(
                                "Metadata refer to details of the model allowing the user to manipulate several elements for plotting and reporting (e.g. compartments/parameters/covariates names, covariates types) by editing the following boxes."
                              ),
                              tags$p("One can also:"),
                              tags$ul(
                                tags$li(
                                  actionButton("browse_metadata", label = "Import"),
                                  "metadata from another run"
                                ),
                                tags$li(
                                  actionButton("clear_metadata", label = "Clear"),
                                  "(remove) metadata"
                                ),
                                tags$li(
                                  actionButton("reset_metadata", label = "Reset"),
                                  "recent changes in metadata to their state at loading"
                                ),
                                tags$li(
                                  actionButton("save_metadata", label = "Save"),
                                  "metadata to the run location"
                                )
                              )
                            )),
                     column(6, p()))
          ),
          box(
            width = 6,
            title = "Compartments",
            rHandsontableOutput("compartments_metadata")
          ),
          box(
            width = 6,
            title = "Independent variables",
            rHandsontableOutput("idv_metadata")
          ),
          box(
            width = 12,
            title = "Parameters",
            fluidRow(
              width = 12,
              column(
                width = 4,
                h4("Fixed effects (THETAs)"),
                rHandsontableOutput("thetas_metadata")
              ),
              column(
                width = 4,
                h4("Random effects (ETAs)"),
                rHandsontableOutput("etas_metadata")
              ),
              column(
                width = 4,
                h4("Individual parameters (POSTHOCs)"),
                rHandsontableOutput("parameters_metadata")
              )
            )
          ),
          box(
            width = 12,
            title = "Covariates",
            column(width = 6,
                   rHandsontableOutput("covariates_metadata")),
            column(
              width = 6,
              uiOutput("metadata_categorical_covariates"),
              rHandsontableOutput("selected_categorical_covariate_metadata"),
              actionButton("update_categorical_levels", "Update categorical levels")
            )
          ),
          box(
            width = 12,
            title = "Unknown columns",
            column(
              6,
              tags$p(
                "Additional column(s) present in the dataset but absent of output tables:"
              ),
              uiOutput("unknown_metadata")
            ),
            column(
              6,
              wellPanel(
                tags$label("Add column(s) selection to:"),
                actionButton("add_to_parameters", "Individual parameters"),
                actionButton("add_to_categorical_cov", "Categorical covariates"),
                actionButton("add_to_continuous_cov", "Continuous covariates"),
                actionButton("add_to_idv", "Independent variables")

              )
            )
          )
        )
      ),
      tabItem(tabName = "population",
              fluidRow(
                box(
                  width = 8,
                  title = "Estimations",
                  uiOutput("estimation_selection")
                ),
                box(
                  width = 4,
                  title = "Report",
                  radioButtons(
                    "estimation_report_format",
                    "Document format",
                    c("PDF", "HTML", "Word"),
                    inline = TRUE
                  ),
                  fluidRow(column(
                    6,
                    downloadButton("estimation_download_report", "Download report")
                  ),
                  column(
                    6, actionButton("estimation_save_report", "Save in run files")
                  ))
                ),
                tabBox(
                  width = 12,
                  tabPanel(
                    title = "Summary",
                    fluidRow(
                      valueBoxOutput(width = 4, "termination_box"),
                      valueBoxOutput(width = 4, "estimation_box"),
                      valueBoxOutput(width = 4, "ofv_box"),
                      valueBoxOutput(width = 4, "eigenratio_box"),
                      valueBoxOutput(width = 4, "correlation_box"),
                      valueBoxOutput(width = 4, "aic_box"),
                      valueBoxOutput(width = 4, "bic_box"),
                      valueBoxOutput(width = 4, "parallel_box")
                    ),
                    h4("Termination messages"),
                    verbatimTextOutput("termination_messages"),
                    h4("Termination information"),
                    verbatimTextOutput("termination_information")
                  ),
                  tabPanel(title = "Fixed effects (THETAs)",
                           extendedDT_with_code_UI("thetas_table")),
                  tabPanel(
                    title = "Random effects (OMEGA)",
                    h4("Covariances (ETA:ETA)"),
                    extendedDT_with_code_UI("omega_table"),
                    p("The coefficient of variation is given considering log-normal variability for parameters:",
                      withMathJax('$$CV(e^{\\eta})=\\sqrt{e^{\\omega^{2}}-1}$$')),
                    br(),
                    fluidRow(
                      column(
                        width = 6,
                        h4("Variance-Covariance Matrix"),
                        rHandsontableOutput("omega_matrix_table")
                      ),
                      column(
                        width = 6,
                        h4("Correlation Matrix"),
                        rHandsontableOutput("correlation_matrix_table")
                      )
                    ),
                    br(),
                    h4("ETA bars"),
                    extendedDT_with_code_UI("eta_bars_table")
                  ),
                  tabPanel(title = "Residual error terms (SIGMA)",
                           fluidRow(
                             column(
                               width = 8,
                               h4("Covariances (EPS:EPS)"),
                               extendedDT_with_code_UI("sigma_table")
                             ),
                             column(
                               width = 4,
                               h4("SIGMA matrix"),
                               rHandsontableOutput("sigma_matrix")
                             )
                           )),
                  tabPanel(
                    title = "Shrinkage",
                    h4("Random effects"),
                    extendedDT_with_code_UI("eta_shrinkage_table"),
                    br(),
                    h4("Residual error terms"),
                    extendedDT_with_code_UI("eps_shrinkage_table"),
                    br(),
                    h4("Legend"),
                    div(tags$ul(
                      tags$li(
                        "ETA shrinkage: Inter-subject shrinkage for each ETA",
                        withMathJax('$$\\eta_{shrink}=1-\\dfrac{SD(\\hat\\eta)}{\\omega}$$')
                      ),
                      tags$li(
                        "EBV shrinkage: Shrinkage based on the average empirical Bayes variance"
                      ),
                      tags$li(
                        "EPS shrinkage: Residual error shrinkage for each residual error",
                        withMathJax('$$\\epsilon_{shrink}=1-SD(IWRES)$$')
                      )
                    ))
                  ),
                  tabPanel(
                    title = "Convergence",
                    h4("Objective function"),
                    tabBox(
                      width = 12,
                      tabPanel(
                        "Population",
                        extendedPlotUI("ofv_convergence_plot", title = "ofv-convergence")
                      ),
                      tabPanel(
                        "Individuals",
                        extendedPlotUI("individual_ofv_plot", title = "individual_ofv")
                      )
                    ),
                    h4("Parameters convergence"),
                    selectInput(
                      "convergence_parameters",
                      "Population parameters",
                      choices = c(
                        "THETAs (Fixed effects)" = "theta",
                        "OMEGAs (Random effects covariances)" = "omega",
                        "SIGMAs (Residual error terms covariances)" = "sigma"
                      ),
                      selected = "theta"
                    ),
                    extendedPlotUI(
                      "pop_parameters_convergence_plot",
                      verbose = TRUE,
                      title = "parameters-convergence",
                      height = 700
                    )
                  )
                )
              )),
      tabItem(tabName = "diagnostics",
              fluidRow(
                box(width = 2,
                    title = "Compartments",
                    uiOutput("cmt")),
                box(
                  width = 10,
                  title = "Plots settings",
                  fluidRow(
                    column(2, selectInput(
                      "plots_x_scale",
                      "X-axis scale",
                      choices = c("Linear" = "linear", "Logarithmic" = "log")
                    )),
                    column(2,
                           selectInput(
                             "plots_y_scale",
                             "Y-axis scale",
                             choices = c("Linear" = "linear", "Logarithmic" = "log")
                           )),
                    column(
                      2,
                      selectInput(
                        "facet_scales",
                        "Scales",
                        choices = c(
                          "Free" = "free",
                          "Free X" = "free_x",
                          "Free Y" = "free_y",
                          "Fixed" = "fixed"
                        ),
                        selected = "free"
                      )
                    ),
                    column(
                      2,
                      conditionalPanel(
                        condition = "input.diagnostic_plots_box == 'dv_vs_pred_tab' | input.diagnostic_plots_box == 'residuals_tab'",
                        selectInput("diag_smoothing_method", "Smoothing method", choices = smoothing_methods)
                      )
                    ),
                    column(
                      2,
                      conditionalPanel(
                        condition = "input.diagnostic_plots_box != 'residuals_tab_tab'",
                        checkboxInput("diag_log_dv", "Log-transformed DV", value = FALSE)
                      ),
                      checkboxInput("diag_avoid_zero", "Exclude baseline", value = TRUE),
                      conditionalPanel(
                        condition = "input.diagnostic_plots_box != 'indiv_tab'",
                        checkboxInput("transparency", "Transparency", value = FALSE)
                      )
                    ),
                    column(
                      2,
                      conditionalPanel(
                        condition = "input.diagnostic_plots_box != 'indiv_tab'",
                        selectizeInput(
                          "diagnostic_split_by",
                          "Split by",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Select categorical covariates',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        )
                      )
                    )
                  )
                ),
                tabBox(
                  title = "Diagnostic plots",
                  width = 12,
                  id = "diagnostic_plots_box",
                  tabPanel(
                    value = "dv_vs_pred_tab",
                    title = "Dependent variables vs Predictions",
                    flowLayout(
                      uiOutput("dv_vs_pred_type"),
                      checkboxInput("stretch", label = "Stretch image", value = TRUE),
                      checkboxInput("dv_vs_pred_overlay", label = "Overlay prediction types", value = FALSE)
                    ),
                    extendedPlotUI("dv_vs_pred_plot", title = "dv-vs-pred", height = 700),
                    h4("Selected points"),
                    DT::dataTableOutput("selected_dv_vs_pred_data")
                  ),
                  tabPanel(
                    value = "spaghetti_tab",
                    title = "Spaghetti plot",
                    flowLayout(
                      uiOutput("spaghetti_idv"),
                      checkboxInput("spaghetti_split_facets", "Splits by facets", value = TRUE),
                      checkboxInput(
                        "spaghetti_split_means",
                        "Mean profiles (with respect to x-axis)",
                        value = FALSE
                      ),
                      checkboxInput("spaghetti_mdv", "Show MDV", value = TRUE),
                      uiOutput("spaghetti_individuals")
                    ),
                    extendedPlotUI("spaghetti_plot", title = "spaghetti", height = 700),
                    h4("Selected points"),
                    DT::dataTableOutput("selected_spaghetti_data")
                  ),
                  tabPanel(
                    value = "residuals_tab",
                    title = "Residuals",
                    fluidRow(
                      column(3,
                             selectInput(
                               "residuals_plot_type",
                               "Plot type",
                               choices = c(
                                 "Scatterplot" = "scatterplot",
                                 "Histogram" = "histogram",
                                 "Q-Q (Normal)" = "qq"
                               )
                             )),
                      column(6,
                             uiOutput("res_x_type"),
                             uiOutput("res_y_type")),
                      column(
                        3,
                        div(id = "res_histogram_widgets",
                            flowLayout(
                              sliderInput(
                                "residuals_hist_bins",
                                "Number of bins",
                                value = 30L,
                                min = 5L,
                                max = 50L
                              ),
                              checkboxInput(
                                "residuals_histogram_empirical_density",
                                "Empirical distribution",
                                value = TRUE
                              ),
                              checkboxInput(
                                "residuals_histogram_theoretical_distribution",
                                "Normal distribution",
                                value = TRUE
                              )
                            )),
                        div(id = "res_scatterplot_widgets",
                            fluidRow(
                              column(
                                6,
                                checkboxInput("res_absolute", "Absolute residuals", value = FALSE),
                                numericInput("res_y_limit", "Y-limit (abs)", value = NA)
                              ),
                              column(
                                6,
                                checkboxInput("show_reference_value", "Show reference threshold", value = FALSE),
                                numericInput("res_reference_value", "Threshold value", value = 2)
                              )
                            ))
                      )
                    ),
                    extendedPlotUI("residuals_plot", title = "residuals", height = 700),
                    h4("Selected points"),
                    DT::dataTableOutput("selected_residuals_data")
                  ),
                  tabPanel(
                    value = "indiv_tab",
                    title = "Individuals",
                    fluidRow(
                      column(
                        2,
                        uiOutput("indiv_idv"),
                        selectInput(
                          "individual_layout",
                          "Layout",
                          choices = c(
                            "1 individual" = 1,
                            "4 individuals" = 4,
                            "16 individuals" = 16
                          ),
                          selected = 1
                        )
                      ),
                      column(6,
                             uiOutput("individual_pages")),
                      column(
                        2,
                        uiOutput("indiv_pred_type"),
                        radioButtons(
                          "predictions_dots",
                          "Predictions display",
                          choices = c("Lines and dots" = TRUE, "Lines" = FALSE),
                          inline = TRUE
                        )
                      ),
                      column(
                        2,
                        selectizeInput(
                          "indiv_cat_cov",
                          "Group by",
                          choices = NULL,
                          multiple = FALSE,
                          options = list(
                            placeholder = 'Categorical covariate',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        ),
                        checkboxInput("show_observations", "Show observations", value = TRUE),
                        checkboxInput("show_indiv_mdv", "Show MDV", value = TRUE)
                      )
                    ),
                    extendedPlotUI(
                      "individuals_plot",
                      title = "individuals",
                      height = 700,
                      verbose = TRUE
                    ),
                    h4("Generate all subjects individual profiles"),
                    downloadButton("export_all_individual_profiles", "Export"),
                    actionButton("save_all_individual_profiles", "Save in run files")
                  )
                )
              )),
      tabItem(tabName = "parameters_tab",
              fluidRow(
                box(
                  width = 12,
                  title = "Settings",
                  fluidRow(
                    column(
                      width = 2,
                      radioButtons(
                        "parameters_values_type",
                        "Individual parameters values",
                        choices = c("First value only" = 0, "All values" = 1),
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 2,
                      selectInput(
                        "individuals_parameters_type_selection",
                        "Parameters type",
                        choices = c(
                          "Random effects (ETAs)" = "eta",
                          "Individual parameters (POSTHOCs)" = "individual"
                        )
                      )
                    ),
                    column(
                      width = 8,
                      selectizeInput(
                        "selected_parameters",
                        "Parameters selection",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Select parameters...',
                          onInitialize = I('function() { this.setValue(""); }')
                        )
                      ),
                      div(align = "center",
                          actionButton("parameters_refresh", "Refresh"))
                    )
                  )

                )
              ),
              fluidRow(tabBox(
                width = 12,
                tabPanel(
                  title = "Distributions",
                  h4("Visualization"),
                  flowLayout(
                    selectInput(
                      "parameters_distributions_plot_type",
                      "Plot type",
                      choices = c(
                        "Histogram" = "histogram",
                        "Density" = "density",
                        "Boxplot" = "boxplot",
                        "Q-Q (Normal)" = "qq"
                      )
                    ),
                    conditionalPanel(
                      condition = "input.parameters_distributions_plot_type == 'histogram'",
                      sliderInput(
                        "parameters_hist_bins",
                        "Number of bins",
                        value = 30L,
                        min = 5L,
                        max = 50L
                      )
                    ),
                    selectizeInput(
                      "parameters_distributions_split_by",
                      "Splitting options",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = 'Select categorical covariates...',
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    ),
                    div(
                      checkboxInput(
                        "overlay_parameters_distributions_splits",
                        "Overlay splits",
                        value = TRUE
                      ),
                      conditionalPanel(
                        condition = "input.parameters_distributions_plot_type == 'boxplot'",
                        checkboxInput(
                          "parameters_distributions_boxplot_facetted",
                          "Facetted boxplot",
                          value = TRUE
                        ),
                        checkboxInput(
                          "parameters_distributions_drop_unused_levels",
                          "Drop unused categorical levels",
                          value = FALSE
                        )
                      )
                    ),
                    selectInput(
                      "parameters_distributions_facet_scales",
                      "Scales",
                      choices = c("Free XY" = "free", "Free X" = "free_x")
                    ),
                    flowLayout(checkboxInput("parameters_histogram_empirical_density",
                                             "Empirical density",
                                             value = FALSE),
                               checkboxInput("parameters_histogram_theoretical_distribution",
                                             "Normal distribution",
                                             value = FALSE))
                    ),
                  extendedPlotUI(
                    "parameters_distributions_plot",
                    title = "parameters-distributions",
                    height = 700
                  ),

                  fluidRow(tabBox(
                    width = 12,
                    title = "Tables",
                    tabPanel(
                      "Summary statistics",
                      extendedDT_with_code_UI("parameters_distributions_summary")
                    ),
                    tabPanel(
                      "Individual values",
                      extendedDTUI("parameters_individual_table")
                    )
                  ))
                ),
                tabPanel(
                  title = "Correlations",
                  fluidRow(
                    column(
                      width = 6,
                      flowLayout(
                        selectInput(
                          "parameters_correlations_method",
                          "Correlation method",
                          choices = c(
                            "pearson",
                            "kendall (heatmap only)" = "kendall",
                            "spearman (heatmap only)" = "spearman"
                          )
                        ),
                        selectInput(
                          "parameters_correlations_plot_type",
                          "Plot type",
                          choices = c("Heatmap" = "heatmap", "Scatterplots" = "scatterplot")
                        ),
                        conditionalPanel(
                          condition = "input.parameters_correlations_plot_type == 'scatterplot'",
                          selectInput(
                            "parameters_correlations_smooth",
                            "Smoothing method",
                            choices = smoothing_methods
                          )
                        )
                      ),
                      extendedPlotUI(
                        "parameters_correlations_plot",
                        zoomable = FALSE,
                        verbose = TRUE,
                        title = "parameters-correlations",
                        export_width = 210,
                        export_height = 210,
                        height = 700,
                        click = "parameters_correlations_plot_click"
                      )
                    ),
                    column(width = 6,
                           DT::dataTableOutput("parameters_correlations_table"))
                  ),
                  h4("Selected correlation"),
                  fluidRow(
                    column(
                      width = 6,
                      flowLayout(
                        selectInput("p_p_correlations_smooth", "Smoothing method",
                                    choices = smoothing_methods),
                        selectizeInput(
                          "p_p_correlations_group",
                          "Highlight by",
                          choices = NULL,
                          multiple = FALSE,
                          options = list(
                            placeholder = 'Select categorical covariate',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        )
                      ),
                      extendedPlotUI("p_p_correlation_plot",
                                     title = "parameters-correlations", height = 700)
                    ),
                    column(width = 6,
                           DT::dataTableOutput("selected_p_p_points_table"))
                  )
                )
              ))),
      tabItem(tabName = "covariates_tab",
              fluidRow(
                box(
                  width = 12,
                  title = "Settings",
                  radioButtons(
                    "covariates_values_type",
                    "Individual covariates values",
                    choices = c("First value only" = 0, "All values" = 1),
                    inline = TRUE
                  )
                )
              ),
              fluidRow(tabBox(
                width = 12,
                tabPanel(
                  title = "Continuous covariates",
                  selectizeInput(
                    "selected_continuous_covariates",
                    "Continuous covariates selection",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      placeholder = 'Select covariates...',
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    width = 1000
                  ),
                  div(
                    align = "center",
                    actionButton("continuous_covariates_refresh", "Refresh")
                  ),
                  fluidRow(tabBox(
                    width = 12,
                    tabPanel(
                      title = "Distributions",
                      h4("Visualization"),
                      flowLayout(
                        selectInput(
                          "continuous_covariates_distributions_plot_type",
                          "Plot type",
                          choices = c(
                            "Histogram" = "histogram",
                            "Density" = "density",
                            "Boxplot" = "boxplot",
                            "Q-Q (Normal)" = "qq"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.continuous_covariates_distributions_plot_type == 'histogram'",
                          sliderInput(
                            "covariates_hist_bins",
                            "Number of bins",
                            value = 30L,
                            min = 5L,
                            max = 50L
                          )
                        ),
                        selectizeInput(
                          "continuous_covariates_distributions_split_by",
                          "Splitting options",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Select categorical covariates...',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        ),

                        div(
                          checkboxInput(
                            "overlay_continuous_covariates_distributions_splits",
                            "Overlay splits",
                            value = TRUE
                          )
                        ),
                        selectInput(
                          "continuous_covariates_distributions_facet_scales",
                          "Scales",
                          choices = c("Free XY" = "free", "Free X" = "free_x")
                        )
                      ),
                      extendedPlotUI(
                        "continuous_covariates_distributions_plot",
                        title = "covariates-distributions",
                        height = 700
                      ),
                      fluidRow(tabBox(
                        width = 12,
                        title = "Tables",
                        tabPanel(
                          "Summary statistics",
                          extendedDTUI("continuous_covariates_distributions_summary")
                        ),
                        tabPanel(
                          "Individual values",
                          extendedDTUI("continuous_covariates_individual_table")
                        )
                      ))
                    ),
                    tabPanel(
                      title = "Correlations",
                      fluidRow(
                        column(
                          width = 6,
                          flowLayout(
                            selectInput(
                              "covariates_correlations_method",
                              "Correlation method",
                              choices = c(
                                "pearson",
                                "kendall (heatmap only)" = "kendall",
                                "spearman (heatmap only)" = "spearman"
                              )
                            ),
                            selectInput(
                              "covariates_correlations_plot_type",
                              "Plot type",
                              choices = c("Heatmap" = "heatmap", "Scatterplots" = "scatterplot")
                            ),
                            conditionalPanel(
                              condition = "input.covariates_correlations_plot_type == 'scatterplot'",
                              selectInput(
                                "covariates_correlations_smooth",
                                "Smoothing method",
                                choices = smoothing_methods
                              )
                            )
                          ),
                          extendedPlotUI(
                            "covariates_correlations_plot",
                            zoomable = FALSE,
                            verbose = TRUE,
                            title = "continuous-covariates-correlations",
                            export_width = 210,
                            export_height = 210,
                            height = 700,
                            click = "covariates_correlations_plot_click"
                          )
                        ),
                        column(width = 6,
                               DT::dataTableOutput("covariates_correlations_table"))
                      ),
                      h4("Selected correlation"),
                      fluidRow(
                        column(
                          width = 6,
                          flowLayout(
                            selectInput("c_c_correlations_smooth", "Smoothing method", choices = smoothing_methods),
                            selectizeInput(
                              "c_c_correlations_group",
                              "Highlight by",
                              choices = NULL,
                              multiple = FALSE,
                              options = list(
                                placeholder = 'Select categorical covariate',
                                onInitialize = I('function() { this.setValue(""); }')
                              )
                            )
                          ),
                          extendedPlotUI("c_c_correlation_plot",
                                         title = "continuous-covariates-correlation", height = 700)
                        ),
                        column(width = 6,
                               DT::dataTableOutput("selected_c_c_points_table"))
                      )
                    )
                  ))
                ),
                tabPanel(
                  title = "Categorical covariates",
                  selectizeInput(
                    "selected_categorical_covariates",
                    "Categorical covariates selection",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      placeholder = 'Select covariates...',
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    width = 1000
                  ),
                  div(
                    align = "center",
                    actionButton("categorical_covariates_refresh", "Refresh")
                  ),
                  flowLayout(
                    selectInput(
                      "categorical_covariates_show_frequency",
                      "Y-axis",
                      choices = c("Count" = FALSE, "Frequency" = TRUE)
                    ),
                    selectizeInput(
                      "categorical_covariates_distributions_split_by",
                      "Splitting options",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = 'Select categorical covariates...',
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    ),
                    checkboxInput("categorical_covariates_drop", "Drop empty levels", value = FALSE),
                    checkboxInput("categorical_covariates_order", "Order values", value = TRUE),
                    selectInput(
                      "categorical_covariates_bar_adjustment",
                      "Bar adjustment",
                      choices = c("dodge", "stack", "fill")
                    )
                  ),
                  extendedPlotUI(
                    "categorical_covariates_distributions_plot",
                    title = "categorical-covariates-distributions",
                    height = 700
                  ),
                  fluidRow(tabBox(
                    width = 12,
                    title = "Tables",
                    tabPanel(
                      "Summary statistics",
                      extendedDTUI("categorical_covariates_distributions_summary")
                    ),
                    tabPanel(
                      "Individual values",
                      extendedDTUI("categorical_covariates_individual_table")
                    )
                  ))
                )
              ))),
      tabItem(tabName = "params_vs_covs_tab",
              fluidRow(
                box(
                  width = 12,
                  title = "Settings",
                  fluidRow(
                    column(
                      width = 2,
                      radioButtons(
                        "p_c_values_type",
                        "Individual parameters and covariates values",
                        choices = c("First value only" = 0, "All values" = 1),
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 2,
                      selectInput(
                        "p_c_parameters_type_selection",
                        "Parameters type",
                        choices = c(
                          "Random effects (ETAs)" = "eta",
                          "Individual parameters (POSTHOCs)" = "individual"
                        )
                      )
                    ),
                    column(
                      width = 8,
                      selectizeInput(
                        "p_c_selected_parameters",
                        "Parameters selection",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Select parameters...',
                          onInitialize = I('function() { this.setValue(""); }')
                        )
                      ),
                      conditionalPanel(
                        condition = "input.covariates_type_tab == 'continuous_tab'",
                        selectizeInput(
                          "p_c_selected_continuous_covariates",
                          "Continuous covariates selection",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Select covariates...',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.covariates_type_tab == 'categorical_tab'",
                        selectizeInput(
                          "p_c_selected_categorical_covariates",
                          "Categorical covariates selection",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Select covariates...',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        )
                      ),
                      div(
                        align = "center",
                        actionButton("parameters_vs_covariates_refresh", "Refresh")
                      )
                    )
                  )
                ),
                tabBox(
                  title = "Covariates",
                  id = "covariates_type_tab",
                  width = 12,
                  tabPanel(
                    title = "Continuous",
                    value = "continuous_tab",
                    fluidRow(
                      column(
                        width = 6,
                        flowLayout(
                          selectInput(
                            "parameters_covariates_correlations_method",
                            "Correlation method",
                            choices = c("pearson",
                                        "kendall",
                                        "spearman")
                          )
                        ),
                        extendedPlotUI(
                          "parameters_covariates_correlations_plot",
                          zoomable = FALSE,
                          verbose = TRUE,
                          title = "parameters-covariates-correlations",
                          export_width = 210,
                          export_height = 210,
                          height = 700,
                          click = "parameters_covariates_correlations_plot_click"
                        )
                      ),
                      column(
                        width = 6,
                        DT::dataTableOutput("parameters_covariates_correlations_table")
                      )
                    ),


                    h4("Selected correlation"),
                    fluidRow(
                      column(
                        width = 6,
                        flowLayout(
                          selectInput("p_c_correlations_smooth", "Smoothing method", choices = smoothing_methods),
                          selectizeInput(
                            "p_c_correlations_group",
                            "Highlight by",
                            choices = NULL,
                            multiple = FALSE,
                            options = list(
                              placeholder = 'Select categorical covariate',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
                        ),
                        extendedPlotUI("p_c_correlation_plot",
                                       title = "parameters-covariates-correlations", height = 700)
                      ),
                      column(width = 6,
                             DT::dataTableOutput("selected_p_c_points_table"))
                    )
                  ),
                  tabPanel(
                    title = "Categorical",
                    value = "categorical_tab",
                    p(
                      "Select parameters and covariates and click the \"Refresh\" button"
                    ),
                    extendedPlotUI(
                      "parameters_cat_covs_plot",
                      title = "parameters-vs-categorical-covariates",
                      height = 700
                    )
                  )
                )
              )),

      tabItem(
        tabName = "quality_criteria",
        fluidRow(
          box(
            width = 8,
            title = "Settings",
            flowLayout(
              uiOutput("qc_pred_type"),
              checkboxInput("qc_log_data", "Log-transformed data", value = FALSE),
              selectInput(
                "qc_alpha",
                "Confidence interval for tests",
                choices = c(
                  "90%" = 0.10,
                  "95%" = 0.05,
                  "99%" = 0.01
                ),
                selected = 0.05
              ),
              selectizeInput(
                "qc_split_by",
                "Split by",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = 'Select compartment or categorical covariate',
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            )
          ),
          box(
            width = 4,
            title = "Report",
            radioButtons(
              "qc_report_format",
              "Document format",
              c("PDF", "HTML", "Word"),
              inline = TRUE
            ),
            fluidRow(column(
              6, downloadButton("qc_download_report", "Download report")
            ),
            column(
              6, actionButton("qc_save_report", "Save in run files")
            ))
          ),
          tabBox(
            width = 12,
            tabPanel(title = "Standard QC",
                     fluidRow(column(9,
                                     DT::dataTableOutput("qc_standard"),
                                     h4("Bias (Mean Prediction Error)"),
                                     DT::dataTableOutput("qc_bias"),
                                     h4("Precision (Root Mean Square Error)"),
                                     DT::dataTableOutput("qc_precision")),
                              column(3,
                                     h4("Formulas"),
                                     tags$ul(
                                       tags$li("Maximal Error",
                                               withMathJax('$$ME=max(|pred_i-obs_i|)$$')),
                                       tags$li(
                                         "Absolute Average Fold Error",
                                         withMathJax(
                                           '$$AAFE=10^{\\dfrac{1}{N}{\\sum_{i=1}^N{|log(\\dfrac{pred_i}{obs_i}) |}}}$$'
                                         )
                                       ),
                                       tags$li("Mean Prediction Error",
                                               withMathJax('$$MPE=\\dfrac{1}{N}{\\sum_{i=1}^N{pred_i-obs_i}}$$')),
                                       tags$li("Mean Prediction Error (relative to the mean)",
                                               withMathJax('$$MPE(\\%)=\\dfrac{MPE}{\\overline{obs}}$$')),
                                       tags$li("Root Mean Square Error",
                                               withMathJax('$$RMSE=\\sqrt{\\dfrac{\\sum_{i=1}^N{(pred_i-obs_i)^2}}{N}}$$')),
                                       tags$li("RMSE (relative to the mean)",
                                               withMathJax('$$RMSE(\\%)=\\dfrac{RMSE}{\\overline{obs}}$$'))
                                     )))),
            tabPanel(title = "Student's t-Tests",
                     em("Observations vs Predictions; paired, two-sided"),
                     DT::dataTableOutput("qc_t_test_obs"),
                     em("Residuals vs zero; paired, two-sided"),
                     DT::dataTableOutput("qc_t_test_res")),
            tabPanel(title = "Correlation test",
                     em("Observations vs Predictions"),
                     DT::dataTableOutput("qc_corr_test")),
            tabPanel(title = "Linear regression",
                     DT::dataTableOutput("qc_lin_reg"),
                     uiOutput("run_qc_lin_reg_plot")),
            tabPanel(title = "R code",
                     shinyAce::aceEditor("qc_r_code", mode = "r", height = "100px", readOnly = TRUE))
        )
      )
      ),
      tabItem(tabName = "outliers",
              fluidRow(
                box(
                  width = 8,
                  title = "Settings",
                  flowLayout(
                    uiOutput("outliers_cmt"),
                    uiOutput("outliers_restype"),
                    checkboxInput("outliers_avoid_zero", "Exclude baseline", value = TRUE),
                    conditionalPanel(
                      condition = "input.outliers_test_tab != 'boxplot'",
                      selectInput(
                        "outliers_pvalue",
                        "p-value threshold",
                        choices = c("1%" = 0.01, "5%" = 0.05, "10%" = 0.1),
                        selected = 0.05
                      )
                    )
                  )
                ),
                box(
                  width = 4,
                  title = "Report",
                  radioButtons(
                    "outliers_report_format",
                    "Document format",
                    c("PDF", "HTML", "Word"),
                    inline = TRUE
                  ),
                  fluidRow(column(
                    6,
                    downloadButton("outliers_download_report", "Download report")
                  ),
                  column(
                    6, actionButton("outliers_save_report", "Save in run files")
                  ))

                ),
                tabBox(
                  width = 12,
                  id = "outliers_test_tab",
                  title = "Procedures",
                  tabPanel(title = "Normality check",
                           fluidRow(
                             column(
                               width  = 8,
                               h4("Q-Q (Normal) plot"),
                               div(align="center",
                                   extendedPlotUI(
                                     "outliers_qqplot",
                                     title = "outliers-qqplot",
                                     width = 500,
                                     height = 500,
                                     export_width = 210,
                                     export_height = 210
                                     )
                               )
                             ),
                             column(
                               width = 4,
                               h4("Kolmogorov-Smirnov test"),
                               h6("Note: duplicate values are considered as one unique value"),
                               DT::dataTableOutput("outliers_ks_table"),
                               br(),
                               verbatimTextOutput("outliers_ks_decision")
                             )
                           ),
                           fluidRow(column(
                             12,
                             h4("Selected residuals"),
                             DT::dataTableOutput("selected_outliers_residuals_table")
                           ))),
                  tabPanel(
                    title = "Grubb's test",
                    h4("Outliers"),
                    extendedDTUI("outliers_grubbs_table"),
                    br(),
                    verbatimTextOutput("outliers_grubbs_message"),
                    h6(
                      "Note: Grubb's test makes the assumption that data is normally distributed."
                    )
                  ),
                  tabPanel(
                    title = "Non-parametric",
                    value = "boxplot",
                    flowLayout(
                      numericInput(
                        "outliers_k_coefficient",
                        "k coefficient (default = 3)",
                        value = 3,
                        min = 0,
                        step = 0.1
                      )
                    ),
                    fluidRow(
                      column(
                        width = 6,
                        h4("Outliers"),
                        extendedDTUI("outliers_boxplot_table"),
                        br(),
                        verbatimTextOutput("outliers_boxplot_message")
                      ),
                      column(
                        width = 6,
                        h4("Box-and-whisker plot"),
                        extendedPlotUI(
                          "outliers_boxplot",
                          title = "outliers-boxplot",
                          height = 600,
                          export_width = 210,
                          export_height = 210
                        )
                      )
                    ),
                    h4("Note"),
                    div(
                      "Observations are detected as outliers if they are outside of the following interval:",
                      withMathJax(
                        "$$\\mathopen{[}F_f - k \\cdot IQR\\,;F_t + k \\cdot IQR\\mathclose{]}$$"
                      ),
                      "with Ff being the first quartile, Ft the third quartile, IQR the inter-quartile range (Ft-Ff) and k is a coefficient (length of the whiskers as multiple of IQR)"
                    )
                  )
                )
              )),
      tabItem(tabName = "vpc",
              fluidRow(
                box(
                  width = 12,
                  title = "Visual Predictive Checks",
                  tags$div(
                    "Based on the run estimation results, a control stream is generated to perform simulations for Visual Predictive Checks."
                  ),
                  br(),
                  tags$strong("The following must be performed to generate VPC:"),
                  tags$ol(
                    tags$li("Set the local paths to NONMEM and NMTRAN check executables"),
                    tags$li("Set number of simulations and seed"),
                    tags$li("Generate the control stream for simulation"),
                    tags$li(
                      "Run simulations locally using NONMEM (do not close the application until computation is finished)"
                    )
                  ),
                  br(),
                  tags$div("The VPC plots and data are then computed by the ", tags$strong("vpc"),
                           " R package (docs: ", tags$a(href = "http://vpc.ronkeizer.com/", target = "_blank", "http://vpc.ronkeizer.com/"), ")")
                  ,
                  fluidRow(column(
                    8,
                    h4("NONMEM paths"),
                    fluidRow(column(6, uiOutput("nm_exe")),
                             column(6, uiOutput("nmcheck_exe")))
                  ),
                  column(
                    4,
                    h4("Simulations settings"),
                    fluidRow(column(
                      6,
                      numericInput(
                        "vpc_n_simulations",
                        "Number of simulations",
                        value = 100,
                        min = 1,
                        width = 175
                      )
                    ),
                    column(6,numericInput("vpc_seed", "Simulations seed", value = 123456, width = 175)
                           ))
                  )),
                  tags$div(
                    align = "center",
                    actionButton("generate_control_stream", "Generate control stream")
                  ),
                  uiOutput("vpc_location")
                ),
                tabBox(
                  width = 12,
                  title = "VPC results",
                  tabPanel("Visualization",
                           fluidRow(
                             column(3,
                                    fluidRow(
                                      column(
                                        6,
                                        uiOutput("vpc_idv"),
                                        uiOutput("vpc_cmt"),
                                        checkboxInput("vpc_correction", "Prediction-correction", value = FALSE),
                                        sliderInput(
                                          "vpc_pi",
                                          "Prediction interval (PI) range",
                                          value = c(0.05, 0.95),
                                          min = 0,
                                          max = 1,
                                          step = 0.01
                                        ),
                                        sliderInput(
                                          "vpc_ci",
                                          "Confidence interval (CI) range",
                                          value = c(0.05, 0.95),
                                          min = 0,
                                          max = 1,
                                          step = 0.01
                                        ),
                                        selectInput("vpc_bin_mid", "Bin mid", choices = c("mean", "middle")),
                                        selectInput(
                                          "vpc_binning",
                                          "Binning method",
                                          choices = c(
                                            "data (default)" = "data",
                                            "time" = "time",
                                            "auto",
                                            "density",
                                            "jenks",
                                            "none",
                                            "pretty"
                                          ),
                                          selected = "data"
                                        ),
                                        numericInput("vpc_n_bins", "Number of bins", value = NA)
                                      ),
                                      column(
                                        6,
                                        tags$label("Plot:"),
                                        checkboxInput("vpc_obs_dv", "Observations", value = TRUE),
                                        checkboxInput("vpc_obs_ci", "Observations CI", value = TRUE),
                                        checkboxInput("vpc_show_pi", "Prediction interval", value = FALSE),
                                        checkboxInput("vpc_pi_as_area", "PI as area", value = FALSE),
                                        checkboxInput("vpc_pi_ci", "CI around PI", value = TRUE),
                                        checkboxInput("vpc_obs_median", "Observations median", value = TRUE),
                                        checkboxInput("vpc_sim_median", "Simulations median", value = FALSE),
                                        checkboxInput("vpc_sim_median_ci", "Simulations median CI", value = TRUE),
                                        checkboxInput("vpc_smooth", "Smooth", value = TRUE),
                                        numericInput("vpc_lloq", "Lower limit of quantification", value = NA),
                                        selectInput(
                                          "vpc_x_scale",
                                          "X-axis scale",
                                          choices = c("Linear" = "linear", "Logarithmic" = "log")
                                        ),
                                        selectInput(
                                          "vpc_y_scale",
                                          "Y-axis scale",
                                          choices = c("Linear" = "linear", "Logarithmic" = "log")
                                        ),
                                        selectInput(
                                          "vpc_facet_scale",
                                          "Facet scales",
                                          choices = c(
                                            "Free" = "free",
                                            "Free X" = "free_x",
                                            "Free Y" = "free_y",
                                            "Fixed" = "fixed"
                                          ),
                                          selected = "fixed"
                                        )
                                      )
                                    )),
                             column(
                               9,
                               fluidRow(column(2, uiOutput("vpc_strat1")), column(10, uiOutput(
                                 "vpc_strat1_levels"
                               ))),
                               fluidRow(column(2, uiOutput("vpc_strat2")), column(10, uiOutput(
                                 "vpc_strat2_levels"
                               ))),
                               extendedPlotUI("vpc_plot", title = "VPC", height = 700)
                             )
                           )),
                  tabPanel("Data",
                           fluidRow(
                             column(12,
                                    extendedDTUI("vpc_db_table", title = "VPC summary data"),
                                    extendedDTUI("vpc_obs_table", title = "Observations"),
                                    extendedDTUI("vpc_sim_table", title = "VPC Simulations"))
                           ))
                )
              )),
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  width = 12,
                  title = "Compare run to...",
                  fluidRow(
                    column(
                      2,
                      actionButton("select_runs_to_compare", "Select runs"),
                      actionButton("clear_comparison_table", "Clear selection")
                    ),
                    column(
                      10,
                      flowLayout(
                        uiOutput("comparison_estimation_n"),
                        uiOutput("comparison_estimation_status")
                      ),
                      uiOutput("comparison_estimation_method")
                    )
                  )
                ),
                div(
                  id = "comparison-content",
                  tabBox(
                    width = 12,
                    title = "Comparison",
                    tabPanel(
                      width = 12,
                      "Summary table",
                      h4("Termination status"),
                      DT::dataTableOutput("comparison_status_summary"),
                      br(),
                      h4("Detailed comparison"),
                      fluidRow(column(
                        10,
                        checkboxGroupInput(
                          "comparison_details",
                          "Details",
                          choices = c(
                            "Info" = "INFO",
                            "Summary" = "SUMMARY",
                            "THETA" = "THETA",
                            "OMEGA" = "OMEGA",
                            "SIGMA" = "SIGMA",
                            "ETABAR" = "ETABAR",
                            "SHRINKAGE" = "SHRINKAGE",
                            "Filenames" = "FILES"
                          ),
                          selected = c("SUMMARY", "THETA", "OMEGA"),
                          inline = TRUE
                        )
                      ),
                      column(
                        2,
                        downloadButton("export_comparison_table", "Export")
                      )),
                      extendedDTUI("comparison_table")
                    ),
                    tabPanel("Plots",

                             fluidRow(
                             tabBox(
                               width = 12,
                               tabPanel(
                                 "Parameters",
                                 fluidRow(
                                   column(
                                     2,
                                     selectInput(
                                       "comparison_distribution_plot",
                                       label = "Plot type",
                                       choices = c(
                                         "Histogram" = "histogram",
                                         "Density" = "density",
                                         "Boxplot" = "boxplot"
                                       )
                                     )
                                   ),
                                   column(
                                     2,
                                     radioButtons(
                                       "comparison_plots_details",
                                       "Parameters",
                                       choices = c(
                                         "THETA" = "THETA",
                                         "OMEGA" = "OMEGA",
                                         "SIGMA" = "SIGMA"
                                       ),
                                       selected = "THETA",
                                       inline = TRUE
                                     )
                                   ),
                                   column(8, uiOutput("comparison_plots_parameters"))
                                 ),
                                 extendedPlotUI("comparison_param_plot", height = 700)
                               ),
                               tabPanel("OFV",
                                        extendedPlotUI("comparison_ofv_plot", height = 700))
                             ))),
                    tabPanel(
                      "Statistics",
                      checkboxGroupInput(
                        "comparison_stats_table_detail",
                        "Details selection",
                        choices = c("THETA", "OMEGA", "SIGMA", "ETABAR", "SHRINKAGE"),
                        selected = c("THETA", "OMEGA", "SIGMA"),
                        inline = TRUE
                      ),
                      checkboxInput("comparison_stats_table_rse", "RSE", value = FALSE),
                      extendedDTUI("comparison_stats_table")
                    )
                  )
                )
              )),
      tabItem(tabName = "table_files",
              fluidRow(
                box(
                  width = 12,
                  title = "Output tables",
                  uiOutput("run_table_selection"),
                  checkboxInput("subject_first_row", "Subject first row only", value = FALSE),
                  extendedDTUI("run_table_output")
                )
              )),
      tabItem(tabName = "control_stream_file",
              fluidRow(
                tabBox(
                  width = 12,
                  title = "Control stream",
                  tabPanel(title = "Raw",
                           verbatimTextOutput("control_stream_raw_content")),
                  tabPanel(title = "PMXploit control stream object",
                           verbatimTextOutput("control_stream_parsed_content"))
                )
              )),
      tabItem(tabName = "report_file",
              fluidRow(
                box(
                  width = 12,
                  title = "Report file (NONMEM output)",

                  verbatimTextOutput("report_content")
                )
              ))
    )
  )
)
