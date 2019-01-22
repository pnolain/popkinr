library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyTree)

library(tibble)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(xml2)
library(ggplot2)

library(plotly)
library(rhandsontable)

library(PKNCA)

source("chooser.R")
useShinyjs()  # Include shinyjs


shinyUI(
  dashboardPage(

    skin = "purple",
    title = "PMxplore Dataset Exploration",

    dashboardHeader(title = "PMxplore Montpellier", titleWidth = 400),

    dashboardSidebar(
      sidebarMenu(

        menuItem("Home", tabName = "dashboard", icon = icon("home"), selected = TRUE),

        menuItem("Dataset View", tabName = "data_view", icon = icon("tv") ),

        menuItem("Dependent Variable",tabName = "DV_tab", icon = icon("gamepad")),

        menuItem("Covariates", icon = icon("pie-chart"),
                 menuSubItem("Continuous covariates", tabName = "continuous_tab", icon = icon("fighter-jet")),
                 menuSubItem("Categorical covariates", tabName = "categorical_tab", icon = icon("database"))
        ),
        menuItem("Manage variables type",tabName="manage_tab", icon = icon("cogs")),
        menuItem("Enrich the dataset",tabName = "enrich_tab", icon = icon("plus")),
        menuItem("NCA",tabName = "NCA_tab", icon = icon("signal")),
        useShinyjs(),  # Include shinyjs
        br(),
        hr(),
        div(align = "center",
            tags$img(src = "pmxplore.svg",  width = 175, height = 175)
        ),
        h6("PMxplore version: 1.1"),
        h6("Romain Combet, Programming M&S Montpellier"),
        h6("Patrick Nolain, M&S Montpellier")
      )
    ),
    dashboardBody(
      shiny::includeCSS("www/css/styles.css"),
      tabItems(
        tabItem(tabName = "dashboard",
                h2(icon("download")),
                fluidRow(theme="bootstrap.css",
                         box(
                           title = "Dataset download ", width = 12,
                           #Selector for file upload
                           # fileInput('datafile', 'Choose .CSV / .DAT dataset',
                           #           accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                           actionLink("click_browse", "Browse..."),br(),br(),
                           br(),
                           actionLink("Demo","Demo dataset",icon("edit"))
                         ),

                         box(width=12,
                             valueBoxOutput(width = 5,"check_box")),
                         box(title="Overall information:",width=12,
                             valueBoxOutput(width = 3, "subjects_box"),
                             valueBoxOutput(width = 3, "observations_box"),
                             valueBoxOutput(width = 3, "compartments_box"),
                             valueBoxOutput(width = 3, "doses_box"))



                )
        ),
        tabItem(tabName = "data_view",
                # sliderInput("PageLength","entries per page",min=10, max=50, value=30, step=10),
                DT::dataTableOutput('filetable')
        ),
        tabItem(tabName = "DV_tab",
                fluidRow(box(
                  width = 12, title = "Options",
                  radioButtons("Alldata",label="", choices=c("All subjects"=0,"Individuals"=1),selected=0, inline=T),
                  conditionalPanel("input.Alldata==1",uiOutput("Subjects")),
                  column(width=3,uiOutput("Filter_dose")),
                  column(width=3,uiOutput("Level_dose"))
                  # column(width=3,uiOutput("DV_choice"))

                )),

                fluidRow(width=12,
                         column(width=2,uiOutput("TimeVar")),
                         column(width=2,uiOutput("Split_dose"),
                                checkboxInput("FreeScale","Free Scale",F)),
                         column(width=2,uiOutput("Filter_CMT")),
                         column(width=2,radioButtons("Ytype","Y axis",choices = c("DV" = 0, "log10(DV)" = 1), inline = TRUE))),
                column(width=2,checkboxInput("Colors",tags$code(strong("Colors")),F)),
                column(width=2,a(id = "toggle_others_options", "show / hide others options", href = "#")),
                div(id="others_options",column(width=2,uiOutput("Other_filter"),
                                               conditionalPanel("input.Other_filter!=='No filter'",uiOutput("filt_Value"))),

                    column(width=2,uiOutput("Split_other")))



                ,
                fluidRow(box(
                  width = 12, title = "DV versus TIME",

                  withSpinner(plotlyOutput('DV_graph',height = "100%"))
                  # withSpinner(plotOutput('DV_graph'))
                  # , type = getOption("spinner.type", default = 4),
                  #                                          color = getOption("spinner.color", default = "#67429f"))
                )
                ),
                fluidRow(box(
                  width = 12, title = "DV statistics:",
                  tableOutput('stat_dv')
                )
                )
        ),
        tabItem(tabName = "continuous_tab",
                fluidRow(box(
                  width = 12, title = "Options",
                  radioButtons(
                    "covariates_values_type", "Covariates values", choices = c("All values" = 0, "Baseline values" = 1), inline = TRUE
                  )
                )),
                fluidRow(box(
                  width = 12, title = "Covariates Selection",
                  uiOutput("ContinuousVar")
                )),

                fluidRow(box(
                  width = 12, title = "Plot Type",
                  column(radioButtons("Plot_type","Choose plot type", choices=c("Boxplot" = 0, "Violin" = 1,"Histogram" = 2, "Density" = 3),inline=TRUE, selected=0),width = 6),
                  column(checkboxInput("Split",label = strong("Split"),FALSE,width = 4),
                         uiOutput("Groupby"),width = 5, offset=1),
                  hr(),
                  conditionalPanel("input.Plot_type==2",textInput("BinsNumber","Number of bins",value=30,width="10%"))
                )),
                fluidRow(box(
                  width = 12, title = "Plot",
                  plotlyOutput("fileboxplot",height = "80%")
                )
                ),
                fluidRow(box(
                  width = 12, title ="Descriptive Statistics",
                  tableOutput('stat_table'),
                  tableOutput('stat_table2'),
                  downloadButton("download_stats", label="export")
                )
                )
        ),
        tabItem(tabName = "categorical_tab",
                fluidRow(box(
                  width = 12, title = "Options",
                  radioButtons(
                    "covariates_values_type_cat", "Covariates values", choices = c("All values" = 0, "Baseline values" = 1), inline = TRUE
                  )
                )),
                fluidRow(box(
                  width = 6, title = "Covariates Selection",
                  uiOutput('CategoricalVar')
                ),
                box(
                  width = 6, title = "Splitting options",
                  uiOutput("SplitCatVar"),
                  uiOutput("Split2CatVar")

                )
                ),


                fluidRow(box(
                  width = 12, title = "Plot",
                  plotlyOutput("CatPlot",height = "100%")
                )),

                fluidRow(box(
                  width = 12, title = "Descriptive Statistics",

                  tableOutput('stat_cat_table'),
                  downloadButton("download_stats_cat", label="export")
                  #actionButton('Help',label='Help')
                )
                )
        ),
        tabItem(tabName = "manage_tab",

                fluidRow(box(
                  width = 4, title = h1("Computed Class of Variable"),
                  br(),h4(strong("Continuous   /    Categorical")),
                  uiOutput("choices")
                ),
                box(
                  width = 4, title = h1("Time Related Variables"),
                  br(),h4(strong("Select in right box all TIME related variables")),
                  uiOutput("Time_related")
                ),
                box(
                  width = 4, title = h1("Dose Related Variables"),
                  br(),h4(strong("Select in right box all DOSE related variables")),
                  uiOutput("Dose_related")
                )
                ),
                fluidRow(
                  box(width=4, title = strong("INFO 1: Number of modalities by variable"),dataTableOutput("modalite_tab")),
                  box(width=8, title = strong("INFO 2: List of variables changing over time"),
                      verbatimTextOutput("COT_varlist"),
                      actionButton("COT_details",label="Modalities by ID",icon =icon("list-alt", lib = "glyphicon"),
                                   style = "color: white;
                     background-color: #56739A;
                     position: relative;
                     left: 3%;
                     height: 35px;
                     width: 130px;
                     text-align:center;
                     text-indent: -2px;
                     border-radius: 6px;
                     border-width: 2px"),

                      conditionalPanel("input$switchinput==T",dataTableOutput("count_mbi")))


                )

        )
        ,
        tabItem(tabName = "enrich_tab",
                fluidRow(
                  box(width=4,title=h3("Sequences"),
                      rHandsontableOutput("hot"),br(),
                      actionButton("Addrow","Add a row",
                                   style = "color: black;
                                                 background-color: #CCD1D1;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px"),
                      actionButton("Delrow","Delete a row",
                                   style = "color: black;
                                                 background-color: #CCD1D1;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px"),br(),
                      checkboxInput("ColOption","See columns to fill",value= F),
                      uiOutput("fillcol"),
                      #conditionalPanel("input.ColOption == TRUE",uiOutput("fillcol")),
                      br(),br(),br(),
                      actionButton("Run",label="  RUN",icon =icon("play", lib = "glyphicon"),
                                   style = "color: black;
                                                 background-color: #4682B4;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px"),br(),
                      downloadButton("download1", label="Download", class = "butt1",
                                     style = "color: black;
                                                 background-color: #CCD1D1;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px")

                  ),
                  box(width=8,title=h3("New dataset preview:"),DT::dataTableOutput('richdata'))
                )),
        tabItem(tabName = "NCA_tab",
                fluidRow(
                  box(width=4,
                      uiOutput("NCA_cmt_dose_choice"),
                      uiOutput("NCA_cmt_conc_choice"),
                      uiOutput("NCA_TimeVar"),
                      numericInput("Conv.factor", "AMT/DV conversion factor", value=1, step=10),
                      h6("eg: for AMT in mg and DV in µg/L, Conversion factor=1000"),
                      radioButtons("dose.choice","Dose choice : ",choices=c("First","Last","Other"),inline = T),
                      numericInput("NCA_dosenumber","Choose dose number:",value=2,min=2,step=1),
                      uiOutput("NCA_DVchoice"),
                      # shinyjs::disabled(selectInput("interest.var","Variable of interest : ", choices=c("DV","DV2","DV3"),multiple=F)),
                      materialSwitch("MDV.delete", strong("Remove MDV=1"), status = "success", value=F),
                      # radioButtons("NCA_TimeUnit","Unit of time variable in the dataset",choices=c("Minutes","Hours","Days"), inline = T),
                      # materialSwitch("Auto_interval", "Automatic Intervals", status = "info", value=T),

                      strong("Time intervals for calculation"),br(),br(),


                      rHandsontableOutput("NCA_hot"),br(),
                      actionButton("NCA_Addrow","Add an interval",
                                   style = "color: black;
                                                 background-color: #CCD1D1;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px"),
                      actionButton("NCA_Delrow","Delete an interval",
                                   style = "color: black;
                                                 background-color: #CCD1D1;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px"),br(),
                      checkboxInput("NCA_ColOption","select NCA parameters",value= F),
                      checkboxGroupInput("NCA_parameters", label=NULL,choices=c("cmax", "tmax", "cmin",  "tlast", "auclast", "half.life","lambda.z", "aucinf.obs", "aucall","aucinf.pred","tlag", "clast.obs", "vss.obs", "vss.pred"),selected=c("cmax", "tmax", "cmin",  "tlast", "auclast", "half.life","lambda.z", "aucinf.obs","aucall","aucinf.pred"),inline = T),
                      # uiOutput("NCA_parameters"),
                      # conditionalPanel("input.Auto_interval==F",uiOutput("NCA_interval")),
                      actionButton("Run_NCA",label="  RUN NCA",icon =icon("play", lib = "glyphicon"),
                                   style = "color: black;
                                                 background-color: #4682B4;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px")
                      ,br(),
                      downloadButton("NCA_download", label="Download", class = "butt1",
                                     style = "color: black;
                                                 background-color: #CCD1D1;
                                                 position: relative;
                                                 left: 3%;
                                                 height: 35px;
                                                 width: 150px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 3px")
                      # sliderInput("PageLength","entries per page",min=10, max=50, value=30, step=10),


                  ),
                  box(width=8,
                      radioButtons("NCA_Alldata",label="", choices=c("All subjects"=0,"Individuals"=1),selected=0, inline=T),
                      uiOutput("NCA_Subjects"),
                      # select first filter column from fields vector
                      # shiny::selectInput("filter1", "Select filter column 1:",
                      #                    choices = NCA_fields),
                      materialSwitch("filter0", "Add a filter ", status = "info", value=F),
                      shiny::uiOutput("filter1"),
                      # reference a uiOutput that will offer values for first column
                      shiny::uiOutput("filter1choice"),
                      # offer a checkbox to allow user to select a second filter
                      shiny::checkboxInput("filter2req", "Add second filter?"),
                      # set further conditional panels to appear in the same fashion
                      shiny::conditionalPanel(condition = 'input.filter2req',
                                              shiny::uiOutput("filter2eval"),
                                              shiny::uiOutput("filter2choice"),
                                              shiny::checkboxInput("filter3req",
                                                                   "Add third filter?")),
                      shiny::conditionalPanel(condition = 'input.filter3req &
                                       input.filter2req',
                                              shiny::uiOutput("filter3eval"),
                                              shiny::uiOutput("filter3choice"))
                      # ,
                      # column(width=3,uiOutput("Filter_dose"))
                      # column(width=3,uiOutput("Level_dose")),
                      #  uiOutput("NCA_filter1"),
                      # conditionalPanel("input.NCA_filter1 !=='No filter'",uiOutput("filt_Value1")),
                      #  uiOutput("NCA_filter2"),
                      # conditionalPanel("input.NCA_filter2 !=='No filter'",uiOutput("filt_Value2"))


                  )


                ),

                fluidRow(tabBox(
                  title = "Results",
                  width = 12,
                  side = "right",
                  height = "1000px",

                  tabPanel("NCA results" , DT::dataTableOutput("NCAdata")),
                  tabPanel ("Data",DT::dataTableOutput("NCA_inputdata"))
                  # tabPanel("Plots")
                ))
                #tableOutput("filtered_data_view")
        )
      )
    )
  )
)
