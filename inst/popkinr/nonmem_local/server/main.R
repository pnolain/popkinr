theme_set(pmxploit::theme_pmx())
# DEBUG
# nmr
# Sys.setenv(NM_EXE = "/usr/local/bin/nmr",
#            NM_CALL = "{nonmem_exe} {control_file} -local",
#            NMCHECK_EXE = "/usr/local/bin/nmr",
#            NMCHECK_CALL = "{nmcheck_exe} {control_file} -test"
#            RUN_RESULTS_SUBDIR = "nmnqs")
# nmfe74
# Sys.setenv(NM_EXE = "/software/nonmem/nonmem741_openleap43/run/nmfe74",
#            NMCHECK_EXE = "/software/nonmem/nonmem741_openleap43/tr/NMTRAN.exe",
#            NM_PARAFILE = "/software/nonmem/nonmem741_openleap43/runfiles/mpilinux8.pnm",
#            RUN_RESULTS_SUBDIR = "nmnqs")

temp_root <- "/usr/scratch"

MAX_CPU_LOAD <- as.numeric(Sys.getenv("NM_MAX_CPU_LOAD"))
MAX_SIMULTANEOUS_RUNS <- as.integer(Sys.getenv("NM_MAX_RUNS"))

if(is.na(MAX_CPU_LOAD))
  MAX_CPU_LOAD <- 0.5 # default

if(is.na(MAX_SIMULTANEOUS_RUNS))
  MAX_SIMULTANEOUS_RUNS <- availableCores() * MAX_CPU_LOAD # default

env_home <- Sys.getenv("HOME")

env_nm_exe <- Sys.getenv("NM_EXE")
env_nm_call <- Sys.getenv("NM_CALL")
env_nmcheck_exe <- Sys.getenv("NMCHECK_EXE")
env_nmcheck_call <- Sys.getenv("NMCHECK_CALL")

shinyjs::toggleState("perform_nmcheck", condition = file.exists(env_nmcheck_exe))

env_nm_parafile_path <- Sys.getenv("NM_PARAFILE")
env_popkin_monitor_path <- Sys.getenv("APP_PATH")
env_popkin_root <- ifelse(Sys.getenv("BROWSER_ROOT") != "", Sys.getenv("BROWSER_ROOT"), "/")

local_app_folder <- ifelse(env_popkin_monitor_path != "",
                           env_popkin_monitor_path ,
                           str_c(env_home, "popkinr", "nonmem_local", sep = "/"))

env_execution_dir <- str_c(local_app_folder, "runs", sep = "/")

if(!dir.exists(local_app_folder)){
  dir.create(local_app_folder, recursive = TRUE)
}

app_temp_directory <- str_c(local_app_folder, "tmp", sep = "/")
app_xml_path <- str_c(env_home, "popkinr", "popkinr.xml", sep = "/")

if(!dir.exists(app_temp_directory))
  dir.create(app_temp_directory, recursive = TRUE)

sys_info <- Sys.info() %>% as.list() %>% tbl_df()

initial_control_file_selection_folder <- env_popkin_root
initial_evaluation_run_selection_folder <- env_popkin_root
initial_misc_run_selection_folder <- env_popkin_root

if(file.exists(app_xml_path)){
  xml_data <- read_xml(app_xml_path)
  run_folder <- xml_text(xml_find_first(xml_data, "//pmxecute/folders/run"))
  evaluation_folder <- xml_text(xml_find_first(xml_data, "//pmxecute/folders/evaluation"))
  misc_folder <- xml_text(xml_find_first(xml_data, "//pmxecute/folders/misc"))

  if(dir.exists(run_folder))
    initial_control_file_selection_folder <- run_folder

  if(dir.exists(evaluation_folder))
    initial_evaluation_run_selection_folder <- evaluation_folder

  if(dir.exists(misc_folder))
    initial_misc_run_selection_folder <- misc_folder
}

is_nm_run_folder <- function(dir_path){
  if(is.null(dir_path) || !dir.exists(dir_path)) return(FALSE)
  dir_files <- list.files(dir_path, ignore.case = TRUE)
  all(c("xml", "ext") %in% tools::file_ext(dir_files))
}

run_browser_formatting <- function(id, text){
  # `id` is the path of the tree node
  is_nm_run <- is_nm_run_folder(id)
  if(is_nm_run) return(sprintf("<span style='color:red;'>%s</span>", text))

  has_archives <- (length(list.files(id, pattern = "\\.tar\\.gz$", ignore.case = TRUE)) > 0)
  if(has_archives) return(sprintf("<strong>%s</strong>", text))

  text
}

control_file_browser_formatting <- function(id, text){
  has_cs <- (length(list.files(id, pattern = "\\.(con|ctl|mod)$", ignore.case = TRUE)) > 0)
  if(has_cs) return(sprintf("<strong>%s</strong>", text))

  text
}

browsing_root <- ifelse(env_popkin_root != "", env_popkin_root, "/")

startup_last_runs <- tibble(date = as.POSIXct(character()), path = character())

if(file.exists(app_xml_path)){

  run_nodes <- xml_data %>%
    xml_find_all("/popkinr/pmxploit/history/run")

  if(length(run_nodes) > 0){
    last_runs <- as_list(run_nodes) %>%
      map(~ list(date = lubridate::ymd_hms(attr(., "date")), path = attr(., "path"))) %>%
      bind_rows() %>%
      arrange(date)

    startup_last_runs <- last_runs %>%
      filter(file.exists(path))
  }
}

rv <- reactiveValues(run_number = NULL,
                     run_queue = Queue$new(name = "Run queue", type = NULL),
                     future = Queue$new(name = "NONMEM execution", type = "popup"),
                     previous_runs = startup_last_runs,
                     control_files = NULL,
                     selected_cs_id = NULL,
                     evaluation_run_path = NULL,
                     misc_run_path = NULL,
                     temp_run_details = NULL)

control_file_browser <- callModule(popkinr::serverBrowser, "control_file_browser",
                           root_directory = browsing_root,
                           initial_selection = initial_control_file_selection_folder,
                           file_extensions = c("con", "ctl", "mod"),
                           folder_shortcuts = reactive({
                             if(nrow(rv$previous_runs) > 0) return(dirname(rv$previous_runs$path))
                           }),
                           formatting_function = control_file_browser_formatting)

misc_run_browser <- callModule(popkinr::serverBrowser,
                       "misc_run_browser",
                       root_directory = browsing_root,
                       initial_selection = initial_misc_run_selection_folder,
                       file_extensions = "tar.gz",
                       formatting_function = run_browser_formatting)

evaluation_run_browser <- callModule(popkinr::serverBrowser,
                             "evaluation_run_browser",
                             root_directory = browsing_root,
                             initial_selection = initial_evaluation_run_selection_folder,
                             file_extensions = "tar.gz",
                             formatting_function = run_browser_formatting)


makeReactiveTrigger <- function() {
  tmp_rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      tmp_rv$a
      invisible()
    },
    trigger = function() {
      tmp_rv$a <- isolate(tmp_rv$a + 1)
    }
  )
}

output$nodes <- renderUI({
  selectInput("nodes", "Nodes", choices = seq_len(req(input$max_cpu)))
})

output$max_cpu <- renderUI({
  sliderInput("max_cpu",
              "Maximum CPU available for NONMEM",
              value = floor(availableCores() * MAX_CPU_LOAD),
              min = 0, max = availableCores(), step = 1)
})

output$max_runs <- renderUI({
  max_r <- req(input$max_cpu)

  sliderInput("max_runs",
              "Maximum simultaneous NONMEM runs",
              value = min(#MAX_SIMULTANEOUS_RUNS,
                          max_r, input$max_runs),
              min = 0, max = max_r, step = 1)
})


output$usage_stats <- renderUI({
  current_jobs <- local_nonmem_jobs()

  n_runs <- ifelse(!is.null(current_jobs), nrow(current_jobs), 0L)
  cpu <- ifelse(n_runs > 0, sum(current_jobs$n_cpu), 0L)

  tags$div(tags$strong("Current runs: "),
           n_runs,
           tags$br(),
           tags$strong("CPU usage: "),
           sprintf("%s (%s %%)", cpu, round(cpu / availableCores() * 100)))
})

output$jacknife_n <- renderText({
  run <- req(evaluation_run())

  n_ids <- run$info$number_of_subjects

  sprintf("%s runs will be generated", n_ids)
})

observe({
  shinyjs::toggle("chain_content", condition = input$evaluation_method == "chain")
  shinyjs::toggle("bootstrap_content", condition = input$evaluation_method == "bootstrap")
  shinyjs::toggle("jacknife_content", condition = input$evaluation_method == "jacknife")

  shinyjs::toggle("simulation_content", condition = input$misc_method == "simulation")
  shinyjs::toggle("prior_content", condition = input$misc_method == "prior")

  shinyjs::toggle("download_generated_cs", condition = !is.null(rv$misc_run_path))
})
