options(shiny.fullstacktrace = TRUE)

# # DEBUGGING
# plan(sequential)
plan(multiprocess)

env_home <- Sys.getenv("HOME")
env_nm_exe <- Sys.getenv("NM_EXE")
env_nm_call <- Sys.getenv("NM_CALL")
env_nmcheck_exe <- Sys.getenv("NMCHECK_EXE")
env_nmcheck_call <- Sys.getenv("NMCHECK_CALL")
env_nmtoolbox_path <- Sys.getenv("APP_PATH")
env_nmtoolbox_root <- Sys.getenv("BROWSER_ROOT")
env_pmx_startup_path <- Sys.getenv("STARTUP_PATH")

local_app_folder <- ifelse(env_nmtoolbox_path != "",
                           env_nmtoolbox_path,
                           str_c(env_home, "popkinr", "nonmem_toolbox", sep = "/"))

if(!dir.exists(local_app_folder)){
  dir.create(local_app_folder, recursive = TRUE)
}

# Global UI options
theme_set(theme_pmx())
options(DT.options = list(pageLength = 50, dom = "rti"))
options(shiny.maxRequestSize = 100*1024^2) # 100 MB max
# rlang deprecation messages
options(lifecycle_disable_verbose_retirement = TRUE)

rhandsontable <- function(...){
  rhandsontable::rhandsontable(..., fillHandle = FALSE)
}

# Styling variables
rse_warning_breaks <- c(.40, .50, .75, 1)
rse_warning_colours <- round(seq(255, 100, length.out = length(rse_warning_breaks) + 1), 0) %>%
  paste0("rgb(255,", ., ",", ., ")")

pvalue_warning_breaks <- c(0, 0.001, 0.01, 0.05, 0.1)
pvalue_warning_colours <- c(rev(rse_warning_colours), NA)

app_xml_path <- str_c(env_home, "popkinr", "popkinr.xml", sep = "/")

app_temp_directory <- str_c(local_app_folder, "tmp", sep = "/")
app_temp_comparison_directory <- str_c(local_app_folder, "comparison", sep = "/")
app_temp_vpc_directory <- str_c(local_app_folder, "vpc", sep = "/")
cleanup_when_ended <- TRUE

user_initial_selection <- "/"

# application metadata
startup_last_runs <- tibble(date = as.POSIXct(character()), path = character())
initial_run_path <- NULL

if(env_pmx_startup_path != "")
  initial_run_path <- env_pmx_startup_path

read_previous_runs <- function(){
  prev_runs <- tibble(date = as.POSIXct(character()), path = character())

  if(file.exists(app_xml_path)){
    doc <- read_xml(app_xml_path)

    run_nodes <- doc %>%
      xml_find_all("/popkinr/pmxploit/history/run")

    if(length(run_nodes) > 0){
      last_runs <- as_list(run_nodes) %>%
        map(~ list(date = lubridate::ymd_hms(attr(., "date")), path = attr(., "path"))) %>%
        bind_rows() %>%
        arrange(date)

      prev_runs <- last_runs %>%
        filter(file.exists(path))
    }
  }

  prev_runs
}

startup_last_runs <- read_previous_runs()

if(!is.null(startup_last_runs) && nrow(startup_last_runs) > 0)
  user_initial_selection <- startup_last_runs %>% slice(n()) %>% .$path %>% dirname
