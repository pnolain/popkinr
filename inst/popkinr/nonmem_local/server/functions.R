quiet_system2 <- quietly(system2)

get_process_wdir <- function(pid){
  wdir <- quiet_system2("pwdx", pid, stdout = TRUE)

  if(length(wdir$warnings) > 0 || length(wdir$result) == 0) return(NULL)

  wdir$result %>% str_split(":\\s*") %>% unlist() %>% last()
}

get_process_usage <- function(pid){
  usage <- quiet_system2("ps", sprintf("-o ppid,user,pcpu,cpuid,start_time,comm -p %s", pid), stdout = TRUE)

  if(length(usage$warnings) > 0 || length(usage$result) == 0) return(NULL)

  read_table(str_c(usage$result, collapse = "\n"))
}

refresh_run_check <- function(run_number, call_time, force = FALSE){
  current_jobs <- global_local_jobs

  if(is.null(current_jobs)) return(NULL)

  job <- current_jobs %>% filter(PPID == run_number)

  if(nrow(job) == 0) return(NULL)

  source_dir <- job$path

  if(!dir.exists(source_dir)) stop(simpleError(str_c("Directory not found: ", source_dir)))

  files_to_copy <- tibble(file = list.files(source_dir, full.names = TRUE, pattern = "\\.(ext|xml)$")) %>%
    mutate(name = basename(file),
           ext = tools::file_ext(name)) %>%
    filter(name != "temporaryfile.xml") %>%
    arrange(desc(file.info(file)$mtime)) %>%
    group_by(ext) %>%
    slice(1) %>%
    ungroup()

  xml_file <- str_subset(files_to_copy$file, "xml$")
  ext_file <- str_subset(files_to_copy$file, "ext$")

  if(length(xml_file) == 0 || length(ext_file) == 0) return(NULL)

  if(!file.exists(xml_file) || !file.exists(ext_file)) return(NULL)

  # control file
  xml_lines <- read_lines(xml_file)
  cs_tag_lines <- str_which(xml_lines, "</?control_stream>")
  control_file_text <- xml_lines[(cs_tag_lines[1]+1):(cs_tag_lines[2]-1)]

  cs_data <- pmxploit::parse_nm_control_stream(content = control_file_text)

  # iterations
  ext_lines <- read_lines(ext_file)

  iterations_data <- pmxploit::parse_nm_ext(content = ext_lines)

  request_duration <- now() - call_time

  list(number = run_number,
       iterations = iterations_data,
       control_stream = cs_data,
       request_duration = request_duration)
}

stop_run <- function(run_number){
  local_wd <- get_process_wdir(run_number)

  if(is.null(local_wd))
    stop(sprintf("Error reading job %s.", run_number))

  cmd_str <- sprintf("touch '%s/sig.stop'", local_wd)

  quiet_call <- quietly(~ system(cmd_str, intern = TRUE))

  cmd <- quiet_call()

  if(length(cmd$warnings) == 0){
    return(TRUE)
  } else {
    stop(sprintf("Error reading job %s.", run_number))
  }
}

next_estimation <- function(run_number){
  local_wd <- get_process_wdir(run_number)

  if(is.null(local_wd))
    stop(sprintf("Error reading job %s.", run_number))

  cmd_str <- sprintf("touch '%s/sig.next'", local_wd)

  quiet_call <- quietly(~ system(cmd_str, intern = TRUE))

  cmd <- quiet_call()

  if(length(cmd$warnings) == 0){
    return(TRUE)
  } else {
    stop(sprintf("Error reading job %s.", run_number))
  }
}

kill_run <- function(run_number){
  is_nonmem <- system2("ps", sprintf("-p %s -o comm=", run_number), stdout = TRUE)

  if(length(is_nonmem) != 1 || is_nonmem != "nonmem")
    stop(simpleError("Process is not a NONMEM job"))

  system2("kill", run_number)
}
