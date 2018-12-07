server <- function(input, output, session){
  options(shiny.deprecation.messages = FALSE)
  # options("shiny.fullstacktrace"=TRUE)

  session$onSessionEnded(function() {
    isolate({
      root_node <- xml_new_root("popkinr")

      new_pmxecute_node <- root_node %>%
        xml_add_child("pmxecute")

      new_pmxecute_node %>%
        xml_add_child("folders") %>%
        xml_add_child("run", control_file_browser()$folder) %>%
        xml_add_sibling("evaluation", evaluation_run_browser()$folder) %>%
        xml_add_sibling("misc", misc_run_browser()$folder)

      if(file.exists(app_xml_path)){
        doc <- read_xml(app_xml_path)

        pmxecute_node <- doc %>% xml_find_first("/popkinr/pmxecute")

        if(!is.na(pmxecute_node)){
          xml_replace(pmxecute_node, new_pmxecute_node)

          write_xml(doc, app_xml_path)
        } else {
          popkinr_node <-  doc %>% xml_find_first("//popkinr")

          if(!is.na(popkinr_node)){
            popkinr_node %>%
              xml_add_child(new_pmxecute_node)

            write_xml(doc, app_xml_path)
          } else {
            write_xml(root_node, app_xml_path)
          }
        }
      } else {
        write_xml(root_node, app_xml_path)
      }
     })

    if(dir.exists(app_temp_directory))
      unlink(app_temp_directory, recursive = TRUE)
  })

  source(file.path("server", "functions.R"),  local = TRUE)$value
  source(file.path("server", "queue.R"),  local = TRUE)$value
  source(file.path("server", "main.R"),  local = TRUE)$value
  source(file.path("server", "watcher.R"),  local = TRUE)$value
  source(file.path("server", "monitoring.R"),  local = TRUE)$value
  source(file.path("server", "execution.R"),  local = TRUE)$value
  source(file.path("server", "evaluation.R"),  local = TRUE)$value
  source(file.path("server", "misc.R"),  local = TRUE)$value

  options(DT.options = list(searching = F))

  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2.5)
}
