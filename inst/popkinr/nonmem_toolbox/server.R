server <- function(input, output, session){

  # Include the logic (server) for each tab
  source(file.path("server", "startup.R"),  local = TRUE)$value
  source(file.path("server", "modules.R"),  local = TRUE)$value
  source(file.path("server", "main.R"),  local = TRUE)$value
  source(file.path("server", "home.R"),  local = TRUE)$value
  source(file.path("server", "metadata.R"),  local = TRUE)$value
  source(file.path("server", "population.R"),  local = TRUE)$value
  source(file.path("server", "individuals.R"),  local = TRUE)$value
  source(file.path("server", "diagnostics.R"),  local = TRUE)$value
  source(file.path("server", "diagnostics_vpc.R"),  local = TRUE)$value
  source(file.path("server", "quality_criteria.R"),  local = TRUE)$value
  source(file.path("server", "outliers.R"),  local = TRUE)$value
  source(file.path("server", "comparison.R"),  local = TRUE)$value
  source(file.path("server", "files.R"),  local = TRUE)$value

  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2.5)
}
