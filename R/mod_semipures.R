mod_semipures_ui <- function(id) {
  ns <- NS(id)
  
  bslib::nav_panel(
    "Semi-pure Extracts"
  )
  
}

mod_semipures_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}