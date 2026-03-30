mod_screening_zoi_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    "Screening - ZOI", 
    bslib::card(
      full_screen = T,
      bslib::layout_columns(
        col_widths = c(4, 8),
        # Value Boxes
        bslib::card(
          uiOutput(
            outputId = ns("vbox_screened")
          ),
          uiOutput(
            outputId = ns("vbox_active")
          ),
          uiOutput(
            outputId = ns("vbox_extracted")
          )
        ),
        # Map
        bslib::card(
          
        ),
      ),
      bslib::accordion(
        bslib::accordion_panel(
          title = "Media Differences"
        ),
        bslib::accordion_panel(
          title = "Age Differences"
        ),
        bslib::accordion_panel(
          title = "Phylogenetic Differences"
        )
      )
    )
  )
}

mod_screening_zoi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add logic here
  })
}
