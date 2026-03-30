# purrr::map(list.files("data", full.names = T), load, .GlobalEnv); devtools::load_all(".")

# suppressWarnings(bslib::bs_theme(base_font = bslib::font_google("Inter")))

library(dplyr)
library(magrittr)
library(tidyr)

source("R/source.R")

bs_theme <- bslib::bs_theme(
  bootswatch = "darkly",
  # base_font = bslib::font_google("Inter"),
  navbar_bg = "#1CA498"
) %>% 
  bslib::bs_add_rules(
    "
    .btn.btn-default.action-button.valuebox_btn.shiny-bound-input { 
      padding: 0px !important;
    }
    .card.bslib-card.bslib-mb-spacing.html-fill-item.html-fill-container.bslib-value-box.bg-info {
      margin: 0px !important;
    }
    "
  )

server <- function(input, output, session) {
  mod_screening_lux_server("nav1")
  mod_screening_zoi_server("nav2")
  mod_extracts_server("nav3")
  mod_semipures_server("nav4")
  mod_individual_fungi_server("individual")
}

ui <- bslib::page_navbar(
  theme = bs_theme,
  id = "nav",
  title = "Fungi Screening Dashboard",
  fillable = F,
  mod_screening_lux_ui("nav1"),
  mod_screening_zoi_ui("nav2"),
  mod_extracts_ui("nav3"),
  mod_semipures_ui("nav4"),
  mod_individual_fungi_ui("individual")
)

# # Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
ggplot2::theme_set(ggplot2::theme_bw(base_size = 16))

server <- function(input, output, session) {
  mod_screening_lux_server("nav1")
  mod_screening_zoi_server("nav2")
  mod_extracts_server("nav3")
  mod_semipures_server("nav4")
  mod_individual_fungi_server("individual")
}

shinyApp(ui = ui, server = server)