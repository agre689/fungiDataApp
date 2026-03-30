# UI function
valueBoxButtonUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("box")),
    # Attach JS once per module
    tags$script(HTML(sprintf("
      Shiny.addCustomMessageHandler('%s_bindClick', function(id) {
        $('#' + id).on('click', function() {
          Shiny.setInputValue(id + '_click', true, {priority: 'event'});
        });
      });
    ", ns(""))))
  )
}

# Server function
valueBoxButtonServer <- function(id, value, subtitle, icon = NULL, color = "primary") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$box <- renderUI({
      # Wrap the valueBox in a clickable <a> tag
      tags$a(
        id = ns("vb"),
        href = "#",
        bslib::valueBox(
          value = value,
          subtitle = subtitle,
          icon = icon,
          theme_color = color
        )
      )
    })
    
    # Bind click handler once UI is ready
    observe({
      session$sendCustomMessage(paste0(ns(""), "_bindClick"), ns("vb"))
    })
    
    # Expose reactive click event
    return(
      reactive({
        input$vb_click
      })
    )
  })
}