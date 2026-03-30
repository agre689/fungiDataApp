mod_extracts_ui <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    "Extracts",
    bslib::card(
      full_screen = TRUE,
      bslib::accordion(
        bslib::accordion_panel(
          "Dynamic Plot",
          bslib::card(
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shinyWidgets::prettySwitch(
                  inputId = ns("switch_plot_filter"),
                  label = "Filter out non-24WP Extracts",
                  value = T
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_plot_type"),
                  label = "Plot Type",
                  choices = c(
                    "Box Plot" = "boxplot"
                  ),
                  selected = "boxplot"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_plot_x"),
                  label = "Plot X Variable",
                  choices = c(
                    # "Extract Fraction" = "extract_fraction",
                    "Isolate Growth Medium" = "medium",
                    "Isolate Age Group" = "age_grown_group",
                    "Organism Tested" = "organism"
                  ),
                  selected = "medium"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_plot_y"),
                  label = "Plot Y Variable",
                  choices = c(
                    # "MIC of All Fractions" = "mic",
                    "Lowest MIC from All Fractions" = "lowest_mic"
                  ),
                  selected = "lowest_mic"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_plot_colour"),
                  label = "Plot Colour Variable",
                  choices = c(
                    # "Extract Fraction" = "extract_fraction",
                    "Isolate Growth Medium" = "medium",
                    "Isolate Age Group" = "age_grown_group",
                    "Organism Tested" = "organism"
                    # "None" = NA
                  ),
                  selected = "medium"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_plot_facet"),
                  label = "Plot Facet Variable",
                  choices = c(
                    # "Extract Fraction" = "extract_fraction",
                    "Isolate Growth Medium" = "medium",
                    "Isolate Age Group" = "age_grown_group",
                    "Organism Tested" = "organism"
                    # "None" = NA
                  ),
                  selected = "organism"
                )
              ),
              bslib::card(
                plotOutput(outputId = ns("plot_extracts"))
              )
            )
          )
        ),
        bslib::accordion_panel(
          "Phylogeny Summary"
          # bslib::card(
          #   bslib::layout_sidebar(
          #     bslib::sidebar("tree options"),
          #     plotOutput(outputId = ns("plot_tree"))
          #   )
          # )
        ),
        bslib::accordion_panel(
          "Media Summary",
          bslib::card(
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shinyWidgets::pickerInput(
                  inputId = ns("picker_medium_plot"),
                  label = "Plot Type:",
                  choices = c("Boxplot" = "box", "Tile" = "tile", "Jitter" = "jitter"),
                  selected = "box"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_medium_tile_colour_var"),
                  label = "Tile Colour Variable",
                  choices = c("Raw Count" = "n", "Percent" = "percent"),
                  selected = "n"
                )
              ),
              bslib::layout_columns(
                col_widths = c(4, 8),
                plotly::plotlyOutput(outputId = ns("plotly_pie_media")),
                plotOutput(outputId = ns("plot_medium"))
              )
            )
          )
        ),
        bslib::accordion_panel(
          "Age Summary",
          bslib::card(
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shinyWidgets::pickerInput(
                  inputId = ns("picker_age_plot"),
                  label = "Plot Type:",
                  choices = c("Boxplot" = "box", "Tile" = "tile", "Jitter" = "jitter"),
                  selected = "box"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_age_tile_colour_var"),
                  label = "Tile Colour Variable",
                  choices = c("Raw Count" = "n", "Percent" = "percent"),
                  selected = "n"
                )
              ),
              bslib::layout_columns(
                col_widths = c(4, 8),
                plotly::plotlyOutput(outputId = ns("plotly_pie_age")),
                plotOutput(outputId = ns("plot_age"))
              )
            )
          )
        ),
        bslib::accordion_panel(
          "Summary Table",
          reactable::reactableOutput(outputId = ns("reactable_extracts"))
        )
      )
    )
  )
}



mod_extracts_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot_data <- left_join(
      expt_summaries$summary_extracts,
      expt_summaries$age_data,
      by = "icmp_isolate"
    ) %>% 
      filter(organism %in% c("EC", "SA"))
    
    observe({
      output$plot_extracts <- renderPlot({
        plot <- plot_data %>% 
          {
            if(input$switch_plot_filter) {
              filter(., extract %in% expt_metadata$relevant_extracts)
            } else .
          } %>% 
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[[input$picker_plot_x]],
              y = as.numeric(.data[[input$picker_plot_y]])
            )
          )
        
        if(input$picker_plot_type == "boxplot") {
          plot <- plot +
            ggplot2::geom_boxplot(
              ggplot2::aes(
                fill = .data[[input$picker_plot_colour]]
              )
            ) +
            ggplot2::scale_y_continuous(
              breaks = 2:9,
              labels = levels(plot_data[["mic"]])[2:9]
            )
        }
        
        if(input$picker_plot_facet != "NA") {
          plot <- plot +
            ggplot2::facet_grid(. ~ .data[[input$picker_plot_facet]])
        }
        
        plot
      })
    })
    
    
    # output$plot_tree <- renderPlot({
    # 
    # })

    output$plotly_pie_media <- plotly::renderPlotly({
      expt_summaries$summary_extracts %>%
        filter(extract %in% expt_metadata$relevant_extracts) %>%
        select(-(CRUDE:`5`)) %>%
        tidyr::pivot_wider(names_from = organism, values_from = lowest_mic) %>%
        group_by(medium) %>%
        count() %>%
        plotly::plot_ly() %>%
        plotly::add_pie(
          values = ~n,
          labels = ~medium
        )
    })

    output$plot_medium <- renderPlot({
      plot_data <- expt_summaries$summary_extracts %>%
        filter(
          extract %in% expt_metadata$relevant_extracts,
          organism %in% c("EC", "SA")
        ) %>%
        select(-lowest_mic) %>%
        tidyr::pivot_longer(cols = CRUDE:`5`, names_to = "extract_fraction", values_to = "mic") %>%
        mutate(
          extract_fraction = factor(
            extract_fraction,
            levels = c("CRUDE", as.character(1:5)),
            ordered = T
          )
        )

      if(input$picker_medium_plot == "jitter") {
        plot_data %>%
          filter(!is.na(mic)) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["extract_fraction"]],
              y = .data[["mic"]]
            )
          ) +
          ggplot2::geom_jitter() +
          ggplot2::facet_grid(organism~medium)
      } else if(input$picker_medium_plot == "box") {
        plot_data %>%
          filter(!is.na(mic)) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["extract_fraction"]],
              y = as.numeric(.data[["mic"]])
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(organism~medium)
      } else if(input$picker_medium_plot == "tile") {
        n_extracts <- plot_data %>%
          group_by(extract_fraction, medium, organism) %>%
          summarise(n_total = n())
        plot_data %>%
          group_by(extract_fraction, medium, organism) %>%
          count(mic) %>%
          ungroup() %>%
          tidyr::complete(extract_fraction, medium, organism, mic, fill = list(n = 0)) %>%
          left_join(n_extracts, by = colnames(n_extracts)[1:3]) %>%
          filter(!(mic %in% c(NA, "0"))) %>%
          mutate(percent = (n/n_total) * 100) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["extract_fraction"]],
              y = .data[["mic"]],
              fill = .data[[input$picker_medium_tile_colour_var]]
            )
          ) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::geom_tile() +
          ggplot2::facet_grid(organism~medium)
      }
    })

    # Age Plots

    output$plotly_pie_age <- plotly::renderPlotly({
      expt_summaries$summary_extracts %>%
        filter(extract %in% expt_metadata$relevant_extracts) %>%
        select(-(CRUDE:`5`)) %>%
        tidyr::pivot_wider(names_from = organism, values_from = lowest_mic) %>%
        left_join(expt_summaries$age_data, by = "icmp_isolate") %>%
        group_by(age_grown_group) %>%
        count() %>%
        plotly::plot_ly() %>%
        plotly::add_pie(
          values = ~n,
          labels = ~age_grown_group
        )
    })

    output$plot_age <- renderPlot({
      plot_data <- expt_summaries$summary_extracts %>%
        filter(
          extract %in% expt_metadata$relevant_extracts,
          organism %in% c("EC", "SA")
        ) %>%
        select(-lowest_mic) %>%
        tidyr::pivot_longer(cols = CRUDE:`5`, names_to = "extract_fraction", values_to = "mic") %>%
        left_join(expt_summaries$age_data, by = "icmp_isolate") %>%
        mutate(
          extract_fraction = factor(
            extract_fraction,
            levels = c("CRUDE", as.character(1:5)),
            ordered = T
          )
        )

      if(input$picker_age_plot == "jitter") {
        plot_data %>%
          filter(!is.na(mic)) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["extract_fraction"]],
              y = .data[["mic"]]
            )
          ) +
          ggplot2::geom_jitter() +
          ggplot2::facet_grid(organism~age_grown_group)
      } else if(input$picker_age_plot == "box") {
        plot_data %>%
          filter(!is.na(mic)) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["extract_fraction"]],
              y = as.numeric(.data[["mic"]])
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::scale_y_continuous(
            breaks = 2:9,
            labels = levels(plot_data[["mic"]])[2:9]
          ) +
          ggplot2::labs(
            x = "Extract Fraction",
            y = "MIC (\U00B5g/mL)"
          ) +
          ggplot2::facet_grid(organism~age_grown_group)
      } else if(input$picker_age_plot == "tile") {
        n_extracts <- plot_data %>%
          group_by(extract_fraction, age_grown_group, organism) %>%
          summarise(n_total = n())

        tile_colour <- input$picker_age_tile_colour_var
        print(input$picker_age_tile_colour_var)
        plot_data %>%
          group_by(extract_fraction, age_grown_group, organism) %>%
          count(mic) %>%
          ungroup() %>%
          tidyr::complete(extract_fraction, age_grown_group, organism, mic, fill = list(n = 0)) %>%
          left_join(n_extracts, by = colnames(n_extracts)[1:3]) %>%
          filter(!(mic %in% c(NA, "0"))) %>%
          mutate(percent = (n/n_total) * 100) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["extract_fraction"]],
              y = .data[["mic"]],
              fill = .data[[tile_colour]]
            )
          ) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::geom_tile() +
          ggplot2::facet_grid(organism~age_grown_group)
      }
    })

    # Reactable Output

    output$reactable_extracts <- reactable::renderReactable({
      table_data <- expt_summaries$summary_extracts %>%
        # filter(organism %in% c("EC", "SA")) %>%
        mutate(
          across(
            where(is.factor),
            as.numeric,
            .names = "index_{.col}"
          ),
          across(
            where(is.factor),
            as.character,
            .names = "label_{.col}"
          )
        )

      reactable::reactable(
        table_data,
        filterable = T,
        searchable = T,
        columns = list(
          index_CRUDE = reactable::colDef(
            defaultSortOrder = "desc",
            name = "CRUDE",
            cell = function(value, index) {
              table_data$label_CRUDE[index]
            },
            sortNALast = T
          ),
          index_1 = reactable::colDef(
            defaultSortOrder = "desc",
            name = "F1",
            cell = function(value, index) {
              table_data$label_1[index]
            },
            sortNALast = T
          ),
          index_2 = reactable::colDef(
            defaultSortOrder = "desc",
            name = "F2",
            cell = function(value, index) {
              table_data$label_2[index]
            },
            sortNALast = T
          ),
          index_3 = reactable::colDef(
            defaultSortOrder = "desc",
            name = "F3",
            cell = function(value, index) {
              table_data$label_3[index]
            },
            sortNALast = T
          ),
          index_4 = reactable::colDef(
            defaultSortOrder = "desc",
            name = "F4",
            cell = function(value, index) {
              table_data$label_4[index]
            },
            sortNALast = T
          ),
          index_5 = reactable::colDef(
            defaultSortOrder = "desc",
            name = "F5",
            cell = function(value, index) {
              table_data$label_5[index]
            },
            sortNALast = T
          ),
          index_lowest_mic = reactable::colDef(
            defaultSortOrder = "desc",
            name = "Lowest MIC",
            cell = function(value, index) {
              table_data$label_lowest_mic[index]
            },
            sortNALast = T
          ),
          CRUDE = reactable::colDef(show = F),
          label_CRUDE = reactable::colDef(show = F),
          `1` = reactable::colDef(show = F),
          label_1 = reactable::colDef(show = F),
          `2` = reactable::colDef(show = F),
          label_2 = reactable::colDef(show = F),
          `3` = reactable::colDef(show = F),
          label_3 = reactable::colDef(show = F),
          `4` = reactable::colDef(show = F),
          label_4 = reactable::colDef(show = F),
          `5` = reactable::colDef(show = F),
          label_lowest_mic = reactable::colDef(show = F),
          lowest_mic = reactable::colDef(show = F),
          label_5 = reactable::colDef(show = F)
        ),
        theme = reactable::reactableTheme(
          backgroundColor = "#222222",
          searchInputStyle = list(
            backgroundColor = "#444",
            "&:hover" = list(borderColor = "#666"), # Example of adding hover effect
            "&:focus" = list(boxShadow = "0 0 0 2px #777") # Example of adding focus effect
          ),
          filterInputStyle = list(
            backgroundColor = "#444",
            "&:hover" = list(borderColor = "#666"), # Example of adding hover effect
            "&:focus" = list(boxShadow = "0 0 0 2px #777") # Example of adding focus effect
          )
        )
      )
    })
  })
}