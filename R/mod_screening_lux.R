mod_screening_lux_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    "Screening - Luminescence",
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
          shinyWidgets::pickerInput(
            inputId = ns("picker_map"),
            label = "Map Type",
            choices = c(
              "Default" = "default",
              "NZ Regions" = "regions",
              "NZ Iwi" = "iwi",
              "Topographic" = "topo",
              "Satellite" = "satellite"
            ),
            selected = "default"
          ),
          leaflet::leafletOutput(outputId = ns("map_"))
        ),
      ),
      bslib::accordion(
        bslib::accordion_panel(
          title = "Media Differences",
          bslib::layout_columns(
            col_widths = c(8, 4),
            bslib::card(
              bslib::card(
                # actionButton(
                #   inputId = ns("box_click"),
                #   label = bslib::value_box(
                #     title = "Users",
                #     value = "128"
                #     # showcase = bs_icon("people-fill")
                #   ),
                #   class = "valuebox-btn"
                # )
              ),
              bslib::card(
                bslib::layout_columns(
                  uiOutput(outputId = ns("vbox_active_cya")),
                  uiOutput(outputId = ns("vbox_active_mea")),
                  uiOutput(outputId = ns("vbox_active_oa")),
                  uiOutput(outputId = ns("vbox_active_pda")),
                  col_widths = c(3, 3, 3, 3)
                )
              ),
              bslib::card(
                plotOutput(outputId = ns("plot_histo_medium")),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  plotOutput(outputId = ns("plot_box1_medium")),
                  plotOutput(outputId = ns("plot_box2_medium"))
                )
              )
            ),
            bslib::card(
              plotOutput(outputId = ns("plot_divbar_medium"))
            )
          )
        ),
        bslib::accordion_panel(
          title = "Age Differences",
          bslib::layout_columns(
            col_widths = c(8, 4),
            bslib::card(
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(
                  shinyWidgets::radioGroupButtons(
                    inputId = ns("btn_plotly_age"),
                    label = "Select Plot Type:",
                    choices = c(
                      `<i class='fa fa-pie-chart'></i> Pie`= "pie",
                      `<i class='fa fa-bar-chart'></i> Histogram` = "histogram"
                    ),
                    selected = "pie",
                    justified = T
                  ),
                  uiOutput(outputId = ns("resetbtn_pie_age")),
                  plotly::plotlyOutput(outputId = ns("plotly_age"))
                ),
                bslib::card(
                  plotOutput(outputId = ns("plot_summary_age"))
                )
              )
            ),
            bslib::card(
              plotOutput(outputId = ns("plot_slope_age"))
            )
          )
        ),
        bslib::accordion_panel(
          title = "Phylogenetic Differences",
          bslib::card(
            bslib::layout_columns(
              col_widths = c(4, 8),
              bslib::card(
                plotly::plotlyOutput(outputId = ns("plotly_pie_phyla")),
                uiOutput(outputId = ns("resetbtn_pie_phylo")),
                uiOutput(outputId = ns("ui_picker_phylo"))),
              bslib::card(
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_x"),
                  label = "Select X-Axis Variable",
                  choices = list(
                    "Phylum" = "phylum",
                    "Class" = "class",
                    "Order" = "order",
                    "Family" = "family",
                    # "Genus" = "genus",
                    "Species" = "species_short",
                    "ICMP Isolate" = "icmp_isolate"
                  ),
                  selected = "phylum"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_colour"),
                  label = "Select Colour Variable",
                  choices = list(
                    "Medium" = "medium",
                    "Organism" = "organism",
                    "Age Percent" = "age_percent",
                    "Growth Rate" = "age_grown_group"
                  ),
                  selected = "medium"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_facet"),
                  label = "Select Facet Variable",
                  choices = list(
                    "Medium" = "medium",
                    "Organism" = "organism",
                    "Age Percent" = "age_percent",
                    "Growth Rate" = "age_grown_group"
                  ),
                  selected = "organism"
                ),
                # shinyWidgets::pickerInput(
                #   inputId = ns("picker_phylo_colour"),
                #   label = "Select Colour Variable",
                #   choices = list(
                #     "Phylum" = "phylum",
                #     "Class" = "class",
                #     "Order" = "order",
                #     "Family" = "family",
                #     # "Genus" = "genus",
                #     "Species" = "species_short",
                #     "ICMP Isolate" = "icmp_isolate"
                #   ),
                #   selected = "phylum"
                # )
                plotOutput(
                  outputId = ns("plot_box_phylo")
                )
              )
            )
          ),
          bslib::card(
            bslib::layout_sidebar(
              # Sidebar
              sidebar = bslib::sidebar(
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_tree_x"),
                  label = "Select Colour Variable",
                  choices = list(
                    "Median Activty Score" = "median_log_reduction_auc",
                    "Median Luminescence at 6h" = "median_lux_t_end"
                  ),
                  selected = "median_log_reduction_auc"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_tree_colour"),
                  label = "Select Colour Variable",
                  choices = list(
                    "Medium" = "medium",
                    "Organism" = "organism",
                    "Age Percent" = "age_percent",
                    "Growth Rate" = "age_grown_group"
                  ),
                  selected = "medium"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_tree_plot_layout"),
                  label = "Select Colour Variable",
                  choices = list(
                    "Circular" = "circular",
                    "Rectangular" = "rectangular",
                    "Fan" = "fan"
                  ),
                  selected = "circular"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("picker_phylo_tree_plot_type"),
                  label = "Select Colour Variable",
                  choices = list(
                    "Bar" = "column",
                    "Tile" = "tile",
                    "Boxplot" = "boxplot"
                  ),
                  selected = "tile"
                ),
                sliderInput(
                  inputId = ns("slider_plot_tree_height"),
                  label = "Plot Height",
                  min = 400,
                  value = 1200, 
                  max = 2400,
                  step = 400
                ),
                sliderInput(
                  inputId = ns("slider_plot_tree_offset"),
                  label = "Offset between plots",
                  min = 0,
                  value = 0.1, 
                  max = 1
                )
              ),
              
              # Main Panel
              # plotOutput(
              #   outputId = ns("plot_tree"), height = "800px"
              # )
              uiOutput(
                outputId = ns("plot_tree_ui")
              )
            )
          )
        )
      )
    )
  )
}

mod_screening_lux_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plotly_source <- reactiveVal(paste0("P-", as.integer(runif(1, 1, 1e6))))

    observeEvent(input$box_click, {
      print("Value box clicked!")
    })
    
    
    
    
    
    
    
    
    
    
    
    
    summary_data <- expt_summaries$summary_screening_lux %>%
      filter(age_percent %in% c(100, 200)) %>%
      left_join(expt_summaries$age_data, by = "icmp_isolate") %>%
      left_join(isolate_info, by = c("icmp_isolate"))

    screening_data <- data_screening_auc %>%
      filter(organism %in% c("EC", "PA"), age_percent %in% c(100, 200)) %>%
      left_join(isolate_info, by = c("icmp_isolate")) %>%
      left_join(expt_summaries$age_data, by = "icmp_isolate")

    screened_fungi <- reactiveVal({
      summary_data$icmp_isolate %>%
        unique()
    })
    # Value Boxes

    output$vbox_screened <- renderUI({
      bslib::value_box(
        title = "ICMP Isolates Screened",
        value = length(screened_fungi()),
        theme = "warning"
      )
    })
    output$vbox_active <- renderUI({
      bslib::value_box(
        title = "ICMP Isolates Active",
        value = summary_data %>%
          group_by(icmp_isolate) %>%
          count(median_log_reduction_auc > 1) %>%
          filter(`median_log_reduction_auc > 1`) %>%
          nrow(),
        theme = "success"
      )
    })
    output$vbox_extracted <- renderUI({
      bslib::value_box(
        title = "ICMP Isolates Extracted",
        value = metadata_extracts %>%
          filter(MEDIA != "PDB", ICMP %in% screened_fungi()) %>%
          n_unique("ICMP"),
        theme = "info"
      )
    })
    
    observe({
      if(input$picker_map == "regions") {
        output$map_ <- leaflet::renderLeaflet(map_regions_lux)
      }
      if(input$picker_map == "iwi") {
        output$map_ <- leaflet::renderLeaflet(map_iwi_lux)
      }
      if(input$picker_map == "satellite") {
        output$map_ <- leaflet::renderLeaflet(map_satellite_lux)
      }
      if(input$picker_map == "topo") {
        output$map_ <- leaflet::renderLeaflet(map_topo_lux)
      }
      if(input$picker_map == "default") {
        map_default_lux
      }
    })

    # Media Accordion
    data_screening_medium_total_active <- summary_data %>%
      group_by(icmp_isolate, medium) %>%
      summarise(is_active_ever = T %in% (median_log_reduction_auc > 1)) %>%
      group_by(medium) %>%
      count(is_active_ever) %>%
      filter(is_active_ever)

    output$vbox_active_cya <- renderUI({
      actionButton(
        inputId = "btn_vbox_active_cya",
        label = bslib::value_box(
          title = "Active on CYA",
          value = data_screening_medium_total_active$n[[1]],
          theme = "info"
        ),
        class = "valuebox_btn"
      )
    })
    output$vbox_active_mea <- renderUI({
      actionButton(
        inputId = "btn_vbox_active_mea",
        label = bslib::value_box(
          title = "Active on MEA",
          value = data_screening_medium_total_active$n[[2]],
          theme = "info"
        ),
        class = "valuebox_btn"
      )
      
    })
    output$vbox_active_oa <- renderUI({
      actionButton(
        inputId = "btn_vbox_active_oa",
        label = bslib::value_box(
          title = "Active on OA",
          value = data_screening_medium_total_active$n[[3]],
          theme = "info"
        ),
        class = "valuebox_btn"
      )
      
    })
    output$vbox_active_pda <- renderUI({
      actionButton(
        inputId = "btn_vbox_active_pda",
        label = bslib::value_box(
          title = "Active on PDA",
          value = data_screening_medium_total_active$n[[4]],
          theme = "info"
        ),
        class = "valuebox_btn"
      )
      
    })

    output$plot_histo_medium <- renderPlot({
      summary_data %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = median_log_reduction_auc
          )
        ) +
        ggplot2::geom_histogram() +
        ggplot2::facet_grid(. ~ medium)
    })

    # Highest x for each medium
    output$plot_box1_medium <- renderPlot({
      x_order <- summary_data %>%
        filter(medium == "CYA") %>%
        group_by(icmp_isolate) %>%
        summarise(median_log_reduction_auc = median(median_log_reduction_auc)) %>%
        arrange(desc(median_log_reduction_auc)) %>%
        .[["icmp_isolate"]] %>%
        .[1:10]

      summary_data %>%
        filter(icmp_isolate %in% x_order) %>%
        mutate(
          icmp_isolate = factor(icmp_isolate, levels = x_order, ordered = T)
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = icmp_isolate,
            y = median_log_reduction_auc,
            fill = medium
          )
        ) +
        ggplot2::geom_boxplot()
    })

    # Largest difference between 1 medium and all others
    output$plot_box2_medium <- renderPlot({
      x_order <- summary_data %>%
        group_by(icmp_isolate, medium) %>%
        summarise(median_log_reduction_auc = median(median_log_reduction_auc)) %>%
        tidyr::pivot_wider(names_from = medium, values_from = median_log_reduction_auc) %>%
        rowwise() %>%
        mutate(media_diff = abs(CYA - (mean(MEA, OA, PDA)) / mean(MEA, OA, PDA))) %>%
        arrange(desc(media_diff)) %>%
        .[["icmp_isolate"]] %>%
        .[1:10]

      summary_data %>%
        filter(icmp_isolate %in% x_order) %>%
        mutate(
          icmp_isolate = factor(icmp_isolate, levels = x_order, ordered = T)
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = icmp_isolate,
            y = median_log_reduction_auc,
            fill = medium
          )
        ) +
        ggplot2::geom_boxplot()

    })

    output$plot_divbar_medium <- renderPlot({
      summary_data %>%
        group_by(icmp_isolate, medium) %>%
        summarise(median_log_reduction_auc = median(median_log_reduction_auc)) %>%
        mutate(
          is_active = ifelse(
            median_log_reduction_auc > 1,
            "Active",
            "Not Active"
          ) %>% factor(levels = c("Active", "Not Active"))
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = reorder(icmp_isolate, median_log_reduction_auc),
            y = median_log_reduction_auc
          )
        ) +
        ggplot2::geom_bar(
          stat = "identity",
          ggplot2::aes(
            fill = is_active
          ),
          width = 0.7
        )  +
        ggplot2::facet_grid(.~medium) +
        ggplot2::coord_flip()

    })

    # Age Accordion

    output$plotly_age <- plotly::renderPlotly({
      expt_summaries$age_data %>%
        group_by(age_grown_group) %>%
        count() %>%
        plotly::plot_ly(source = "A") %>%
        {
          if(input$btn_plotly_age == "pie") {
            plotly::add_pie(
              .,
              values = ~n,
              labels = ~age_grown_group,
              customdata = ~age_grown_group,
              sort = F,
              direction = "clockwise"
            )
          } else {
            plotly::add_bars(
              .,
              x = ~age_grown_group,
              y = ~n,
              customdata = ~age_grown_group
            )
          }
        } %>%
        plotly::event_register("plotly_click")
    })

    observe(print(paste("A:", current_category_age())))

    current_category_age <- reactiveVal()

    observeEvent(
      plotly::event_data(event = "plotly_click", source = "A")$customdata[[1]],
      current_category_age(plotly::event_data(event = "plotly_click", source = "A")$customdata[[1]])
    )

    # populate back button if category is chosen
    output$resetbtn_pie_age <- renderUI({
      if (is.null(current_category_age())) return(NULL)
      actionButton(ns("btn_clear_age"), "Reset", icon("chevron-left"))
    })


    observeEvent(input$btn_clear_age, {
      isolate({
        current_category_age(NULL)
        # updateSelectInput(session, "picker_phylo_x", selected = "phylum")
        # updateSelectInput(session, "picker_phylo_colour", selected = "phylum")
        # if (exists("relevant_isolate_info") && nrow(relevant_isolate_info) > 0) {
        #   data_phylo_count(dplyr::count(relevant_isolate_info, phylum))
        # } else {
        #   data_phylo_count(tibble::tibble(labels = character(), values = integer()))
        # }
      })
    }, ignoreInit = TRUE)

    output$plot_summary_age <- renderPlot({
      if (is.null(current_category_age())) {
        screening_data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = age_grown_group,
              y = log_reduction_auc,
              fill = as.character(age_percent)
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(. ~ organism)
      } else {
        screening_data %>%
          filter(age_grown_group %in% current_category_age()) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = medium,
              y = log_reduction_auc,
              fill = as.character(age_percent)
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(. ~ organism)
      }

    })

    output$plot_slope_age <- renderPlot({
      summary_data %>%
        filter(age_percent %in% c(100, 200)) %>%
        group_by(icmp_isolate, age_percent) %>%
        summarise(median_log_reduction_auc = median(median_log_reduction_auc)) %>%
        tidyr::pivot_wider(names_from = age_percent, values_from = median_log_reduction_auc) %>%
        mutate(
          gradient = ifelse((`100` - `200`) < 0, "red", "blue"),
          left_label = paste0(icmp_isolate, ", 100%"),
          right_label = paste0(icmp_isolate, ", 200%")
        ) %>% ggplot2::ggplot() +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = 1,
            xend = 2,
            y = `100`,
            yend = `200`,
            col = gradient
          ),
          size = 1,
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = left_label,
            x = 1,
            y = `100`,
            hjust = 1.1
          )
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = right_label,
            x = 2,
            y = `200`,
            hjust = -0.1
          )
        ) +
        ggplot2::geom_vline(
          xintercept = 1,
          linetype = "longdash",
          linewidth = 0.1
        ) +
        ggplot2::geom_vline(
          xintercept = 2,
          linetype = "longdash",
          linewidth = 0.1
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(-1, 5, 0.2)
        ) +
        ggplot2::xlim(0.5, 2.5)
    })

    # Phylo Accordion

    output$plotly_pie_phyla <- plotly::renderPlotly(plotly::plot_ly())
    output$plot_box_phylo <- renderPlot(ggplot2::ggplot())

    relevant_expt_metadata <- expt_metadata$screening_lux
    relevant_isolate_info <- isolate_info %>%
      filter(icmp_isolate %in% relevant_expt_metadata$isolates)
    phyla <- relevant_expt_metadata$phyla
    classes <- relevant_expt_metadata$classes
    orders <- relevant_expt_metadata$orders
    families <- relevant_expt_metadata$families
    # genera <- relevant_expt_metadata$genera
    specieses <- relevant_expt_metadata$specieses # Spelled incorrectly on purpose :)
    icmp_isolates <- relevant_expt_metadata$isolates

    # The specific currently-selected phylogenetic group.
    # E.g. "Ascomycota", "Helotiales" etc.
    current_category_phylo <- reactiveVal()

    # Count the number of phylogenetic groups under the current selection.


    data_phylo_count <- reactiveVal(count(relevant_isolate_info, phylum))

    observe({
      if (is.null(current_category_phylo())) {
        data_phylo_count(count(relevant_isolate_info, phylum))
        print("worked")
      } else if (current_category_phylo() %in% phyla) {
        data_phylo_count(
          relevant_isolate_info %>%
            filter(phylum %in% current_category_phylo()) %>%
            count(class)
        )
      } else if (current_category_phylo() %in% classes) {
        data_phylo_count(
          relevant_isolate_info %>%
            filter(class %in% current_category_phylo()) %>%
            count(order)
        )
      } else if (current_category_phylo() %in% orders) {
        data_phylo_count(
          relevant_isolate_info %>%
            filter(order %in% current_category_phylo()) %>%
            count(family)
        )
      } else if (current_category_phylo() %in% families) {
        data_phylo_count(
          relevant_isolate_info %>%
            filter(family %in% current_category_phylo()) %>%
            count(species_short)
        )
      } else if (current_category_phylo() %in% specieses) {
        data_phylo_count(
          relevant_isolate_info %>%
            filter(species_short %in% current_category_phylo()) %>%
            count(icmp_isolate)
        )
      }
    })

    # read value with: data_phylo_count()

    # observe(print(head(data_phylo_count())))

    colours <- reactive(
      data_phylo_count() %>%
        nrow() %>%
        scico::scico(palette = "lajolla", begin = 0.15, end = 0.85)
    )

    output$plotly_pie_phyla <- plotly::renderPlotly({
      src <- plotly_source()
      data_phylo_count() %>%
        setNames(nm = c("labels", "values")) %>%
        plotly::plot_ly(source = src) %>%
        plotly::add_pie(
          labels = ~labels,
          values = ~values,
          marker = list(colors = colours()),
          customdata = ~labels,
          sort = FALSE,
          direction = "clockwise"
        ) %>%
        plotly::layout(
          title = rlang::`%||%`(current_category_phylo(), "Phylum")
          # paper_bgcolor = "#222222",
          # plot_bgcolor = "#222222",
          # font = list(color = "#FFFFFF", family = "Rubik")
        ) %>%
        plotly::event_register("plotly_click")
    })


    observe({
      src <- plotly_source()
      ed <- plotly::event_data("plotly_click", source = src)
      if (is.null(ed) || length(ed$customdata) == 0) return()
      cd <- ed$customdata[[1]]
      if (identical(cd, current_category_phylo())) return()    # ignore redundant assignments

      # assign selected category and update pickers
      current_category_phylo(cd)

      if (isTRUE(cd %in% relevant_expt_metadata$phyla)) {
        updateSelectInput(inputId = "picker_phylo_x", selected = "class")
        # updateSelectInput(inputId = "picker_phylo_colour", selected = "class")
      } else if (isTRUE(cd %in% relevant_expt_metadata$classes)) {
        updateSelectInput(inputId = "picker_phylo_x", selected = "order")
        # updateSelectInput(inputId = "picker_phylo_colour", selected = "order")
      } else if (isTRUE(cd %in% relevant_expt_metadata$orders)) {
        updateSelectInput(inputId = "picker_phylo_x", selected = "family")
        # updateSelectInput(inputId = "picker_phylo_colour", selected = "family")
      } else if (isTRUE(cd %in% relevant_expt_metadata$families)) {
        updateSelectInput(inputId = "picker_phylo_x", selected = "species_short")
        # updateSelectInput(inputId = "picker_phylo_colour", selected = "species_short")
      } else if (isTRUE(cd %in% relevant_expt_metadata$specieses)) {
        updateSelectInput(inputId = "picker_phylo_x", selected = "icmp_isolate")
        # updateSelectInput(inputId = "picker_phylo_colour", selected = "icmp_isolate")
      }
    })

    output$ui_picker_phylo <- renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("picker_phylo_pie"),
        label = "",
        options = list(
          title = "Select Phylogenetic Group",
          size = 10,
          `live-search` = TRUE
        ),
        choices = list(
          "Phylum" = sort(unique(screening_data$phylum)),
          "Class" = sort(unique(screening_data$class)),
          "Order" = sort(unique(screening_data$order)),
          "Family" = sort(unique(screening_data$family)),
          # "Genus" = sort(unique(screening_data$genus)),
          "Species" = sort(unique(screening_data$species_short))
        )
      )
    })

    # populate back button if category is chosen
    output$resetbtn_pie_phylo <- renderUI({
      if (is.null(current_category_phylo())) return(NULL)
      actionButton(ns("btn_clear_phyla"), "Reset", icon("chevron-left"))
    })

    # Reset handler: clear state, restore counts, and rotate the source id
    observeEvent(input$btn_clear_phyla, {
      current_category_phylo(NULL)
      shinyWidgets::updatePickerInput(inputId = "picker_phylo_x", selected = "phylum")
      # shinyWidgets::updatePickerInput(inputId = "picker_phylo_colour", selected = "phylum")
      data_phylo_count(dplyr::count(relevant_isolate_info, phylum))
      # rotate source id so any stale client events no longer match
      plotly_source(paste0("P-", as.integer(runif(1, 1, 1e6))))
    }, ignoreInit = TRUE)

    observe({
      if (!is.null(current_category_phylo())) {
        cat(Sys.time(), "SET current_category_phylo ->", current_category_phylo(), "\n")
      } else {
        cat(Sys.time(), "SET current_category_phylo -> NULL\n")
      }
    })


    plot_data_phylo <- reactiveVal(screening_data)

    observe({
      if(is.null(current_category_phylo())) {
        plot_data_phylo(screening_data)
      } else {
        plot_data_phylo({
          screening_data %>%
            filter(
              phylum %in% current_category_phylo() |
                class %in% current_category_phylo() |
                order %in% current_category_phylo() |
                family %in% current_category_phylo() |
                # genus %in% current_category_phylo() |
                species_short %in% current_category_phylo() |
                icmp_isolate %in% current_category_phylo()
            )
        })
      }
    })

    output$plot_box_phylo <- renderPlot({
      plot_data_phylo() %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data[[rlang::`%||%`(input$picker_phylo_x, "phylum")]],
            y = log_reduction_auc,
            fill = .data[[input$picker_phylo_colour]]
          )
        ) +
        ggplot2::geom_boxplot() +
        ggplot2::facet_grid(.~.data[[input$picker_phylo_facet]])
    })
    
    output$plot_tree_ui <- renderUI({
      plotOutput(
        outputId = ns("plot_tree"), 
        height = paste0(input$slider_plot_tree_height, "px")
      )
    })
    
    output$plot_tree <- renderPlot({
      plot_screening_tree(
        plot_data = summary_data,
        plot_type = input$picker_phylo_tree_plot_type,
        x_var = !!input$picker_phylo_tree_x,
        colour_var = !!input$picker_phylo_tree_colour,
        branch_length = "none",
        activity_threshold = NA,
        show_labels = F,
        full_labels = F,
        layout = input$picker_phylo_tree_plot_layout,
        offset = input$slider_plot_tree_offset,
        plot_width = 0.35
      ) +
        ggplot2::labs(
          fill = ifelse(
            input$picker_phylo_tree_plot_type == "tile",
            get_axis_title(input$picker_phylo_tree_x),
            get_axis_title(input$picker_phylo_tree_colour)
          )
        )
    })
  })
}
