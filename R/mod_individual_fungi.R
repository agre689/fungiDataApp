mod_individual_fungi_ui <- function(id) {
  ns <- NS(id)
  
  bslib::nav_panel(
    "Individual Fungi",
    bslib::card(
      full_screen = TRUE,
      bslib::layout_sidebar(
        sidebar = tagList(
          selectInput(
            inputId = ns("selected_icmp"),
            label = "ICMP Isolate",
            choices = expt_metadata$all_fungi[[1]],
            selected = "16487"
          )
        ),
        htmlOutput(ns("selected_icmp_o"), inline = T),
        bslib::layout_columns(
          col_widths = c(4, 8),
          # Value Boxes
          bslib::card(
            uiOutput(
              outputId = ns("vbox_screening")
            ),
            uiOutput(
              outputId = ns("vbox_extracts")
            ),
            uiOutput(
              outputId = ns("vbox_compounds")
            )
          ),
          # Map
          bslib::card(
            
          ),
        ),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Screening Summary",
            bslib::navset_card_tab(
              bslib::nav_panel(
                "Summary Plots",
                bslib::layout_columns(
                  plotOutput(ns("plot_summary_screening_auc")),
                  plotOutput(ns("plot_summary_screening_zoi"))
                )
              ),
              bslib::nav_panel(
                "Platemaps",
                # bslib::layout_columns(
                  # col_widths = c(10, 2),
                  div(
                    plotOutput(ns("plot_platemaps_screening_long")),
                    style = "margin: 10px; overflow-x: scroll;"
                  )
                  # div(
                  #   plotOutput(
                  #     ns("plot_platemaps_legend_screening_long"),
                  #     width = "10em",
                  #     height = "25em"
                  #   )
                  # )
                # )
              ),
              bslib::nav_panel(
                "Individual Platemaps",
                div(
                  uiOutput(
                    outputId = ns("ui_platemaps_screening")
                  )
                )
              )
            )
          ),
          bslib::accordion_panel(
            title = "Extract Testing Summary",
            bslib::navset_card_tab(
              bslib::nav_panel(
                "Extract Testing Summary",
                plotOutput(outputId = ns("plot_extracts_summary"))
              ),
              bslib::nav_panel(
                "Platemaps",
                div(
                  plotOutput(ns("plot_platemaps_extracts_long")),
                  style = "margin: 10px; overflow-x: scroll;"
                )
              ),
              bslib::nav_panel(
                "Individual Platemaps",
                div(
                  uiOutput(
                    outputId = ns("ui_platemaps_extracts")
                  )
                )
              )
            )
          ),
          bslib::accordion_panel(
            title = "Semipure and Compound Testing Summary",
            bslib::navset_card_tab(
              bslib::nav_panel(
                "Semipure Testing Summary",
                plotOutput(outputId = ns("plot_semipures_summary"))
              ),
              bslib::nav_panel(
                "Semipure Platemaps",
                div(
                  plotOutput(ns("plot_platemaps_semipures_long")),
                  style = "margin: 10px; overflow-x: scroll;"
                )
              ),
              bslib::nav_panel(
                "Semipure Individual Platemaps",
                div(
                  uiOutput(
                    outputId = ns("ui_platemaps_semipures")
                  )
                )
              ),
              bslib::nav_panel(
                "Compound Testing Summary",
                plotOutput(outputId = ns("plot_compounds_summary"))
              ),
              bslib::nav_panel(
                "Compound Platemaps",
                div(
                  plotOutput(ns("plot_platemaps_compounds_long")),
                  style = "margin: 10px; overflow-x: scroll;"
                )
              ),
              bslib::nav_panel(
                "Compound Individual Platemaps",
                div(
                  uiOutput(
                    outputId = ns("ui_platemaps_compounds")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_individual_fungi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter relevant data sets for selected ICMP isolate
    filtered_data <- reactive({
      req(input$selected_icmp)
      list(
        info = filter(isolate_info, icmp_isolate == input$selected_icmp),
        status = filter(status_info, icmp_isolate == input$selected_icmp),
        
        screening_lux = filter(data_screening_lux, icmp_isolate == input$selected_icmp),
        screening_auc = filter(data_screening_auc, icmp_isolate == input$selected_icmp),
        screening_zoi = filter(data_screening_zoi, icmp_isolate == input$selected_icmp),
        summary_screening_lux = filter(expt_summaries$summary_screening_lux, icmp_isolate == input$selected_icmp),
        summary_screening_zoi = filter(expt_summaries$summary_screening_zoi, icmp_isolate == input$selected_icmp),
        
        extracts_lux = filter(data_extracts_lux, icmp_isolate == input$selected_icmp),
        extracts_auc = filter(data_extracts_auc, icmp_isolate == input$selected_icmp),
        summary_extracts = filter(expt_summaries$summary_extracts, icmp_isolate == input$selected_icmp),
        
        semipures_lux = filter(data_semipures_lux, icmp_isolate == input$selected_icmp),
        semipures_auc = filter(data_semipures_auc, icmp_isolate == input$selected_icmp),
        summary_semipures = filter(expt_summaries$summary_semipures, icmp_isolate == input$selected_icmp),
        
        compounds_lux = filter(data_compounds_lux, icmp_isolate == input$selected_icmp),
        compounds_auc = filter(data_compounds_auc, icmp_isolate == input$selected_icmp),
        summary_compounds = filter(expt_summaries$summary_compounds, icmp_isolate == input$selected_icmp)
      )
    })
    
    # Text output
    output$selected_icmp_o <- renderUI({
      isolate_name_bits <- stringr::str_split(filtered_data()$info$species, " ") %>%
        unlist()
      isolate_name_bits_short <- stringr::str_split(filtered_data()$info$species_short, " ") %>%
        unlist()
      
      if (stringr::str_detect(isolate_name_bits_short[2], "sp\\.|[:upper:]")) {
        em_isolate_title <- paste0("<em>", isolate_name_bits_short[1], "</em>")
        rest_of_title <- paste(isolate_name_bits[2:length(isolate_name_bits)],
                               collapse = " ")
      } else {
        em_isolate_title <- paste0("<em>", isolate_name_bits_short[1], " ",
                                   isolate_name_bits_short[2], "</em>")
        rest_of_title <- paste(isolate_name_bits[!(isolate_name_bits %in% isolate_name_bits_short)],
                               collapse = " ")
      }
      
      h3(class = "text-center", HTML(
        # "Text <em>Text</em> Text"
        paste0("ICMP ", input$selected_icmp, ": ", em_isolate_title, " ", rest_of_title)
      ))
    })
    
    
    # Valuebox Rendering
    
    box_colour_screening <- c(
      "Active in Screening" = "success",
      "Weakly Active in Screening" = "olive",
      "Screening In Progress" = "warning",
      "Awaiting Screening/In Progress" = "warning",
      "Not Active in Screening" = "danger",
      "Not Ready for Screening" = "light"
    )
    box_colour_extracts <- c(
      "Extracts Active" = "success",
      "Extracts Inactive" = "danger",
      "Awaiting Extract Testing" = "warning",
      "Extract Testing In Progress" = "warning",
      "Awaiting Extraction" = "warning",
      "Not Extracted - Weak Screening Activity" = "light",
      "Not Extracted - No Screening Activity" = "light",
      "Not Extracted - No Screening Data" = "light",
      "Not Extracted" = "light"
    )
    box_colour_compounds <- c(
      "Active Compounds Found" = "success",
      "Compounds Not Active" = "danger",
      "Compounds Not Tested" = "light",
      "Semi-pures Active" = "warning",
      "Semi-pures Not Active" = "danger",
      "Semi-pures Not Tested" = "light",
      "No Semi-pures or Compounds Tested" = "light",
      "Invalid Compound or Semi-pure Status" = "light"
    )
    
    output$vbox_screening <- renderUI({
      val = as.character(filtered_data()$status$screening_status)
      
      bslib::value_box(
        title = "Screening Activity",
        value = val,
        theme = box_colour_screening[val]
      )
    })
    
    output$vbox_extracts <- renderUI({
      val = as.character(filtered_data()$status$extracts_status)
      
      bslib::value_box(
        title = "Screening Activity",
        value = val,
        theme = box_colour_extracts[val]
      )
    })
    
    output$vbox_compounds <- renderUI({
      if(as.character(filtered_data()$status$compounds_status) == "Compounds Not Tested" & 
         as.character(filtered_data()$status$semipures_status) == "Semi-pures Not Tested") {
        val <- "No Semi-pures or Compounds Tested"
      } else if(as.character(filtered_data()$status$compounds_status) != "Compounds Not Tested") {
        val <- as.character(filtered_data()$status$compounds_status)
      } else if(as.character(filtered_data()$status$semipures_status) != "Semi-pures Not Tested") {
        val <- as.character(filtered_data()$status$semipures_status)
      } else {
        val <- "Invalid Compound or Semi-pure Status"
      }
      # print(val)
      bslib::value_box(
        title = "Semipures/Compounds Status",
        value = val,
        theme = box_colour_compounds[val]
      )
    })
    
    #Summary Plots
    # Plot 1
    output$plot_summary_screening_auc <- renderPlot({
      req(input$selected_icmp)
      
      ggplot2::ggplot(
        filtered_data()$screening_auc,
        ggplot2::aes(x = as.character(age_percent), y = log_reduction_auc, fill = medium)
      ) +
        ggplot2::geom_boxplot()
    })
    
    # Plot 2
    output$plot_summary_screening_zoi <- renderPlot({
      req(input$selected_icmp)
      
      ggplot2::ggplot(
        filtered_data()$screening_zoi,
        ggplot2::aes(x = as.character(age_percent), y = zoi, fill = medium)
      ) +
        ggplot2::geom_boxplot()
    })
    
    # Long Platemaps
    observe({
      output$plot_platemaps_screening_long <- renderPlot(
        plot_platemaps_24wp(input$selected_icmp),
        # height = 420
        width = length(unique(filtered_data()$screening_auc$biological_rep)) * 580
      )    
      # output$plot_platemaps_legend_screening_long <- renderPlot({
      #   plot_platemap_legend()
      # }, bg = "transparent")
    })
    
    # Individual Platemaps
    
    observe({
      platemaps_list_screening <- filtered_data()$screening_lux %>%
        filter(time_point == 6) %>%
        group_by(biological_rep) %>%
        do(p = plot_platemap_24wp(.)) %>%
        pull(p)
      
      # Create dynamic Number of Outputs:
        purrr::iwalk(platemaps_list_screening, ~ {
          output_name <- paste0("screening_plate", .y)
          output[[output_name]] <- renderPlot(.x)
          # print(output_name)
        })
        
        # Create dynamic number of UI elements:
        output$ui_platemaps_screening <- renderUI({
          plots_list <- purrr::imap(platemaps_list_screening, ~{
            tagList(
              plotOutput(
                outputId = ns(paste0("screening_plate", .y)),
                width = "800px",
                height = "450px"
              ),
              br()
            )
          })
          tagList(plots_list)
        })
    })
    

    # Extract Testing
    
    observe({
      output$plot_extracts_summary <- renderPlot({
        filtered_data()$extracts_auc %>% 
          # filter(data_extracts_auc, icmp_isolate == 16487) %>% 
          ggplot2::ggplot(
            ggplot2::aes(
              x = factor(concentration, levels = rev(serial_dilution(1000, 7)), ordered = T),
              y = log_reduction_auc,
              fill = extract_fraction
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(organism ~ extract)
      })
    })
    
    # Long Platemaps
    observe({
      n_platemaps_extracts <- filtered_data()$extracts_auc %>% 
        group_by(extract, biological_rep, organism) %>% 
        n_groups()
      
      output$plot_platemaps_extracts_long <- renderPlot(
        plot_platemaps_96wp(filtered_data()$extracts_lux),
        # height = 420
        width = n_platemaps_extracts * 580
      )    
      # output$plot_platemaps_legend_screening_long <- renderPlot({
      #   plot_platemap_legend()
      # }, bg = "transparent")
    })
    
    # Individual Platemaps
    
    observe({
      platemaps_list_extracts <- filtered_data()$extracts_lux %>%
        filter(
          time_point %in% c(6, 24, 72),
          !(organism %in% c("MA", "MM") & time_point == 24)
        ) %>% 
        group_by(extract, biological_rep, organism, time_point) %>% 
        do(p = plot_platemap_96wp(.)) %>% 
        pull(p)
      
      # Create dynamic Number of Outputs:
      purrr::iwalk(platemaps_list_extracts, ~ {
        output_name <- paste0("extracts_plate", .y)
        output[[output_name]] <- renderPlot(.x)
        # print(output_name)
      })
      
      # Create dynamic number of UI elements:
      output$ui_platemaps_extracts <- renderUI({
        plots_list <- purrr::imap(platemaps_list_extracts, ~{
          tagList(
            plotOutput(
              outputId = ns(paste0("extracts_plate", .y)),
              width = "800px",
              height = "450px"
            ),
            br()
          )
        })
        tagList(plots_list)
      })
    })
    
    # Semipures and COmpounds
    
    # Summary Plots
    observe({
      output$plot_semipures_summary <- renderPlot({
        filtered_data()$semipures_auc %>% 
          # filter(data_semipures_auc, icmp_isolate == 16864) %>% 
          ggplot2::ggplot(
            ggplot2::aes(
              x = factor(concentration, levels = rev(serial_dilution(1000, 7)), ordered = T),
              y = log_reduction_auc,
              fill = extract
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(organism ~ semi_pure_name)
      })
    })
    
    observe({
      output$plot_compounds_summary <- renderPlot({
        filtered_data()$compounds_auc %>% 
          # filter(data_compounds_auc, icmp_isolate == 16864) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = factor(concentration, levels = rev(serial_dilution(64, 7)), ordered = T),
              y = log_reduction_auc,
              fill = extract
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(organism ~ compound_name)
      })
    })
    
    # Long Platemaps
    
    observe({
      n_platemaps_semipures <- filtered_data()$semipures_auc %>% 
        group_by(semi_pure_name, biological_rep, organism) %>% 
        n_groups()
      
      output$plot_platemaps_semipures_long <- renderPlot(
        plot_platemaps_semipures(filtered_data()$semipures_lux),
        # height = 420
        width = n_platemaps_semipures * 580
      )    
      # output$plot_platemaps_legend_screening_long <- renderPlot({
      #   plot_platemap_legend()
      # }, bg = "transparent")
    })
    
    observe({
      n_platemaps_compounds <- filtered_data()$compounds_auc %>% 
        group_by(compound_name, biological_rep, organism) %>% 
        n_groups()
      
      output$plot_platemaps_compounds_long <- renderPlot(
        plot_platemaps_compounds(filtered_data()$compounds_lux),
        # height = 420
        width = n_platemaps_compounds * 580
      )    
      # output$plot_platemaps_legend_screening_long <- renderPlot({
      #   plot_platemap_legend()
      # }, bg = "transparent")
    })
    
    # Individual Platemaps
    
    observe({
      platemaps_list_semipures <- filtered_data()$semipures_lux %>%
        filter(
          time_point %in% c(6, 24, 72),
          !(organism %in% c("MA", "MM") & time_point == 24)
        ) %>% 
        group_by(semi_pure_name, biological_rep, organism, time_point) %>% 
        do(p = plot_platemap_semipure(.)) %>% 
        pull(p)
      
      # Create dynamic Number of Outputs:
      purrr::iwalk(platemaps_list_semipures, ~ {
        output_name <- paste0("semipures_plate", .y)
        output[[output_name]] <- renderPlot(.x)
        # print(output_name)
      })
      
      # Create dynamic number of UI elements:
      output$ui_platemaps_semipures <- renderUI({
        plots_list <- purrr::imap(platemaps_list_semipures, ~{
          tagList(
            plotOutput(
              outputId = ns(paste0("semipures_plate", .y)),
              width = "800px",
              height = "450px"
            ),
            br()
          )
        })
        tagList(plots_list)
      })
    })
    
    observe({
      platemaps_list_compounds <- filtered_data()$compounds_lux %>%
        filter(
          time_point %in% c(6, 24, 72),
          !(organism %in% c("MA", "MM") & time_point == 24)
        ) %>% 
        group_by(compound_name, biological_rep, organism, time_point) %>% 
        do(p = plot_platemap_compound(.)) %>% 
        pull(p)
      
      # Create dynamic Number of Outputs:
      purrr::iwalk(platemaps_list_compounds, ~ {
        output_name <- paste0("compounds_plate", .y)
        output[[output_name]] <- renderPlot(.x)
        # print(output_name)
      })
      
      # Create dynamic number of UI elements:
      output$ui_platemaps_compounds <- renderUI({
        plots_list <- purrr::imap(platemaps_list_compounds, ~{
          tagList(
            plotOutput(
              outputId = ns(paste0("compounds_plate", .y)),
              width = "800px",
              height = "450px"
            ),
            br()
          )
        })
        tagList(plots_list)
      })
    })
  })
}
