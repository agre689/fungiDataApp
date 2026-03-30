# get_age_tags <- function() {
#   age_group_tags <- tibble(
#     age_days = unique(data_screening_auc$age_days),
#     avg_age_bin  = cut(
#       unique(data_screening_auc$age_days),
#       breaks = c(1, 7, 14, 21, 28, 100),
#       include.lowest = TRUE,
#       right = FALSE,
#       ordered_result = TRUE
#     )
#   ) %>%
#     filter(!is.na(avg_age_bin ))
#   levels(age_group_tags$avg_age_bin ) <- factor(
#     c("< 7 Days", "7 - 13 Days", "14 - 20 Days", "21 - 27 Days", "28+ Days"),
#     ordered = TRUE
#   )
#   age_group_tags
# }
# get_age_data <- function(binning = T) {
#   age_group_tags <- get_age_tags()
#   data_screening_auc %>%
#     filter(age_percent == 100, age_days < 30) %>%
#     group_by(icmp_isolate) %>%
#     summarise(avg_age_screened = round(median(age_days))) %>%
#     left_join(age_group_tags, by = c("avg_age_screened" = "age_days")) %>%
#     filter(!is.na(avg_age_bin )) %>%
#     right_join(
#       select(data_screening_auc),
#       by = "icmp_isolate"
#     ) %>%
#     filter(!is.na(avg_age_bin ))
# }
# 
# age_group_tags <- get_age_tags()
# agedata <- get_age_data()
# colour_set <- viridis::mako(
#   n = agedata %>%
#     select(avg_age_screened) %>%
#     unique() %>%
#     nrow(),
#   begin = 0.35,
#   end = 0.8
# )


plotly_screening_histogram <- function(age_data, binning = T, highlight_position = NULL, plotly_source = "A") {
  if(binning) {
    colours <- viridis::mako(
      n = 5,
      begin = 0.35,
      end = 0.8
    )
    if(!is.null(highlight_position)) colours[highlight_position] <- "#FF69B4"
    
    plot <- age_data %>%
      count(avg_age_bin ) %>%
      plotly::plot_ly(
        source = plotly_source
      ) %>%
      plotly::add_bars(
        x = ~avg_age_bin ,
        y = ~n,
        marker = list(
          color = colours
        )
      )
  } else {
    colours <- viridis::mako(
      n = age_data %>% 
        select(avg_age_screened) %>% 
        unique() %>% 
        nrow(),
      begin = 0.35,
      end = 0.8
    )
    if(!is.null(highlight_position)) colours[highlight_position] <- "#FF69B4"
    
    plot <- age_data %>% 
      count(avg_age_screened) %>% 
      plotly::plot_ly(
        source = plotly_source
      ) %>% 
      plotly::add_bars(
        x = ~avg_age_screened, 
        y = ~n,
        marker = list(
          color = colours
        )
      )
  }
  
  plot %>% 
    plotly::layout(
      bargap = 0,
      paper_bgcolor = "#222222",
      plot_bgcolor = "#222222",
      font = list(
        color = "#FFFFFF",
        family = "Rubik"
      ),
      xaxis = list(
        title = "Age (Days)"
      ),
      yaxis = list(
        title = "Frequency"
      )
    ) %>% 
    plotly::event_register("plotly_click")
}
# plotly_screening_histogram(agedata, F, 16)
# plotly_screening_histogram(agedata, T)

# Pass a df summarised with count()
plotly_screening_pie <- function(summary_data, x, highlight_position = NULL, plotly_source = "A") {
  colours <- viridis::mako(
    n = nrow(summary_data),
    begin = 0.35,
    end = 0.8
  )
  if(!is.null(highlight_position)) colours[highlight_position] <- "#FF69B4"
  
  # if(age_binning) x <- "avg_age_bin "
  
  summary_data %>% 
    plotly::plot_ly(
      source = plotly_source
    ) %>% 
    plotly::add_pie(
      values = ~n,
      labels = ~eval(ensym(x)),
      customdata = ~eval(ensym(x)),
      marker = list(
        colors = colours
      ),
      sort = F,
      direction = "clockwise"
    ) %>%
    plotly::layout(
      paper_bgcolor = "#222222",
      plot_bgcolor = "#222222",
      font = list(
        color = "#FFFFFF",
        family = "Rubik"
      )
    ) %>% 
    plotly::event_register("plotly_click")
}
# isolate_info %>% filter(icmp_isolate %in% unique(data_screening_auc$icmp_isolate)) %>% group_by(endemic) %>% count() %>% plotly_screening_pie(x = "endemic")
# data_screening_auc %>% filter(age_percent == 100, age_days < 30) %>% group_by(icmp_isolate) %>% summarise(avg_age_screened = round(median(age_days))) %>% group_by(avg_age_screened) %>% count() %>% plotly_screening_pie(x = "avg_age_screened", highlight_position = 16)
# data_screening_auc %>% filter(age_percent == 100, age_days < 30) %>% group_by(icmp_isolate) %>% summarise(avg_age_screened = round(median(age_days))) %>% left_join(age_group_tags, by = c("avg_age_screened" = "age_days")) %>% filter(!is.na(avg_age_bin )) %>% group_by(avg_age_bin ) %>% count() %>% plotly_screening_pie(x = "avg_age_bin ", highlight_position = 3)
