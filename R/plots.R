well_txt_colour <- function(num) {
  ifelse(num > 1 & num < 100, "white", "black")
}

ggtheme_dark <- function(base_size = 11, base_family = "rubik") {
  half_line <- base_size/2
  ggplot2::theme(
    line = ggplot2::element_line(colour = "#FF0000", size = 0.5,
                                 linetype = 1, lineend = "butt"),
    rect = ggplot2::element_rect(fill = "#222222", colour = "#FFFFFF",
                                 size = 0.5, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain",
                                 colour = "#EEEEEE", size =  base_size,
                                 lineheight = 0.9,  hjust = 0.5,
                                 vjust = 0.5, angle = 0,
                                 margin = ggplot2::margin(), debug = FALSE),

    axis.line = ggplot2::element_line(colour = "#DDDDDD"),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = "#EEEEEE"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8*half_line/2),
                                        vjust = 1),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8*half_line/2),
                                        hjust = 1),
    axis.ticks = ggplot2::element_line(colour = "#FFFFFF"),
    axis.ticks.length = grid::unit(half_line/2, "pt"),
    axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line,
                                                                  b = 0.8 * half_line/2)),
    axis.title.y = ggplot2::element_text(angle = 90,
                                         margin = ggplot2::margin(r = 0.8 * half_line,
                                                                  l = 0.8 * half_line/2)),

    legend.background = ggplot2::element_rect(colour = "#222222"),
    legend.spacing = grid::unit(0.2, "cm"),
    legend.key = ggplot2::element_rect(fill = "#222222", colour = "#222222"),
    legend.key.size = grid::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = grid::unit(0.02, "npc"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    panel.background = ggplot2::element_rect(fill = "#222222", colour = "#EEEEEE"),
    panel.border = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#888888"),
    panel.grid.minor.x = ggplot2::element_line(colour = "#555555"),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.spacing = grid::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL, panel.ontop = FALSE,

    strip.background = ggplot2::element_rect(fill = "#666666", colour = "#666666"),
    strip.text = ggplot2::element_text(colour = "#EEEEEE", size = ggplot2::rel(1)),
    strip.text.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line,
                                                                  b = half_line)),
    strip.text.y = ggplot2::element_text(angle = -90,
                                         margin = ggplot2::margin(l = half_line,
                                                                  r = half_line)),
    strip.switch.pad.grid = grid::unit(0.1, "cm"),
    strip.switch.pad.wrap = grid::unit(0.1, "cm"),

    plot.background = ggplot2::element_rect(colour = "#222222"),
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.3),
                                       margin = ggplot2::margin(b = half_line * 1.2)),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.2),
                                          hjust = 0.5),
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
}

ggtheme_light <- function(base_size = 11, base_family = "rubik") {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family
  )
}
facet_names = c(
  # Screened Organisms
  AB = "A. baumannii", EC = "E. coli", PA = "P. aeruginosa",
  KO = "K. oxytoca", KP = "K. pneumoniae", MA = "M. abscessus",
  MM = "M. marinum", MS = "M. smegmatis", SA = "S. aureus",
  # Fungi Growth Media
  CSA = "Czapek Solution", CYA = "Czapek Yeast", MEA = "Malt Extract",
  MYA = "Malt Yeast Extract", OA = "Oatmeal", PDA = "Potato Dextrose",
  REA = "Rice Extract", TYA = "Tryptone Yeast Extract",
  # Fungi Growth Percent
  "50" = "50% Growth", "100" = "100% Growth", 
  "200" = "200% Growth",
  # Age Groupings
  "Very Fast (< 7 Days)" = "Very Fast (< 7 Days)", 
  "Fast (7 - 13 Days)" = "Fast (7 - 13 Days)", 
  "Moderate (14 - 20 Days)" = "Moderate (14 - 20 Days)", 
  "Slow (21 - 27 Days)" = "Slow (21 - 27 Days)", 
  "Very Slow (28+ Days)" = "Very Slow (28+ Days)",
  # Endemic Status
  "Endemic" = "Endemic", "Indigenous" = "Indigenous", "Non-endemic" = "Non-endemic",
  "Exotic" = "Exotic", "Uncertain" = "Uncertain",
  # Extract Fractions
  "CRUDE" = "CRUDE", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"
)

# fungi <- all_isolates
# media <- all_media
# rounds <- most_screening_rounds
# ages <- all_ages
# maturity <- all_maturities

get_axis_title <- function(title) {
  axis_titles <- c(
    "ICMP Isolate" = "icmp_isolate",
    "ICMP Isolate" = "fungi",
    "Activity Score" = "log_reduction_auc",
    "Median Activity Score" = "median_log_reduction_auc",
    "% Growth Inhibition" = "percent_inhibition_auc",
    "% Growth Inhibition (Altered)" = "altered_percent_inhibition_auc",
    "ZOI Diameter (mm)" = "zoi",
    "Fungal Medium" = "medium",
    "Screened Against" = "organism",
    "Endemic Status"= "endemic",
    "Phylogenetic Class" = "class",
    "Phylogenetic Order" = "order",
    "Phylogenetic Family" = "family",
    "Genus" = "genus",
    "Species" = "species_short",
    "Age (Days)"= "age_days",
    "Growth Rate (Grouped Age to 100% Growth)" = "avg_age_bin",
    "Growth Rate (Average Age to 100% Growth)" = "avg_age_screened",
    "Age (Growth %)" = "age_percent",
    "Land District" = "LandDistrict_CE1",
    "Ecological District" = "EcologicalDistrict_CE1",
    "Locality" = "Locality_CE1",
    "Substrate" = "Substrate_CE1",
    "Habitat" = "Habitat_CE1",
    "Extract Fraction" = "extract_fraction",
    "Semipure Fraction" = "semi_pure_name",
    "Compound Name" = "compound_name",
    "Compound Code" = "compound_code",
    "A. baumannii" = "AB",
    "E. coli" = "EC",
    "P. aeruginosa" = "PA",
    "S. aureus" = "SA",
    "M. abscessus" = "MA",
    "M. marinum" = "MM"
  )
  if(is.null(title)) {
    NULL
  } else if(title %in% axis_titles) {
    names(axis_titles[axis_titles == title])
  } else if(title %in% isolate_info$species) {
    paste(str_split(title, " ")[[1]][1:2], collapse = " ")
  } else {
    tools::toTitleCase(title)
  }
}

# Plot Platemaps

# Input: Lux screening data filtered to show 1 ICMP isolate from 1 screening
#   round at 1 time point (filtered to prevent overlap).
# Output: Plot for 1 plate map.
# Note that x-axis labels (screened bacteria) are manually defined.
plot_platemap_24wp <- function(plot_data) {
  plot_title <- paste("Replicate ", unique(plot_data$biological_rep), 
                      " (", unique(plot_data$age_percent), "% growth) at ", 
                      unique(plot_data$time_point), " Hours", sep = "")
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = as.double(col_num), 
        y = rev(row_num), 
        fill = log10(luminescence)
      ),
      size = 36,
      colour = "#BBBBBB",
      shape = 21
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = as.double(col_num),
        y = rev(row_num),
        label = luminescence,
        colour = well_txt_colour(luminescence)
      ),
      size = 4.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = c("black", "white")
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1.5, 3.5, 5.5),
      labels = c("A. baumannii", "E. coli", "P. aeruginosa"),
      limits = c(0.8, 6.2)) +
    ggplot2::scale_y_discrete(labels = c("PDA", "OA", "MEA", "CYA")) +
    ggplot2::scale_fill_gradientn(
      colours = rev(grDevices::rainbow(7)),
      limits = c(0, 6),
      breaks = c(0:6),
      labels = c(1, 10, 100, 1000, 10000, 100000, 1000000)
    ) +
    ggplot2::labs(
      title = plot_title,
      x = NULL,
      y = NULL,
      fill = "Luminescence (RLU)\n"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic"))
}

# plot_platemap_24wp(filter(data_screening_lux, icmp_isolate == 17587, biological_rep == 3, time_point == 6))

# Input: Lux screening data filtered to show 1 extract from 1 screening
#   round at 1 time point (filtered to prevent overlap).
# Output: Plot for 1 plate map.
plot_platemap_96wp <- function(plot_data) {
  if(isTRUE(unique(plot_data$additive != "NONE"))) {
    plot_title <- paste0(
      unique(plot_data$extract), " (", 
      unique(plot_data$icmp_isolate), " + ",
      unique(plot_data$additive), " grown on ", 
      unique(plot_data$medium), ") \n",
      "vs ", unique(plot_data$organism),
      " Replicate ", unique(plot_data$biological_rep), 
      " at ", unique(plot_data$time_point), " Hours.")
  } else {
    plot_title <- paste0(
      unique(plot_data$extract), " (", unique(plot_data$icmp_isolate), " grown on ", 
      unique(plot_data$medium), ") \n",
      "vs ", unique(plot_data$organism),
      " Replicate ", unique(plot_data$biological_rep), 
      " at ", unique(plot_data$time_point), " Hours.")
  }
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = as.double(col_num),
        y = rev(row_num),
        fill = log10(luminescence)
      ),
      size = 20,
      shape = 21
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = as.double(col_num),
        y = rev(row_num),
        label = luminescence,
        colour = well_txt_colour(luminescence)
      ),
      size = 3.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white")) +
    ggplot2::scale_x_continuous(
      breaks = seq(1.5, 11.5, 2),
      labels = c(
        "CRUDE", "FRACTION 1", "FRACTION 2", "FRACTION 3", "FRACTION 4", "FRACTION 5"
      ),
      limits = c(0.8, 12.2)
    ) +
    ggplot2::scale_y_discrete(labels = c(0, 15.6, 31.2, 62.5, 125, 250, 500, 1000)) +
    ggplot2::scale_fill_gradientn(
      colours = rev(grDevices::rainbow(7)),
      limits = c(0, 6),
      breaks = c(0:6),
      labels = c(1, 10, 100, 1000, 10000, 100000, 1000000)
    ) +
    ggplot2::labs(
      title = plot_title,
      x = NULL,
      y = "Extract Concentration (\U00B5g/mL)",
      fill = "Luminescence (RLU)\n"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9))
}

# plot_platemap_96wp(filter(data_extracts_lux, extract == "MC10-162", biological_rep == 1, organism == "MA", time_point == 72))
# plot_platemap_96wp(filter(data_compounds_lux, compound_name == "Taiwapyrone", biological_rep == 1, organism == "MA", time_point == 72))


# Plate maps for all screening rounds for 1 isolate at the last time point.
# Input: ICMP isolate number.
# Output: Looooong plot with all platemaps for that isolate at the last time point.
plot_platemaps_24wp <- function(isolate) {
  plots <- data_screening_lux %>% 
    filter(icmp_isolate == isolate) %>% 
    group_by(biological_rep) %>% 
    slice_max(order_by = time_point) %>% 
    do(p = plot_platemap_24wp(.))
  p <- do.call(
    getFromNamespace("ggarrange", "ggpubr"), 
    c(plots$p,  ncol = n_groups(plots), legend = "none")
  )
  
  p + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#111111", color = "#111111")
  )
}
# plot_platemaps_24wp(17587)

plot_platemaps_96wp <- function(data) {
  plots <- data %>% 
    filter(
      time_point %in% c(24, 72),
      !(organism %in% c("MA", "MM") & time_point == 24)
    ) %>% 
    group_by(extract, organism, biological_rep) %>% 
    slice_max(order_by = time_point) %>% 
    do(p = plot_platemap_96wp(.))
  p <- do.call(
    getFromNamespace("ggarrange", "ggpubr"), 
    c(plots$p, ncol = n_groups(plots), legend = "none"))
  
  p + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#111111", color = "#111111")
  )
}

# plot_platemaps_96wp(16864, lux_data_extracts)

# plot_data <- filter(data_semipures_lux, icmp_isolate == 15555, screening_round == "SV1", organism == "EC", time_point == 24)
plot_platemap_semipure <- function(plot_data, force_angle_text = T) {
  if(nrow(plot_data)==0) stop("Plot data has no rows.")
  
  plot_title = paste0(
    "ICMP ", unique(plot_data$icmp_isolate),
    " on ", unique(plot_data$medium),
    " vs ", unique(plot_data$organism)
  )
  if(!is.na(unique(plot_data$additive))) {
    plot_subtitle  <-  paste(
      "\n", " + \"", unique(plot_data$additive), "\""
    )
  } else {
    plot_subtitle  <- "\n"
  }
  
  plot_data %>% 
    mutate(
      col_id = paste(semi_pure_name, technical_rep),
      row_num = factor(row_num, levels = LETTERS[8:1], ordered = T)
    ) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        x = col_id,
        y = row_num,
        fill = log10(luminescence)
      ),
      size = 20,
      colour = "#BBBBBB",
      shape = 21
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = col_id,
        y = row_num,
        label = luminescence,
        colour = well_txt_colour(luminescence)
      ),
      size = 3.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white")) +
    ggplot2::scale_y_discrete(labels = rev(unique(plot_data$concentration))) +
    ggplot2::scale_fill_gradientn(
      colours = rev(grDevices::rainbow(7)),
      limits = c(0, 6),
      breaks = c(0:6),
      labels = c(1, 10, 100, 1000, 10000, 100000, 1000000)
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = "Concentration (\U00B5g/mL)",
      fill = "Luminescence (RLU)\n"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9),
      plot.subtitle = ggplot2::element_text(lineheight = 0, size = 10)
    ) +
    {
      if(force_angle_text) {
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 0.5, vjust = 0.5))
      } else ggplot2::theme()
    }
}

# isolate <- 15555
# data <- lux_data_semipures
plot_platemaps_semipures <- function(data) {
  data2 <- data %>%
    filter(
      time_point %in% c(24, 72),
      !(organism %in% c("MA", "MM") & time_point == 24)
    ) %>%
    group_by(screening_round, organism, biological_rep, extract) %>%
    slice_max(order_by = time_point)
  
  n_cols_all <- data2 %>% 
    count(screening_round, organism, biological_rep) %>% 
    mutate(n = (n / 8)+1.6)
  
  plots <- data2 %>% 
    do(p = plot_platemap_semipure(.))
  
  p <- do.call(
    getFromNamespace("ggarrange", "ggpubr"), c(
      plots$p, 
      nrow = 1,
      legend = "none", 
      list(widths = n_cols_all$n)
    )
  )
  p + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#111111", color = "#111111")
  )
}

plot_platemap_compound <- function(plot_data) {
  if(tolower(unique(plot_data$additive)) != "none") {
    plot_subtitle <- paste0(
      # unique(plot_data$compound_name), "\n(", 
      unique(plot_data$compound_code), " - ",
      unique(plot_data$icmp), " + ",
      unique(plot_data$additive), " grown on ", 
      unique(plot_data$medium), ") \n",
      "vs ", unique(plot_data$organism),
      " Replicate ", unique(plot_data$biological_rep), 
      " at ", unique(plot_data$time_point), " Hours.")
  } else {
    plot_subtitle <- paste0(
      # unique(plot_data$compound_name), "\n(", 
      unique(plot_data$compound_code), " \U2013 ",
      unique(plot_data$icmp), " grown on ", 
      unique(plot_data$medium), ") \n",
      "vs ", unique(plot_data$organism),
      " Replicate ", unique(plot_data$biological_rep), 
      " at ", unique(plot_data$time_point), " Hours.")
  }
  
  cols_read <- unique(plot_data$col_num)
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = as.double(col_num),
        y = rev(row_num),
        fill = log10(luminescence)
      ),
      size = 20,
      colour = "#BBBBBB",
      shape = 21
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = as.double(col_num),
        y = rev(row_num),
        label = luminescence,
        colour = well_txt_colour(luminescence)
      ),
      size = 3.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white")) +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      labels = NULL,
      limits = c(min(cols_read) - 0.2, 
                 max(cols_read) + 0.2)) +
    ggplot2::scale_y_discrete(labels = rev(unique(plot_data$concentration))) +
    ggplot2::scale_fill_gradientn(
      colours = rev(rainbow(7)),
      limits = c(0, 6),
      breaks = c(0:6),
      labels = c(1, 10, 100, 1000, 10000, 100000, 1000000)
    ) +
    ggplot2::labs(
      title = unique(plot_data$compound_name),
      subtitle = plot_subtitle, 
      x = NULL,
      y = "Concentration (\U00B5g/mL)",
      fill = "Luminescence (RLU)\n"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9))
  
}
# plot_platemap_compound(filter(lux_data_compounds, compound_name == "Taiwapyrone", biological_rep == 1, organism == "MA", time_point == 72))


plot_platemaps_compounds <- function(data) {
  plots <- data %>%
    filter(time_point %in% c(24, 72),
           !(organism %in% c("MA", "MM") & time_point == 24)
    ) %>%
    group_by(compound_code, organism, biological_rep) %>%
    slice_max(order_by = time_point) %>%
    do(p = plot_platemap_compound(.))
  p <- do.call(getFromNamespace("ggarrange", "ggpubr"), 
               c(plots$p,
                 ncol = n_groups(plots),
                 legend = "none"))
  p + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#111111",
                                                              color = "#111111"))
}

# plot_platemaps_compounds(477, data_compounds_lux)


plot_platemap_hplc <- function(plot_data) {
  if(stringr::str_sub(unique(plot_data$medium), -1) == "B") {
    title_medium <- paste(" in", unique(plot_data$medium))
  } else {
    title_medium <- paste(" on", unique(plot_data$medium))
  }
  
  if(unique(plot_data$modifier) %in% c("none", NA, "mono")) {
    title_mod <- ""
  } else {
    title_mod <- paste ("+", unique(plot_data$modifier))
  }
  
  if(unique(plot_data$modifier_conc) %in% c(0, NA)) {
    title_modconc <- ""
  } else {
    title_modconc <- paste(" ", unique(plot_data$modifier_conc), "\U00B5g/mL")
  }
  
  if(is.na(unique(plot_data$age_days))) {
    title_age_days = ""
  } else {
    title_age_days <- paste("at", unique(plot_data$age_days), "days")
  }
  
  if(unique(plot_data$coculture_organism) %in% c("mono", NA)) {
    title_coculture <- " monoculture"
  } else {
    title_coculture <- paste(" +", unique(plot_data$coculture_organism), "coculture")
  }
  
  plot_title <- paste0(
    "ICMP ", unique(plot_data$icmp_isolate), 
    title_coculture,
    title_medium,
    title_mod,
    title_modconc,
    title_age_days, "  \n",
    "vs *", facet_names[unique(plot_data$organism)], "*",
    " Replicate ", unique(plot_data$biological_rep)
  )
  
  plot_data %>% 
    mutate(
      row_num = 9 - match(row_num, LETTERS), # Plot goes weird without altering row_num
    ) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        x = as.double(col_num),
        y = row_num,
        fill = log10(luminescence)
      ),
      size = 20,
      colour = "#BBBBBB",
      shape = 21
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = as.double(col_num),
        y = row_num,
        label = luminescence,
        colour = well_txt_colour(luminescence)
      ),
      size = 3.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      expand = c(0.06, 0.06)
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      expand = c(0.07, 0.07)
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white")) +
    ggplot2::scale_fill_gradientn(
      colours = rev(grDevices::rainbow(7)),
      limits = c(0, 6),
      breaks = c(0:6),
      labels = c(1, 10, 100, 1000, 10000, 100000, 1000000)
    ) +
    ggplot2::labs(title = plot_title,
                  fill = "Luminescence (RLU)\n") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown()
    )
}

# plot_platemap_hplc(filter(data_hplc_lux, icmp_isolate == 13886, biological_rep == 1, coculture_organism == "mono"))
# plot_platemap_hplc(filter(data_hplc_lux, icmp_isolate == 5500, organism == "SA"))
# plot_platemap_hplc(filter(data_hplc_lux, icmp_isolate == 7766, organism == "EC", plate_number == 1))
plot_platemaps_hplc <- function(data) {
  plots <- data %>% 
    filter(
      time_point >= 24,
      !(organism %in% c("MA", "MM") & time_point == 24)
    ) %>% 
    mutate(time_point = as.integer(time_point)) %>% 
    group_by(
      icmp_isolate, biological_rep, organism, coculture_organism,
      modifier, modifier_conc) %>% 
    slice_max(order_by = time_point) %>% 
    do(p = plot_platemap_hplc(.))
  
  p <- do.call(
    getFromNamespace("ggarrange", "ggpubr"), 
    c(plots$p, ncol = n_groups(plots), legend = "none")
  )
  p + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#111111", color = "#111111")
  )
}

get_tip_labels <- function(isolates) {
  sapply(
    isolates, function(x) {
      filter(
        isolate_info,
        icmp_isolate == as.character(x)
      )[["species_short"]]
      
      paste0("   ICMP ", x, " italic(", filter(
        isolate_info,
        icmp_isolate == as.character(x)
      )[["species_short"]], ")")
    }
  ) %>%
    stringr::str_replace_all("[:blank:]", "~")
}
# plot_type from: "column", "tile", "boxplot"
# Note that x_var and colour_var are flipped when using geom_tile, so x_var is
#   always the continuous variable and colour_var is always discrete
# col_position: use position_stackx() or position_dodgex() from ggtreeExtra
plot_screening_tree <- function(
    plot_data = expt_summaries$summary_screening_lux, plot_type = "column", 
    x_var = "median_log_reduction_auc", y_var = "icmp_isolate", 
    colour_var = "medium", branch_length = "none", activity_threshold = NA, 
    show_labels = F, full_labels = F, layout = "rectangular", offset = 0.1,
    plot_width = 0.5, col_position = ggtreeExtra::position_dodgex(),
    tree_text_size = 4
) {
  library(ggplot2)
  # print(colour_var)
  # print(!!colour_var)
  tiplab_fn <- ifelse(layout == "rectangular", ggtree::geom_tiplab, ggtree::geom_tiplab2)
  
  tree_data <- tree
  if(full_labels) {
    isolates <- tree_data$tip.label
    names(isolates) <- get_tip_labels(isolates)
    isolates <- setNames(names(isolates), isolates)
    tree_data$tip.label <- isolates
    plot_data <- mutate(plot_data, icmp_isolate = get_tip_labels(icmp_isolate))
  }
  
  tree_plot <- ggtree::ggtree(
    tree_data, colour = "black", branch.length = branch_length
  ) +
    tiplab_fn(size = tree_text_size, linetype = "dotted", align = T, colour = "black", parse = T)
  
  if(plot_type == "column") {
    tree_plot <- tree_plot +
      ggtreeExtra::geom_fruit(
        data =  filter(plot_data, .data[[x_var]] > 0),
        geom = geom_col,
        mapping = ggplot2::aes(
          x = !!ensym(x_var),
          y = !!ensym(y_var),
          fill = !!ensym(colour_var)
        ),
        orientation = "y",
        stat = "identity",
        offset = offset,
        position = col_position,
        pwidth = plot_width,
        axis.params = list(
          axis       = "x",
          text.size  = 1.8,
          nbreak     = 3
        ),
        grid.params = list()
      )
  }
  
  if(plot_type == "tile") {
    tree_plot <- tree_plot +
      ggtreeExtra::geom_fruit(
        data = plot_data,
        geom = geom_tile,
        mapping = ggplot2::aes(
          x = !!ensym(colour_var),
          y = !!ensym(y_var),
          fill = !!ensym(x_var)
        ),
        offset = offset,
        pwidth = plot_width,
        axis.params = list(
          axis       = "x",
          text.size  = 1.8,
          nbreak     = 3
        ),
        grid.params = list()
      ) +
      scale_fill_viridis_c()
  }
  
  if(plot_type == "boxplot") {
    tree_plot <- tree_plot +
      ggtreeExtra::geom_fruit(
        data = plot_data,
        geom = geom_boxplot,
        mapping = ggplot2::aes(
          x = !!ensym(x_var),
          y = !!ensym(y_var),
          fill = !!ensym(colour_var)
        ),
        offset = offset,
        pwidth = plot_width,
        axis.params = list(
          axis       = "x",
          text.size  = 1.8,
          nbreak     = 3
        ),
        grid.params = list()
      )
  }
  
  if(layout == "rectangular") {
    tree_plot
  } else if(layout == "circular") {
    tree_plot + ggtree::layout_circular()
  } else if(layout == "fan") {
    tree_plot + ggtree::layout_fan()
  }
}
 
# plot_screening_tree()
# plot_screening_tree(layout = "circular", plot_data = filter(expt_summaries$summary_screening_lux, organism == "EC", age_percent == 100))
# plot_screening_tree(plot_type = "tile", layout = "circular")
# plot_screening_tree(plot_type = "boxplot", plot_data = filter(expt_summaries$summary_screening_lux, organism == "EC", age_percent == 100))

# plot_screening_tree(layout = "circular", plot_data = filter(expt_summaries$summary_extracts, extract %in% expt_metadata$relevant_extracts), x_var = "lowest_mic")
