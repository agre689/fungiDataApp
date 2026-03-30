options(scipen = 9)

# "Not in"
`%nin%` <- Negate(`%in%`)

# Find number of unique values for a given variable
n_unique <- function(dataset, variable) {
  dataset[[as.character(variable)]] %>% 
    unique() %>% 
    length()
}

order_by_activity <- function(data, x_var, y_var, what = "icmp_isolate") {
  data %>%
    group_by_(x_var) %>%
    summarise(
      median_y = median(.data[[y_var]], na.rm = T)
    ) %>% 
    mutate(
      x = factor(.data[[x_var]]),
      x = forcats::fct_reorder(x, median_y, .na_rm = T)
    ) %>% 
    .$x %>% 
    levels()
}

colnames_to_tags <- function(df){
  lapply(
    colnames(df),
    function(co) {
      tag(
        "p",
        list(
          class = class(df[, co]),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}

var_plurals <- c(
  "medium" = "growth media",
  "phylum" = "phyla",
  "class" = "classes",
  "order" = "orders",
  "family" = "families",
  "genus" = "genera",
  "species_short" = "species",
  "icmp_isolate" = "ICMP isolates"
)

# Use readr coltypes string with readxl
expand_col_types <- function(string) {
  col_types <- c("c" = "text", "d" = "numeric", "l" = "logical", "f" = "text")
  stringr::str_split(string, pattern = "")[[1]] %>% 
    col_types[.] %>% 
    unname()
}

get_organism_short <- function(x) {
  x %>% 
    stringr::word(1:2) %>% 
    stringr::str_sub(1, 1) %>% 
    paste(collapse = "") %>% 
    toupper()
}