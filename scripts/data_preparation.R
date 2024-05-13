# Libraries ---------------------------------------------------------------
library(forcats)
library(tidyverse)

# Light/Medium/Heavy import fraction ----
prepare_data_weight_fractions <- function(oil_tb) {
  oil_tb_tansformed <-
    oil_tb %>%
    mutate(Weight = case_when(
      grepl("Heavy", gradeName) ~ "Heavy",
      grepl("Light", gradeName) ~ "Light",
      TRUE ~ "Medium"
    )) %>%
    mutate(Weight = factor(Weight, levels = rev(c( "Heavy", "Medium", "Light")))) %>% # Factorize for plotting in proper order
    group_by(year, Weight) %>%
    summarise(total_import = sum(quantity)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(perc = total_import / sum(total_import)) %>%
    mutate(year = factor(year)) %>%
    arrange(year, rev(Weight)) %>%
    mutate(label_y = cumsum(perc) - 0.5 * perc)
  return(oil_tb_tansformed)
}

# Total import between 2009 and 2023 ----
prepare_data_total_import_monthly <- function(oil_tb) {
  oil_tb_tansformed <-
    oil_tb %>%
    mutate(year = factor(year)) %>%
    group_by(year, month) %>%
    summarize(total = sum(quantity))
  return(oil_tb_tansformed)
}

# Oil import by country ----
prepare_data_oil_import_by_country <- function(oil_tb) {
  oil_import_by_country <- 
    oil_tb %>%
    group_by(year, originName) %>%
    summarise(total_import = sum(quantity)) 
  return(oil_import_by_country)
}

# Major suppliers in 2023 by percentage import ----
prepare_data_major_suppliers_2023 <- function(oil_tb) {
  oil_tb_transformed <-
    oil_tb %>%
    filter(year == 2023) %>%
    group_by(originName) %>%
    summarise(total = sum(quantity)) %>%
    mutate(perc = total / sum(total))
  return(oil_tb_transformed)
}

# Choropleth oil suppliers 2009 ----
get_world_geojson <- function() {
  tmp_geojson <- tempfile(fileext = ".geojson")
  
  download.file(
    "https://r2.datahub.io/clvyjaryy0000la0cxieg4o8o/master/raw/data/countries.geojson",
    tmp_geojson
  )
  world_sf <- read_sf(tmp_geojson)
  return(world_sf)
}

prepare_data_suppliers_2009_choropleth <- function(oil_import_by_country, world_sf) {
  world_sf_merged <-
    world_sf %>%
    filter(ADMIN != "Antarctica") %>%
    left_join(filter(oil_import_by_country, year == 2009), by = c("ADMIN" = "originName")) %>%
    mutate(total_import = ifelse(is.na(total_import), 0, total_import))
  return(world_sf_merged)
}















