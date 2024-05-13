
# Libraries ---------------------------------------------------------------
library(forcats)
library(tidyverse)
library(sf)

# Helper functions ----
savefig <- function(plot, filename, device = "png", path = "../plots", width = 6, height = 4, bg = "white") {
  ggsave(
    filename = filename,
    plot = plot,
    path = path,
    device = device,
    width = width,
    height = height,
    bg = bg
  )
}

# Weight fraction vs year plot ----
plot_weight_import_fraction_vs_year <- function(data) {
  data %>%
    ggplot(aes(y = perc, x = fct_rev(year), fill = Weight)) +
    geom_bar(stat = "identity", width = 0.8)  +
    geom_text(aes(label = round(perc * 100), y = label_y), 
              color = "black", 
              size = 3.5) +
    labs(title = "Import of heavy crude oil increased since 2009",
         x = NULL,
         y = "Percent of total") +
    scale_y_continuous(position = "right", 
                       labels = scales::percent_format(),
                       expand = c(0,0)) +
    scale_fill_manual(
      values = c(Light = "darkgray", Medium = "lightgray", Heavy = "orange"),
      breaks = c("Heavy", "Medium", "Light"),
      name = NULL,
    ) +
    theme(
      axis.ticks.length.y = unit(0,'cm'),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(hjust = -0.02, color = "gray30"),
      axis.text.x = element_text(size = 11),
      axis.line.x = element_line(color = "black", size = 0.2),
      plot.title = element_text(margin = margin(b = 10), color = "gray30", hjust = -0.04),
      plot.background = element_blank(),
      plot.margin = unit(c(0.3,0.6,0.3,0.3), "cm"),
      panel.background = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 11)
    ) +
    coord_flip()
}

# Total import over the years ----
plot_total_import <- function(data_import_monthly) {
  data_import_monthly %>%
    group_by(year) %>%
    summarise(total = sum(total)) %>%
    ggplot(aes(x = year, y = total)) +
    geom_bar(stat = "identity", width = 0.8, fill = "royalblue") +
    labs(
      x = NULL,
      y = "Imports in thousands of barrels",
      title = "Total crude oil import decreased between 2009 and 2023"
    ) +
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      breaks = seq(0,4e6,5e5), 
      expand = c(0,0)) +
    coord_cartesian(ylim = c(0,3.5e6)) +
    theme_classic() +
    theme(
      axis.ticks.length.x = unit(0, "cm"),
      axis.ticks.length.y = unit(0, "cm"),
      axis.line.y = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(margin = margin(t = 5)),
      panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
      plot.title = element_text(margin = margin(b = 20), color = "gray30")
    )
}

# Major suppliers of crude oil for 2023 ----
plot_top_10_suppliers_2023 <- function(data_import_country_2023) {
  canada_color <- "orange"
  other_top9_color <- "gray50"
  data_import_country_2023 %>%
    arrange(desc(perc)) %>%
    mutate(rank = rank(perc)) %>%
    mutate(rank = max(rank) + 1 - rank) %>% # Reverse rank order (best=1)
    filter(rank <= 10) %>% # Select top 10 countries
    ggplot(aes(x = perc, y = reorder(originName, perc), fill = factor(ifelse(rank == 1, "highlight", "normal")))) +
    geom_bar(stat = "identity") +
    annotate(
      geom = "text",
      x = 0.45,
      y = "Saudi Arabia",
      label = expression(
        atop(Almost~bold("60%")~of~total~crude~oil~import,
             "in"~2023~came~from~bold(Canada))),
      parse = T,
      color = "gray30") +
    theme_classic() +
    scale_fill_manual(values = c(canada_color, other_top9_color)) +
    guides(fill = "none") +
    labs(
      x = NULL, 
      y = NULL, 
      title = "Top 10 countries in terms of the percentage of crude oil\nimports for the year 2023") +
    scale_x_continuous(
      expand = c(0,0.0),
      labels = scales::percent,
      breaks = seq(0,0.6,0.1),
      position = "top"
    ) + 
    theme(
      axis.ticks.length.y = unit(0, "cm"),
      axis.line.y = element_blank(),
      plot.margin = unit(c(0.3,0.6,0.3,0.3), "cm"),
      plot.title = element_text(margin = margin(b = 10), color = "gray30")
    ) +
    coord_cartesian(xlim = c(0,0.6))
}

# Choropleth map, suppliers by import percent, 2009
plot_oil_suppliers_2009_choropleth <- function(choropleth_data_2009) {
  choropleth_data_2009 %>%
    mutate(prop = total_import / sum(total_import) * 100) %>%
    ggplot() +
    geom_sf(aes(fill = prop), linewidth = 0.1, alpha = 0.9) +
    theme_void() +
    scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      name = "Import percentage",
      guide = guide_legend(
        keyheight = unit(1.5, units = "mm"),
        keywidth = unit(8, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    ) +
    theme(
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(
        size = 12, color = "gray40",
        margin = margin(
          b = -0.1, t = 0.2, l = 0.5,
          unit = "cm"
        )),
      legend.position = c(0.15,0.26)
    ) +
    labs(
      title = "Crude oil suppliers for the year 2009"
    )
}

