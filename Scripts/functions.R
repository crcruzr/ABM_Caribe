

theme_paper <- function() {
  theme_bw(base_size = 11, base_family = "Arial") +
    theme(
      # Background
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      # Grid
      panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      # Panel borders
      panel.border = element_rect(colour = "#BDBDBD", fill = NA, linewidth = 0.45),
      # Facet strips
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text = element_text(face = "bold", size = 10, colour = "#333333"),
      # Axes
      axis.text = element_text(size = 9, colour = "#333333"),
      axis.title = element_text(size = 11, face = "bold", colour = "#222222"),
      axis.ticks = element_line(colour = "#444444", linewidth = 0.4),
      axis.line = element_line(colour = "#444444", linewidth = 0.4),
      # Spacing
      panel.spacing = unit(0.8, "lines"),
      plot.margin = margin(8, 10, 8, 10),
      # Legend
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 9)
    )
}

#for summary metrics of Moran and Shannon
summarise_metric <- function(data, var) {
  data %>%
    group_by(Type, NotAv, Group) %>%
    summarise(
      min    = min({{ var }}, na.rm = TRUE),
      mean   = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      max    = max({{ var }}, na.rm = TRUE),
      q25    = quantile({{ var }}, 0.25, na.rm = TRUE),
      q75    = quantile({{ var }}, 0.75, na.rm = TRUE),
      sd     = sd({{ var }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(
      c(min, mean, median, max, q25, q75, sd),
      ~ ifelse(Type %in% c("Sin", "Normal") & Group %in% c(5, 6), NA, .x)
    ))
}

#for figure 5
plot_summary <- function(df, colour, ylab, hide_row_strip = FALSE) {
  p <- ggplot(df, aes(x = factor(Group), y = median)) +
    geom_errorbar(aes(ymin = q25, ymax = q75),
                  colour = colour, width = 0.15, na.rm = TRUE) +
    geom_point(colour = colour, size = 1.8, na.rm = TRUE) +
    facet_grid(Type ~ NotAv, drop = FALSE) +
    labs(x = "Socioeconomic Group", y = ylab) +
    theme_paper()                       # theme first...
  if (hide_row_strip) {
    p <- p + theme(strip.text.y = element_blank(),   # ...tweaks after
                   strip.background.y = element_blank())
  }
  p
}