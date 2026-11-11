

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
