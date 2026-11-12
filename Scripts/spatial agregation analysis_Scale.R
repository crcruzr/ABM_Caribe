
library(sf)       # For handling spatial vector data
library(spdep)    # For spatial weights and Moran's I calculations
library(dplyr)
library(vegan)
library(terra)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidyr)

dir_rasters <- file.path(getwd(), "ABM_Caribe", "Outputs")
extension <- "asc"
# Files
files <- list.files(dir_rasters,
                    pattern = paste0("^human-locations-.*\\.", extension, "$"),
                    full.names = TRUE)

# -Names/patterns
pat <- paste0("^human-locations-run-(\\d+)-type-(.+)-mode-(.+)-cpp-(\\d+)-group-(\\d+)\\.", extension, "$")
parts <- str_match(basename(files), pat)

inventory <- data.frame(
  path  = files,
  file  = basename(files),
  run   = as.integer(parts[, 2]),
  type  = parts[, 3],
  mode  = parts[, 4],
  cpp   = as.integer(parts[, 5]),
  group = as.integer(parts[, 6]),
  stringsAsFactors = FALSE
)

# Moran and Shannon metrics for row
calc_metrics <- function(path) {
  r <- rast(path)
  
  moran <- autocor(r, method = "moran", global = TRUE)
  
  v <- values(r, mat = FALSE)
  v <- v[!is.na(v) & v > 0]
  shannon <- diversity(v, index = "shannon")
  
  data.frame(moran = as.numeric(moran), shannon = shannon)
}

# Test
calc_metrics(inventory$path[1])

results

results <- inventory %>%
  mutate(metrics = lapply(path, calc_metrics)) %>%
  unnest(metrics)

dir.create("tables", showWarnings = FALSE)

write.csv(results, "tables/metrics_by_raster.csv", row.names = FALSE)
results<- read.csv('tables/metrics_by_raster_Scale.csv')

head(results)

results %>%
  group_by(type, mode, group, cpp) %>%
  summarise(
    n            = n(),
    moran_median = median(moran, na.rm = TRUE),
    moran_mean   = mean(moran, na.rm = TRUE),
    shannon_median = median(shannon, na.rm = TRUE),
    .groups = "drop"
  )

results %>%
  group_by(type, mode, group, cpp) %>%
  summarise(moran = median(moran, na.rm = TRUE),
            shannon = median(shannon, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cpp, values_from = c(moran, shannon))
names(results)

table(results$cpp)
results$cpp <- ifelse((results$cpp ==1), '25m', '100m')
table(results$cpp)

#plot
results$Group <- factor(results$group)
results$cpp <- factor(results$cpp)
names(results)

pmoran <-
ggplot(results, aes(
  x = cpp, y = moran, color = Group, group = Group)) +
  stat_summary( fun = mean, geom = "point",  position = position_dodge(width = 0.15), size = 1.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  position = position_dodge(width = 0.35), width = 0.15, linewidth = 0.8) +
  facet_grid(mode ~ Group) +

scale_color_manual(values = c("#F8766D","#00BFC4","#619CFF","#7CAE00","#C77CFF","#E69F00"))+
  labs( x = "Scale",  y = "Moran's I",  color = "Group") +

  theme_bw(base_size = 15) +

  theme(strip.background = element_rect(fill = "white", color = "black" ),
    axis.text.x = element_text(angle = 90),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.spacing = unit(0.8, "lines")
  )
pmoran

pshannon <-
ggplot(results, aes(
  x = cpp, y = shannon, color = Group, group = Group)) +
  stat_summary( fun = mean, geom = "point",  position = position_dodge(width = 0.15), size = 1.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  position = position_dodge(width = 0.35), width = 0.15, linewidth = 0.8) +
  facet_grid(mode ~ Group) +

scale_color_manual(values = c("#F8766D","#00BFC4","#619CFF","#7CAE00","#C77CFF","#E69F00"))+
  labs( x = "Scale",  y = "Shannon",  color = "Group") +

  theme_bw(base_size = 15) +

  theme(strip.background = element_rect(fill = "white", color = "black" ),
    axis.text.x = element_text(angle = 90),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.spacing = unit(0.8, "lines")
  )
pshannon

pscale <- (pmoran + theme(legend.position = "none")) + pshannon +
  plot_layout(axes = "collect", axis_titles = "collect")

ggsave(filename = "plots/scale_comparation.png", plot = pscale, width = 3200, height = 2400, units = "px", dpi = 300)
