library(sf)       # For handling spatial vector data
library(spdep)    # For spatial weights and Moran's I calculations
library(dplyr)
library(vegan)
library(terra)
library(ggplot2)
library(patchwork)

# --- To plot ---
source("Scripts/functions.R") 

# --- Define combinations ---
type  <- c("Sin", "Normal", "Extendida")
mode  <- c("Normal","Infra and Transport")
NotAv <- c( "0", "30","70")
group <- 1:6
fl <- list.files('To_Model', pattern = '.shp$', full.names = T)
aoi<-  vect(fl[1]) #areaproject.shp"
aoi<- project(aoi, crs(vect(fl[2])))


base_dir <- paste0(getwd(), '/Outputs/')      # adjust

spatial.results<-as.data.frame(matrix(nrow = 1, ncol = 6))
names(spatial.results) <-c('moran.I', 'shannon.I','Interaction', 'Type',  'NotAv', 'Group')
spatial.results.m <- NULL
t<-1
n <-1
g <-1
row_id <- 1

# --- Moran and Shannon Index ---
spatial.results.m <- data.frame()
total_iter <- length(type) * length(NotAv) * length(group)

pb <- txtProgressBar(min = 0, max = total_iter, style = 3)
counter <- 0

for (t in seq_along(type)) {
  for (n in seq_along(NotAv)) {
    for (g in seq_along(group)) {

      counter <- counter + 1
      setTxtProgressBar(pb, counter)

      files_found <- list.files(path = paste0(base_dir, "Models"),
      pattern = paste0("type-", type[t], "-mode-Infra and Transport-cpp-4-na-", NotAv[n], "-group-", group[g], "\\.asc$"),
      full.names = TRUE)

      if (length(files_found) == 0) next
      for (m in seq_along(files_found)) {
        fil.h <- rast(files_found[m])
        # Moran I
        fil.h.m <- as.numeric(autocor(fil.h, method = "moran", global = TRUE))
        # Shannon
        pop_counts <- values(fil.h, mat = FALSE)
        pop_counts <- pop_counts[!is.na(pop_counts) & pop_counts > 0]
        spatial_shannon_value <- diversity(pop_counts, index = "shannon")

        spatial.results.m <- rbind(
          spatial.results.m,
          data.frame(
            moran.I = fil.h.m,
            shannon.I = spatial_shannon_value,
            Interaction = m,
            Type = type[t],
            NotAv = NotAv[n],
            Group = group[g]
          )
        )

      }
    }
  }
}

close(pb)

spatial.results <- rbind(
  spatial.results,
  spatial.results.m
)

head(spatial.results)
spatial.results.c <- spatial.results #copy
#spatial.results <- spatial.results.c
spatial.results <- spatial.results[-1,]

# Names Standarization
spatial.results$Type <- factor(
  spatial.results$Type,
  levels = c("Extendida", "Normal", "Sin"),
  labels = c("Extensive", "Normal", "Without\nexpansion")
)
table(spatial.results$Type)

# Levels to plot
spatial.results$NotAv <- factor(
  spatial.results$NotAv,
  levels = c("0", "30", "70"),
  labels = c("0%", "30%", "70%")
)
table(spatial.results$NotAv)
write.csv(spatial.results, 'tables/Other/dispersion_measurements.csv')
#spatial.results <- read.csv('tables/Other/dispersion_measurements.csv')

summary_plot.m <- summarise_metric(spatial.results, moran.I)
summary_plot.s <- summarise_metric(spatial.results, shannon.I)

summary_plot.m[summary_plot.m$Type %in% c("Without\nexpansion", "Normal") &
    summary_plot.m$Group %in% c(5, 6),(setdiff(names(summary_plot.m), c("Type", "Group", "NotAv")))] <- NA

summary_plot.s[summary_plot.s$Type %in% c("Without\nexpansion", "Normal") &
    summary_plot.s$Group %in% c(5, 6),(setdiff(names(summary_plot.s), c("Type", "Group", "NotAv")))] <- NA

write.csv(summary_plot.m, "tables/SupMat_Table2.csv", row.names = FALSE)
write.csv(summary_plot.s, "tables/SupMat_Table3.csv", row.names = FALSE)

p5.1 <- plot_summary(summary_plot.m, "#2C7FB8", "Median Moran's I", hide_row_strip = TRUE)
p5.2 <- plot_summary(summary_plot.s, "#b8892c", "Median Shannon's I")

ggsave("plots/other/p5.1.jpeg", p5.1, width = 8, height = 6, units = "in", dpi = 300)
ggsave("plots/other/p5.2.jpeg", p5.2, width = 8, height = 6, units = "in", dpi = 300)

##################
## Null model
##################

## We are obtained the Moran and Shannon H to compare them with the results
base_dir <- paste0(getwd(), '/Outputs/')      # adjust

spatial.results.null <-as.data.frame(matrix(nrow = 1, ncol = 6))
names(spatial.results.null) <-c('moran.I', 'shannon.I','Interaction', 'Type',  'mode', 'Group')
spatial.results.null <- NULL

# Data frame final
spatial.results.null <- data.frame(
  moran.I = numeric(),
  shannon.I = numeric(),
  Interaction = integer(),
  Type = character(),
  mode = character(),
  NotAv = character(),
  Group = integer()
)

pb <- txtProgressBar(min = 0, max = total_iter, style = 3)
counter <- 0

for(t in seq_along(type)){
  for(h in seq_along(NotAv)){
    for(g in seq_along(group)){
      counter <- counter + 1
      setTxtProgressBar(pb, counter)

      files_found <- list.files(
        path = paste0(base_dir, "Models"),
        pattern = paste0("type-", type[t], "-mode-Normal-cpp-4-na-", NotAv[h], "-group-", group[g],"\\.asc$"),
        full.names = TRUE)

      if(length(files_found) == 0) next

      for(m in seq_along(files_found)){
        fil.h <- rast(files_found[m])
        fil.h.mk <- mask(fil.h, aoi)

        # Moran I
        moran_value <- as.numeric(autocor(fil.h.mk, method = "moran", global = TRUE))
        # Shannon
        pop_counts <- values(fil.h.mk, mat = FALSE)
        pop_counts <- pop_counts[!is.na(pop_counts) & pop_counts > 0]

        shannon_value <- diversity(pop_counts, index = "shannon")
        
        spatial.results.null <- rbind(
          spatial.results.null,
          data.frame(
            moran.I = moran_value,
            shannon.I = shannon_value,
            Interaction = m,
            Type = type[t],
            mode = "Normal",
            NotAv = NotAv[h],
            Group = group[g]
          )
        )
      }  # interaction
    }  # group
  }  # NotAv
}  # type

close(pb)

head(spatial.results.null)
table(spatial.results.null$mode)
table(spatial.results.null$Group)
table(spatial.results.null$NotAv)

spatial.results.null.c <- spatial.results.null #copy
#spatial.results.null <- spatial.results.null.c

# Names Standarization
spatial.results.null$Type <- factor(
  spatial.results.null$Type,
  levels = c("Extendida", "Normal", "Sin"),
  labels = c("Extensive", "Normal", "Without\nexpansion")
)
table(spatial.results.null$Type)

# Levels to plot
spatial.results.null$NotAv <- factor(
  spatial.results.null$NotAv,
  levels = c("0", "30", "70"),
  labels = c("0%", "30%", "70%")
)
table(spatial.results.null$NotAv)

write.csv(spatial.results.null, 'tables/Other/dispersion_measurements_Null.csv')
#spatial.results.null <- read.csv('tables/Other/dispersion_measurements_Null.csv')

### Moran's I
summary_plot.m.null <- spatial.results.null %>%
  group_by(Type, mode, Group) %>%
  summarise(
    min=min(moran.I, na.rm = T), 
    mean = mean(moran.I, na.rm = TRUE),
    median = median(moran.I, na.rm = TRUE),
     max =max(moran.I, na.rm = T), 
    q25 = quantile(moran.I, 0.25, na.rm = TRUE),
    q75 = quantile(moran.I, 0.75, na.rm = TRUE),
    sd = sd(moran.I, na.rm = TRUE),
   .groups = "drop"
  ) 

summary_plot.m.null <- summary_plot.m.null %>%
  mutate( min = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6),NA, min),
          mean = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, mean),
          median = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, median),
          max = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, max),
          q25 = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, q25),
          q75 = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, q75),
          sd = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, sd),
        )

summary_plot.m.null[summary_plot.m.null$Type %in% c("Without\nexpansion", "Normal") &
    summary_plot.m.null$Group %in% c(5, 6),(setdiff(names(summary_plot.m.null), c("Type", "Group", "NotAv")))] <- NA

summary_plot.m.null$mode <- 'Normal' 

pSM.3.1 <- ggplot(summary_plot.m.null,
  aes(x = factor(Group), y = median)) +
  geom_errorbar(colour = "#2C7FB8", aes(ymin = q25, ymax = q75), width = 0.15) +
  facet_grid(Type ~ mode, drop = FALSE) +
  labs(x = "Socioeconomic Group", y = "Median Moran's I", colour = "Group") +
  theme(strip.text.y = element_blank(),
    strip.background.y = element_blank())


### Shannon
summary_plot.s.null <- spatial.results.null %>%
  group_by(Type, mode, Group) %>%
  summarise(
    min=min(shannon.I, na.rm = T), 
    mean = mean(shannon.I, na.rm = TRUE),
    median = median(shannon.I, na.rm = TRUE),
     max =max(shannon.I, na.rm = T), 
    q25 = quantile(shannon.I, 0.25, na.rm = TRUE),
    q75 = quantile(shannon.I, 0.75, na.rm = TRUE),
    sd = sd(shannon.I, na.rm = TRUE),
   .groups = "drop"
  ) 

summary_plot.s.null <- summary_plot.s.null %>%
  mutate( min = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6),NA, min),
          mean = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, mean),
          median = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, median),
          max = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, max),
          q25 = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, q25),
          q75 = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, q75),
          sd = ifelse(Type %in% c("Without\nexpansion","Normal") & Group %in% c(5,6), NA, sd),
        )


summary_plot.s.null[summary_plot.s.null$Type %in% c("Without\nexpansion", "Normal") &
    summary_plot.s.null$Group %in% c(5, 6),(setdiff(names(summary_plot.s.null), c("Type", "Group", "NotAv")))] <- NA

summary_plot.s.null$mode <- 'Normal' 

pSM.3.2 <- ggplot(summary_plot.s.null,
  aes(x = factor(Group), y = median)) +
  geom_errorbar(colour = "#2C7FB8", aes(ymin = q25, ymax = q75), width = 0.15) +
  facet_grid(Type ~ mode, drop = FALSE) +
  labs(x = "Socioeconomic Group", y = "Median Shannon's I", colour = "Group") +
  theme(strip.text.y = element_blank(),
    strip.background.y = element_blank())

################################
## comparation Null and previous models
################################

#  we are integrated the dataset to the previous result to see the results in paralel with the previous results

summary_plot.s$mode <- 'Infra and Transport'

summary_plot.s.null.sub <- summary_plot.s.null %>%
    filter(mode == "Normal") 

summary_plot.m.null.sub <- summary_plot.m.null %>%
    filter(mode == "Normal")     

# Moran I
pMMS_join <- ggplot() +
  # Model
  geom_pointrange(data = summary_plot.m,
    aes(
      x = factor(Group), y = median,  ymin = q25,  ymax = q75,  colour = "Behavioural model"),
      position = position_nudge(x = -0.2), linewidth = 0.7, fatten = 1
  ) +
  # Null (orange)
  geom_pointrange(
    data = summary_plot.m.null.sub,
    aes(
      x = factor(Group), y = median,  ymin = q25,  ymax = q75, colour = "Null model"),
        position = position_nudge(x = 0.2), linewidth = 0.4, fatten = 1
  ) +
    scale_colour_manual(
    name = NULL,
    values = c(
      "Behavioural model" = "#3182BD",
      "Null model" = "#E69F00"
    )
  ) +
  facet_grid(Type ~ NotAv) +
  labs(
    x = "Socioeconomic group",
    y = "Median Moran's I"
  ) +
  theme_bw(base_size = 13) +
     theme(
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.spacing = unit(0.8, "lines")
  )

pMMS_join

#shannon
pSMS_join <- ggplot() +
  # Model
  geom_pointrange(data = summary_plot.s,
    aes(
      x = factor(Group), y = median,  ymin = q25,  ymax = q75,  colour = "Behavioural model"),
      position = position_nudge(x = -0.2), linewidth = 0.7, fatten = 1
  ) +
  # Null (orange)
  geom_pointrange(
    data = summary_plot.s.null.sub,
    aes(
      x = factor(Group), y = median,  ymin = q25,  ymax = q75, colour = "Null model"),
        position = position_nudge(x = 0.2), linewidth = 0.4, fatten = 1
  ) +
    scale_colour_manual(
    name = NULL,
    values = c(
      "Behavioural model" = "#3182BD",
      "Null model" = "#E69F00"
    )
  ) +
  facet_grid(Type ~ NotAv) +
  labs(
    x = "Socioeconomic group",
    y = "Median Shannon's H"
  ) +
  theme_bw(base_size = 13) +
   theme(
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.spacing = unit(0.8, "lines")
  )

pSMS_join

modeljoin <- (pSMS_join+theme_paper()) +(pMMS_join+theme_paper()) +  plot_layout(axes = "collect", axis_titles = "collect" )+ plot_annotation(tag_levels = 'A')

ggsave('plots/Fig5.tiff', plot = modeljoin+ plot_annotation(tag_levels = 'A'), width = 8, height = 6, units = "in", dpi = 300)
