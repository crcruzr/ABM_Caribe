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
mode  <- "Infra and Transport"
NotAv <- c( "0", "30","70")
group <- 1:6

base_dir <- paste0(getwd(), '/Outputs/')      # adjust
out_dir  <- paste0(base_dir, "Consolidated_Rasters")

spatial.results<-as.data.frame(matrix(nrow = 1, ncol = 6))
names(spatial.results) <-c('moran.I', 'shannon.I','Interaction', 'Type',  'NotAv', 'Group')
spatial.results.m <- NULL
t<-1
n <-1
g <-1
row_id <- 1

for(t in 1:length(type)){
  for(n in 1:length(NotAv)){
    for(g in 1:length(group)){   
# Load or create a SpatRaster
      files_found <- list.files(path = paste0(base_dir, 'Models'),
                                pattern = paste0("type-", type[t], "-mode-Infra and Transport-group-", g, "-NotAv-", NotAv[n],".asc" ),
                                full.names = TRUE)
if(length(files_found) == 0) next
#fil <- rast(files_found)
    for(m in seq_along(files_found)){
      ## -- Moran I. analysis
      fil.h <- rast(files_found[m])
      fil.h.m <- autocor <- autocor(fil.h, method = "moran", global = TRUE)
      ## -- Shannon analysis
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
    Group = g
  )
)
      }  # interaction

    }  # group

  } # percentage
spatial.results <- rbind(spatial.results, spatial.results.m)
} # type

spatial.results.c <- spatial.results
#spatial.results <- spatial.results.c

spatial.results <- spatial.results[-1,]


spatial.results$Type <- factor(
  spatial.results$Type,
  levels = c("Extendida", "Normal", "Sin"),
  labels = c("Extensive", "Normal", "Without\nexpansion")
)
table(spatial.results$Type)

spatial.results$NotAv <- factor(
  spatial.results$NotAv,
  levels = c("0", "30", "70"),
  labels = c("0%", "30%", "70%")
)
table(spatial.results$NotAv)
write.csv(spatial.results, 'tables/dispersion_measurements.csv')
spatial.results <- read.csv('tables/dispersion_measurements.csv')

## Moran I. results
summary_plot.m <- spatial.results %>%
  group_by(Type, NotAv, Group) %>%
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

summary_plot.m <- summary_plot.m %>%
  mutate( min = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6),NA, min),
          mean = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, mean),
          median = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, median),
          max = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, max),
          q25 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q25),
          q75 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q75),
          sd = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, sd),
        )

write.csv(summary_plot.m, 'tables/Table_S2.csv')

#summary_plot.m <- summary_plot.m[!is.na(summary_plot.m$min), ]

p5.1 <- ggplot(summary_plot.m,
  aes(x = factor(Group), y = median)) +
  geom_errorbar(colour = "#2C7FB8", aes(ymin = q25, ymax = q75), width = 0.15) +
  facet_grid(Type ~ NotAv, drop = FALSE) +
  labs(x = "Socioeconomic Group", y = "Median Moran's I", colour = "Group") +
  theme(strip.text.y = element_blank(),
    strip.background.y = element_blank())

## Shannon Index results
summary_plot.s <- spatial.results %>%
  group_by(Type, NotAv, Group) %>%
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

summary_plot.s <- summary_plot.s %>%
  mutate( min = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6),NA, min),
          mean = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, mean),
          median = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, median),
          max = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, max),
          q25 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q25),
          q75 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q75),
          sd = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, sd),
        )
write.csv(summary_plot.s, 'tables/Table_S3.csv')

p5.2 <- ggplot(summary_plot.s,
  aes(x = factor(Group), y = median)) +
  geom_errorbar(colour = "#b8892c", aes(ymin = q25, ymax = q75), width = 0.15) +
  facet_grid(Type ~ NotAv, drop = FALSE) +
  labs(x = "Socioeconomic Group", y = "Median Sannon's I", colour = "Group")
  

ggsave('plots/p5.1.png', plot = p5.1, width = 8, height = 6, units = "in", dpi = 300)
ggsave('plots/p5.2.png', plot = p5.2, width = 8, height = 6, units = "in", dpi = 300)

p5 <- (p5.1+theme_paper()) + (p5.2 +theme_paper())+ plot_annotation(tag_levels = 'A')

ggsave('plots/p5.png', plot = p5, width = 8, height = 6, units = "in", dpi = 300)

