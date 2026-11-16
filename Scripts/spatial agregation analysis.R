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
#spatial.results <- read.csv('tables/dispersion_measurements.csv')

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

##################
## Null model
##################

## We are obtained the Moran and Shannon H to compare them with the results

base_dir <- paste0(getwd(), '/Outputs/')      # adjust
out_dir  <- paste0(base_dir, "Random_model")

spatial.results.null <-as.data.frame(matrix(nrow = 1, ncol = 6))
names(spatial.results.null) <-c('moran.I', 'shannon.I','Interaction', 'Type',  'mode', 'Group')
spatial.results.null <- NULL


t<-1
h <-1
g <-1
row_id <- 1
m<- 1
for(t in 1:length(type)){
  for(h in 1:length(mode)){
    for(g in 1:length(group)){   
# Load or create a SpatRaster
      files_found <- list.files(path = paste0(base_dir, 'Random_model'),
                                pattern = paste0("type-", type[t],"-mode-",  mode[h], "-cpp-4-group-", g,".asc" ),
                                full.names = TRUE)
if(length(files_found) == 0) next
#fil <- rast(files_found)
    for(m in seq_along(files_found)){
      fil.h <- rast(files_found[m])
      fil.h.mk<-mask(fil.h, aoi)
      ## -- Moran I. analysis
      fil.h.mk2 <- autocor(fil.h.mk, method = "moran", global = TRUE)
  
      ## -- Shannon analysis
      pop_counts <- values(fil.h, mat = FALSE)
      pop_counts <- pop_counts[!is.na(pop_counts) & pop_counts > 0]
      
      pop_counts <- values(fil.h.mk, mat = FALSE)
      pop_counts <- pop_counts[!is.na(pop_counts) & pop_counts > 0]
      spatial_shannon_value <- diversity(pop_counts, index = "shannon")

  spatial.results.m <- rbind(
  spatial.results.m,
  data.frame(
    moran.I = fil.h.mk2,
    shannon.I = spatial_shannon_value,
    Interaction = m,
    Type = type[t],
    mode = mode[h],
    Group = g
  )
)
      }  # interaction

    }  # group

  } # percentage
spatial.results.null <- rbind(spatial.results.null, spatial.results.m)
} 
spatial.results.null <-spatial.results.null[-1,]
head(spatial.results.null)
table(spatial.results.null$mode)
table(spatial.results.null$Group)

spatial.results.null$Type <- factor(
  spatial.results.null$Type,
  levels = c("Extendida", "Normal", "Sin"),
  labels = c("Extensive", "Normal", "Without\nexpansion")
)
table(spatial.results.null$Type)

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
  mutate( min = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6),NA, min),
          mean = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, mean),
          median = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, median),
          max = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, max),
          q25 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q25),
          q75 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q75),
          sd = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, sd),
        )

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
  mutate( min = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6),NA, min),
          mean = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, mean),
          median = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, median),
          max = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, max),
          q25 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q25),
          q75 = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, q75),
          sd = ifelse(Type %in% c("Sin","Normal") & Group %in% c(5,6), NA, sd),
        )

pSM.3.2 <- ggplot(summary_plot.s.null,
  aes(x = factor(Group), y = median)) +
  geom_errorbar(colour = "#2C7FB8", aes(ymin = q25, ymax = q75), width = 0.15) +
  facet_grid(Type ~ mode, drop = FALSE) +
  labs(x = "Socioeconomic Group", y = "Median Shannon's I", colour = "Group") +
  theme(strip.text.y = element_blank(),
    strip.background.y = element_blank())

pSM.3.1 +pSM.3.2
head(summary_plot.s.null)
p5 <- (p5.1+theme_paper()) + (p5.2 +theme_paper())+ plot_annotation(tag_levels = 'A')

pSM.3<- (pSM.3.1+theme_paper()) + (pSM.3.2 +theme_paper())+ plot_annotation(tag_levels = 'A')

moranR <- (p5.1+theme_paper()) +(pSM.3.1+theme_paper()) +  plot_layout(axes = "collect", axis_titles = "collect")+ plot_annotation(tag_levels = 'A')
ggsave('plots/SM3_Moran.png', plot = moranR, width = 8, height = 6, units = "in", dpi = 300)

shannonR <- (p5.2+theme_paper()) +(pSM.3.2+theme_paper()) +  plot_layout(axes = "collect", axis_titles = "collect" )+ plot_annotation(tag_levels = 'A')

ggsave('plots/shan_moran.png', plot = (shannonR / moranR)+ plot_annotation(tag_levels = 'A'), width = 8, height = 6, units = "in", dpi = 300)

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

ggsave('plots/shan_moranJoin.png', plot = modeljoin+ plot_annotation(tag_levels = 'A'), width = 8, height = 6, units = "in", dpi = 300)
