library(terra)
library(dplyr)
library(tmap)
library(ggplot2)


fl <- list.files('To_Model', pattern = '.shp$', full.names = T)

aoi<-  vect(fl[1]) #areaproject.shp"
inund <-  vect(fl[4]) #inundacionproject.shp
appl <-  vect(fl[5]) #"MANZANA_CA2.shp"   

map_template <- function(raster_layer, title_text, group_label, aoi) {
  
  tm_shape(raster_layer) +
    tm_raster(palette = "viridis", title = title_text) +   # raster with palette
    tm_shape(aoi) +
    tm_borders(col = "white", lty = "dotted") +           # AOI dotted outline
    tm_layout(bg.color = "black",
              main.title = paste(title_text, "-", group_label))
}

# --- Define combinations ---
type  <- c("Sin", "Normal", "Extendida")
mode  <- "Infra and Transport"
NotAv <- c( "0", "30","70")
group <- 1:6

base_dir <- paste0(getwd(), '/Outputs/')      # adjust
out_dir  <- paste0(base_dir, "Consolidated_Rasters")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

t<-1
n <-1
g <-1
# --- Loop through all combinations ---
for(t in 1:length(type) ){
  for(n in 1:length(NotAv)){
    for(g in 1:length(group)){
      
      # --- List all runs (.asc) for this combination ---
      files_found <- list.files(path = paste0(base_dir, 'Models'),
                                pattern = paste0("type-", type[t], "-mode-Infra and Transport-group-", g, "-NotAv-", NotAv[n],".asc" ),
                                full.names = TRUE)
      if(length(files_found) == 0) next
      
      # --- Load raster stack and replace 0 with NA ---
      fil <- rast(files_found)
      fil_NA <- classify(fil, cbind(0, NA))
      
      # --- Create mask for residential polygons in current group ---
      Gpart <- appl[appl$USO == "residencial" & appl$GRUPO == paste0("G", g)]
      Gpart_mask <- rasterize(Gpart, fil_NA, field = 1, background = NA)
      Gpart_mask[Gpart_mask == 1] <- 0
      
      # --- Compute consolidated rasters ---
      mean_r <- app(fil_NA, fun = function(x) mean(x, na.rm = TRUE))
      sd_r   <- app(fil_NA, sd, na.rm = TRUE)
      cv_r   <- sd_r / mean_r
      
      # --- Apply mask ---
      mean_r[Gpart_mask != 0] <- NA
      sd_r[Gpart_mask != 0]   <- NA
      cv_r[Gpart_mask != 0]   <- NA
      
      # --- Save rasters in the single folder with unique filenames ---
      writeRaster(mean_r, file.path(out_dir, paste0("type-", type[t], "-mode-Infra and Transport-",  "NotAv-", NotAv[n], "group-",  g, "_mean.tif") ), overwrite = TRUE)
      writeRaster(sd_r,   file.path(out_dir, paste0("type-", type[t], "-mode-Infra and Transport-",  "NotAv-", NotAv[n], "group-",  g, "_sd.tif")) ,   overwrite = TRUE)
      writeRaster(cv_r,   file.path(out_dir, paste0("type-", type[t], "-mode-Infra and Transport-",  "NotAv-", NotAv[n], "group-",  g, "_cv.tif")),   overwrite = TRUE)
      
      # --- Compute global statistics ---
      stats <- data.frame(
        type      = type[t],
        NotAv     =NotAv[n],
        group     = g,
        Mean_mean <- global(mean_r, fun = function(x) mean(x, na.rm = TRUE))[1,1],
        Mean_sd   <- global(sd_r,   fun = function(x) mean(x, na.rm = TRUE))[1,1],
        Mean_cv   <- global(cv_r,   fun = function(x) mean(x, na.rm = TRUE))[1,1],
        Min_mean  <- global(mean_r, fun = function(x) min(x, na.rm = TRUE))[1,1],
        Max_mean  <- global(mean_r, fun = function(x) max(x, na.rm = TRUE))[1,1]
      )
      
      # --- Append to master summary table ---
      if(!exists("summary_table_all")){
        summary_table_all <- stats
      } else {
        summary_table_all <- rbind(summary_table_all, stats)
      }
      
    } # group
  } # NotAv
} # type

# --- Save consolidated summary ---
write.csv(summary_table_all, file.path(out_dir, "Consolidated_Summary2.csv"), row.names = FALSE)


mean_stack1 <- rast(list.files(paste0(getwd(), "/Outputs/Consolidated_Rasters"), 
                               pattern = "_mean.tif$", full.names = TRUE))

namesR<- (list.files(paste0(getwd(), "/Outputs/Consolidated_Rasters"), 
                               pattern = "_mean.tif$", full.names = F))
nlyr(mean_stack1)

mean_maps_list1 <- list()

for (i in 1:6) {
  mean_map <- map_template(
    raster_layer = mean_stack1[[i]],
    group_label = "Mean households",
    title_text = paste0("Group ", i),
  aoi = aoi)
  mean_maps_list1[[i]] <- mean_map
}

ll_mapsMeanS1 <- do.call(tmap_arrange, c(mean_maps_list1, ncol = 3))
tmap_save(ll_mapsMeanS1,
          filename = paste0(getwd(), "/plots/",type[1], "/AllGroupsMeanS1.png"),
          width = 12,
          height = 8,
          dpi = 300)

## New approach
# Create an empty list to store all tmap objects
mean_maps_list1 <- list()

# Create all the individual map objects first
for (i in 1:nlyr(mean_stack1)) {
  mean_map <- map_template(
    raster_layer = mean_stack1[[i]],
    group_label = "Mean households",
    title_text = paste0("Group ", i),
    aoi = aoi
  )
  mean_maps_list1[[i]] <- mean_map
}

# Now export maps grouped by type × NotAv
maps_per_row <- 3

index <- 1  # layer counter

for (t in type) {
  for (na in NotAv) {
    # Select the 6 layers corresponding to this combination
    idx <- index:(index + 5)

    # Arrange maps (3 per row, 2 rows total)
    combo_maps <- do.call(tmap_arrange, c(mean_maps_list1[idx], ncol = maps_per_row))

    # Build file name (clean, no mode)
    filename <- paste0(
      getwd(), "/plots/", t,
      "/MeanHouseholds_type-", t, "_NotAv-", na, ".png"
    )
    # Save
    tmap_save(
      combo_maps,
      filename = filename,
      width = 12,
      height = 8,
      dpi = 300
    )
    message("✅ Saved: ", filename)
    # Move to next 6 layers
    index <- index + 6
  }
}

#statistical steps ----------
#Question 1 Do restriction and scenario affect how densely households settle?

t<-1
n <-1
g <-1

for(t in 1:length(type) ){
  for(n in 1:length(NotAv)){
    for(g in 1:length(group)){
      
      # --- List all runs (.asc) for this combination ---
      files_found <- list.files(path = paste0(base_dir, 'Models'),
                                pattern = paste0("type-", type[t], "-mode-Infra and Transport-group-", g, "-NotAv-", NotAv[n],".asc" ),
                                full.names = TRUE)
      if(length(files_found) == 0) next
      
      # --- Load raster stack and replace 0 with NA ---
      fil <- rast(files_found)
      fil_NA <- classify(fil, cbind(0, NA))
      
      # --- Mean per iteraction ---
      mean <- as.data.frame(global(fil_NA, "mean", na.rm=TRUE)) ## i only need to calcualte with the stack
      mean <- mean$mean
      sd_r   <- as.data.frame(global(fil_NA, "sd", na.rm=TRUE))   
      sd_r <- sd_r$sd
      cv_r   <- sd_r / mean

        stats <- data.frame(
        sim = 1:length(mean),
        Mean = mean,
        sd = sd_r,
        cv = cv_r,
       type = type[t],
        NotAv =NotAv[n],
        group     = g)
        # --- Append to master summary table ---
      if(!exists("summary_interactions")){
        summary_interactions <- stats
      } else {
        summary_interactions <- rbind(summary_interactions, stats)
      }
    } # group
  } # NotAv
} # type

nrow(summary_interactions)


summary_interactions$type <- factor(summary_interactions$type,
                                     levels = c( "Sin", "Normal", "Extendida"))
summary_interactions$NotAv <- factor(summary_interactions$NotAv,
                                     levels = c(0, 30, 70))

summary_interactions$group <- factor(summary_interactions$group,
                                     levels = 1:6,
                                     labels = c("G1", "G2", "G3", "G4", "G5", "G6"))

##--------
summary_interactions <- na.omit(summary_interactions)

model <- lm(Mean ~ type * NotAv, data = summary_interactions)
coeftest(model, vcov = vcovHC(model, type = "HC3"))



summary_table <- summary_interactions %>%
  group_by(type, NotAv) %>%
  summarise(mean_occupancy = mean(Mean, na.rm = TRUE)) %>%
  ungroup()

ggplot(summary_table, aes(x = factor(NotAv), y = mean_occupancy, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Restriction (%)", y = "Mean households per pixel", fill = "Scenario") +
  theme_minimal()

## question 2
#Do household groups (G1–G6) react differently to those restrictions?
# Your model
model <- lm(Mean ~ group * type * NotAv, data = summary_interactions)

# Create emmeans objects for each comparison type
emm_type   <- emmeans(model, ~ NotAv | type + group)     # restriction differences within each type
emm_restr  <- emmeans(model, ~ type | NotAv + group)     # type differences within each restriction

groups <- unique(summary_interactions$group)

g<-1
all_results <- list()  # create empty list

for (g in groups) {
  # --- restriction comparisons within type ---
  pairs_type_g <- summary(pairs(emm_type, by = c("type", "group"))) |>
    subset(group == g)
  
  if (nrow(pairs_type_g) > 0) {
    pairs_type_g <- pairs_type_g |>
      transform(comparison_type = "Restriction_within_Type") |>
      rename(type_NotAv = "type")
  }
  
  # --- type comparisons within restriction ---
  pairs_restr_g <- summary(pairs(emm_restr, by = c("NotAv", "group"))) |>
    subset(group == g)
  
  if (nrow(pairs_restr_g) > 0) {
    pairs_restr_g <- pairs_restr_g |>
      transform(comparison_type = "Type_within_Restriction") |>
      rename(type_NotAv = "NotAv")
  }
  
  # --- Combine both (only if at least one exists) ---
  group_results <- rbind(
    if (exists("pairs_type_g")) pairs_type_g,
    if (exists("pairs_restr_g")) pairs_restr_g
  )
  
  if (!is.null(group_results) && nrow(group_results) > 0) {
    all_results[[g]] <- group_results
  }
}

# --- Combine all groups into one table ---
results_all_groups <- do.call(rbind, all_results)
results_all_groups <- na.omit(results_all_groups)

write.csv(results_all_groups, 'tables/pvalue_pergroups_contrast_type_NotAV.csv')

key_results <- results_all_groups %>%
  group_by(group, comparison_type) %>%
  slice_min(p.value, n = 1)

write.csv(key_results, 'tables/pvalue_pergroups_contrast_type_NotAV_Keyresults.csv')
