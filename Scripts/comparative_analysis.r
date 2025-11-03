library(terra)
library(tmap)

# --- List of scenarios ---
getwd()
scenarios <- c("Sin(InfraTrans-0)/", "Normal(InfraTrans-0)/", "Extendida(InfraTrans-0)/")
wdir <- '/Outputs/Variando un solo parametro/Expansion/Infra trans/'
all_summaries <- list()
w<-3
for (w in 1:length(scenarios)) {

  cat("Processing scenario:", scenarios[w], "\n")
  
  # --- Initialize lists ---
  summary_list <- list()
  mean_maps_list <- list()

  # --- Loop through groups with data (G1-G5) ---
  for (i in 1:6) {

    GT <- paste0("G", i)

    # --- List files for this group in current scenario ---
    files_found <- list.files(paste0(getwd(), wdir, scenarios[w]),
            pattern = paste0('group-', i, ".asc$"), full.names = T)

    if (length(files_found) == 0) {
      warning(paste("No files found for", GT, "in scenario", wdir))
      next
    }

    # --- Load raster stack ---
    fil <- rast(files_found)
    fil_NA <- classify(fil, cbind(0, NA))

    # --- Create mask for current group ---
    Gpart <- appl[appl$USO == "residencial" & appl$GRUPO == GT]
    Gpart_mask <- rasterize(Gpart, fil_NA, field = 1, background = NA)
    Gpart_mask[Gpart_mask == 1] <- 0

    # --- Compute raster summaries ---
    mean_r <- app(fil_NA, mean, na.rm = TRUE)
    sd_r   <- app(fil_NA, sd, na.rm = TRUE)
    cv_r   <- sd_r / mean_r

    # --- Apply mask ---
    mean_r[Gpart_mask != 0] <- NA
    sd_r[Gpart_mask != 0]   <- NA
    cv_r[Gpart_mask != 0]   <- NA

    # --- Save rasters ---
    out_dir <- paste0(getwd(), wdir, scenarios[w], "Summary")
    if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    writeRaster(mean_r, file.path(out_dir, paste0(GT, "_mean.tif")), overwrite = TRUE)
    writeRaster(sd_r,   file.path(out_dir, paste0(GT, "_sd.tif")),   overwrite = TRUE)
    writeRaster(cv_r,   file.path(out_dir, paste0(GT, "_cv.tif")),   overwrite = TRUE)

    # --- Compute global statistics ---
    stats <- data.frame(
      Group     = GT,
      Mean_mean = global(mean_r, mean, na.rm = TRUE)[1,1],
      Mean_sd   = global(sd_r,   mean, na.rm = TRUE)[1,1],
      Mean_cv   = global(cv_r,   mean, na.rm = TRUE)[1,1],
      Min_mean  = global(mean_r, min,  na.rm = TRUE)[1,1],
      Max_mean  = global(mean_r, max,  na.rm = TRUE)[1,1]
    )
    summary_list[[length(summary_list) + 1]] <- stats

    # --- Create Mean map using template ---
    mean_maps_list[[i]] <- map_template(
      raster_layer = mean_r,
      title_text   = "Mean households",
      group_label  = GT,
      bbox_extent  = ext(aoi)
    )

  }
  df <- do.call(rbind, summary_list)
  all_summaries[[w]] <- df
}

  # --- Combine summaries and save CSV ---
summary_table_all <- do.call(rbind, all_summaries)
summary_table_all$Scenario <- rep(
  c('Sin expansion', 'Moderate expansion', 'Extensive expansion'),
  each = 6
)
  write.csv(summary_table_all, paste0(getwd(), wdir, "Group_Summary_Table.csv"), row.names = FALSE)

## without expansion
mean_stack1 <- rast(list.files(paste0(getwd(), wdir, scenarios[1],"/Summary/"), pattern = "_mean.tif$", full.names = TRUE))
mean_maps_list1 <- list()
for (i in 1:5) {  
    mean_map <- map_template(mean_stack1[[i]], 
                           "Mean households", 
                           paste0("Group ", i, "") 
                           )   
  mean_maps_list1[[i]] <- mean_map
}
ll_mapsMeanS1 <-  do.call(tmap_arrange, c(mean_maps_list1, ncol = 3))

tmap_save(ll_mapsMeanS1, 
          filename =paste0(getwd(), wdir, scenarios[1], "/Summary/AllGroupsMeanS1.png"),
          width = 12,   # inches
          height = 8,
          dpi = 300)


## Moderate expansion
mean_stack2 <- rast(list.files(paste0(getwd(), wdir, scenarios[2],"/Summary/"), pattern = "_mean.tif$", full.names = TRUE))
mean_maps_list2 <- list()
for (i in 1:5) {  
    mean_map <- map_template(mean_stack2[[i]], 
                           "Mean households", 
                           paste0("Group ", i, "") 
                           )   
  mean_maps_list2[[i]] <- mean_map
}
ll_mapsMeanS2 <-  do.call(tmap_arrange, c(mean_maps_list2, ncol = 3))

tmap_save(ll_mapsMeanS2, 
          filename =paste0(getwd(), wdir, scenarios[2], "/Summary/AllGroupsMeanS2.png"),
          width = 12,   # inches
          height = 8,
          dpi = 300)

## Extensive expansion
mean_stack3 <- rast(list.files(paste0(getwd(), wdir, scenarios[3],"/Summary/"), pattern = "_mean.tif$", full.names = TRUE))
mean_maps_list3 <- list()
for (i in 1:6) {  
    mean_map <- map_template(mean_stack3[[i]], 
                           "Mean households", 
                           paste0("Group ", i, "") 
                           )   
  mean_maps_list3[[i]] <- mean_map
}
ll_mapsMeanS3 <-  do.call(tmap_arrange, c(mean_maps_list3, ncol = 3))

tmap_save(ll_mapsMeanS3, 
          filename =paste0(getwd(), wdir, scenarios[3], "/Summary/AllGroupsMeanS3.png"),
          width = 12,   # inches
          height = 8,
          dpi = 300)