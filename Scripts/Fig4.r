library(terra)
library(tmap)
library(geodata)
library(patchwork)
library(ggplot2)
library(magick)

setwd('/Users/ccruzr/Library/Mobile Documents/com~apple~CloudDocs/Cristian/Documents/Trabajo/IGAC/2025/Inundacion/ABM_Caribe')
getwd()

fl <- list.files('To_Model', pattern = '.shp$', full.names = T)

aoi <-  vect(fl[1]) #areaproject2p.shp"
inund <-  vect(fl[2]) #inundacionproject.shp
appl <-  vect(fl[3]) #"MANZANA_CAT6.shp"   
aoi2 <- project(aoi, crs(inund))
plot(aoi2)
plot(inund, add= T)

type  <- c("Sin", "Normal", "Extendida")
mode  <- "Infra and Transport"
NotAv <- c( "0", "30","70")
group <- 1:6

t<-1
n <-1
g <-1

base_dir <- paste0(getwd(), '/Outputs/')      # adjust
out_dir <- 'Outputs/Models/Consolidated_RastersBin/'
# --- Loop through final binary maps ---

for(t in 1:length(type) ){
  for(n in 1:length(NotAv)){
    comb <- list()
    for(g in 1:length(group)){
    # --- List all runs (.asc) for this combination ---
      files_found <- list.files(path = paste0(base_dir, 'Models'),
                                pattern = paste0("type-", type[t], "-mode-Infra and Transport-cpp-4-na-", NotAv[n],"-group-", g,".asc" ),
                                full.names = TRUE)

      # --- Load raster stack and replace 0 with NA ---
      fil <- sum(rast(files_found))
      fil_NA <- classify(fil, cbind(0, NA))
      fil_NA2 <- classify(fil_NA, rcl = matrix(c(-Inf, 0, 0, 0, Inf, g), 
                ncol = 3, byrow = TRUE))
      names(fil_NA2) <- paste0("type-", type[t], "-mode-Infra and Transport-cpp-4-na-", NotAv[n], "-group-", g )

     comb[[g]] <- fil_NA2
    } # group
  r_comb <- sum(rast(comb),na.rm = T)
  nm<- paste0("type-", type[t], "-NotAv-", NotAv[n] )
  names(r_comb) <-nm
  writeRaster(r_comb, paste0(out_dir, 'bin', nm, '.tif'), overwrite=TRUE)
  } # NotAv
} # type

##load Maps 
rastbin <- rast(list.files((out_dir), full.names = T))

###Plot it
palette6 <- c("#00204DFF", "#31446BFF", "#666970FF", "#958F78FF", "#CBBA69FF", "#FFEA46FF")

plot(rastbin[[1]], col = palette6, axes = FALSE, box = FALSE)

## raster and vector
colombia <- gadm('COL' , level = 2, path=tempdir())
colo <- gadm('COL' , level = 0, path=tempdir()) # to map
extM <- c(-75.681154, -75.330188, 10.226815, 10.577781)
bolivar<-colombia[colombia$NAME_1 == 'Bolívar']
cdi <- bolivar[bolivar$NAME_2 == 'Cartagena de Indias']

plot(extM)
appl_merged <- aggregate(appl, by = "USO")

# Map 1
## only one map
map_i <- tm_shape(crop(aoi2, c(-75.681154, -75.430188, 10.22, 10.57))) +
    tm_fill(col = "#ebebeb", alpha = 0.7, legend.show = FALSE) +
    tm_shape(appl_merged) +
      tm_fill(col = "USO", palette = c("#E69F00", "#56B4E9", "#009E73"),
              alpha = 0.4, lwd = 0, legend.show = FALSE) +
tm_graticules(
  fill_alpha = 0.2,
  col = '#a7a7a7',
  labels.size = 0.4,
  n.y = 6, n.x = 3,
  labels.cardinal = TRUE,
  labels.inside.frame = FALSE) +
tm_shape(rastbin[[1]]) +
tm_raster(palette = palette6, title = "Raster Categories",
                legend.show = T) +
tm_layout(
      legend.show = T,
      #main.title = nmmaps[i],
      bg.color = "white")
## all the maps

## reorder
names(rastbin)
rastbin <- rastbin[[c(7,8,9,4,5,6,1,2,3)]]
names(rastbin)

# Store all maps in a list
map_list <- vector("list", 9)
tmap_options(
  component.autoscale = TRUE
)
for (i in 1:9) {
  show_legend <- (i == 9)
  map_title <- names(rastbin)[i]
  
  map_i <- tm_shape(crop(aoi2, c(-75.681154, -75.430188, 10.22, 10.57))) +
    tm_fill(
      col = "#ebebeb",
      alpha = 0.7,
      legend.show = FALSE
    ) +
    tm_shape(appl_merged) +
    tm_fill(
      col = "USO",
      palette = c("#999999", "#BDBDBD", "#E0E0E0"),
      alpha = 0.4,
      lwd = 0,
      legend.show = FALSE
    ) +
    tm_graticules(
      fill_alpha = 0.2,
      col = "#a7a7a7",
      labels.size = 0.4,
      n.y = 6,
      n.x = 3,
      labels.cardinal = TRUE,
      labels.inside.frame = FALSE
    ) +
    tm_shape(rastbin[[i]]) +
    tm_raster(
      palette = palette6,
      legend.text.size = 2,
      title = "SocioEconomic\n Groups",
      legend.show = show_legend,
      labels = c("One", "Two", "Three",
                 "Four", "Five", "Six"),
      breaks = 1:7
    ) +

tm_layout(
  legend.show = show_legend,
  legend.text.size = 1.5,
  legend.title.fontface = "bold",
  bg.color = "white",
   meta.margins = c(0, 0, 0, 0.2)
)

  if (i == 1) {
    map_i <- map_i +
      tm_compass(position = c(0.18, 0.93), size = 3)
  }
  # Additional legend on last map
  if (i == 9) {
    map_i <- map_i +
      tm_add_legend(
        type = "fill",
        labels = c("Urban", "Normative", "Extensive"),
        col = c("#E0E0E0", "#BDBDBD", "#999999"),
        title = "Expansion areas"
      )
      
  }
    map_list[[i]] <- map_i
}

## Add letters to plot
panel_letters <- paste0("(", letters[1:9], ")")

for (i in seq_along(map_list)) {
  map_list[[i]] <- map_list[[i]] +
    tm_credits(
      panel_letters[i],
      position = c(0.01, 0.98),
      size = 1.3,
      fontface = "bold",
      color = "black",
      bg = FALSE
    )
}

# Arrange all nine maps
final_figure <- tmap_arrange(
  map_list,
  ncol = 3,
  nrow = 3
)

# Export as one high-resolution figure
tmap_save(
  final_figure,
  filename = "plots/Fig4.tiff",
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)


