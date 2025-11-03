library(terra)
library(tmap)
library(geodata)
library(patchwork)
library(ggplot2)
library(magick)



setwd('/Users/ccruzr/Library/Mobile Documents/com~apple~CloudDocs/Cristian/Documents/Trabajo/IGAC/2025/Inundacion/ABM_Caribe')
getwd()

fl <- list.files('To_Model', pattern = '.shp$', full.names = T)

aoi<-  vect(fl[1]) #areaproject.shp"
inund <-  vect(fl[4]) #inundacionproject.shp
appl <-  vect(fl[4]) #"MANZANA_CAT5.shp"   

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
                                pattern = paste0("type-", type[t], "-mode-Infra and Transport-group-", g, "-NotAv-", NotAv[n],".asc" ),
                                full.names = TRUE)

      # --- Load raster stack and replace 0 with NA ---
      fil <- sum(rast(files_found))
      fil_NA <- classify(fil, cbind(0, NA))
      fil_NA2 <- classify(fil_NA, rcl = matrix(c(-Inf, 0, 0, 0, Inf, g), 
                ncol = 3, byrow = TRUE))
      names(fil_NA2)<- paste0("type-", type[t], "-group-", g, "-NotAv-", NotAv[n] )

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
palette6 <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # green
  "#F0E442", # yellow
  "#D55E00", # dark orange/red
  "#CC79A7"  # magenta
)

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
map_i <- tm_shape(crop(aoi, c(-75.681154, -75.430188, 10.22, 10.57))) +
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


for (i in 1:9) {
  show_legend <- ifelse(i == 9, TRUE, FALSE)  # Only last map shows legend
    map_title   <- names(rastbin)[i]

  # --- Conditional axis titles ---
  x_axis_title <- if (i %in% 1:3) c("0%", "30%", "70%")[i] else ""
  y_axis_title <- if (i %in% c(1, 4, 6)) {
    c("Extensive", "Normative", "Without expansion")[match(i, c(1, 4, 6))]
  } else ""


  map_i <- tm_shape(crop(aoi, c(-75.681154, -75.430188, 10.22, 10.57))) +
    tm_fill(col = "#ebebeb", alpha = 0.7, legend.show = FALSE) +
    tm_shape(appl_merged) +
      tm_fill(col = "USO", palette = c("#999999", "#BDBDBD", "#E0E0E0"),
              alpha = 0.4, lwd = 0, legend.show = FALSE) +
    tm_graticules(
      fill_alpha = 0.2,
      col = '#a7a7a7',
      labels.size = 0.4,
      n.y = 6, n.x = 3,
      labels.cardinal = TRUE,
      labels.inside.frame = FALSE) +
    tm_shape(rastbin[[i]]) +
      tm_raster(
        palette = palette6,
        title = "SocioEconomic Groups",
        legend.show = show_legend,
        labels = c("One", "Two", "Three", "Four", "Five", "Six"),
         breaks = 1:7
      ) +
    tm_layout(
      legend.show = show_legend,
      bg.color = "white"
    )
  
  # ➤ Add compass only for the first map
  if (i == 1) {
    map_i <- map_i + tm_compass(position = c('top', 'left'), size = 3)
  }

  # ➤ Add scalebar only for the ninth (last) map
  if (i == 9) {
    map_i <- map_i +
      tm_add_legend(
        type = "fill",
        labels = c("Urban", "Normative", "Extensive"),
        col = c( "#E0E0E0", "#BDBDBD", "#999999"),
        title = "Expansion areas"
      ) 
  }
  
  # Save each map
  tmap_save(
    map_i, 
    filename = paste0("plots/",names(rastbin[[i]]), ".png"), 
    width = 2000, height = 2000, dpi = 300
  )
}


### Combine plots
img1 <-image_read(paste0("plots/map_1.png"))
img2 <-image_read(paste0("plots/map_2.png"))
img3 <-image_read(paste0("plots/map_3.png"))

combined <- image_append(c(img1, img2, img3, stack = TRUE)
image_write(combined, path = paste0(wd, "plots/maps.jpeg"), density = "300x300")

names(rastbin[[i]])
