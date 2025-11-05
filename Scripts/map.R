
library(terra)
library(dplyr)
library(tmap)
library(ggplot2)
library(geodata)
library(grid)



setwd('/Users/ccruzr/Library/Mobile Documents/com~apple~CloudDocs/Cristian/Documents/Trabajo/IGAC/2025/Inundacion/ABM_Caribe')
getwd()


fl <- list.files('To_Model', pattern = '.shp$', full.names = T)
fr <- list.files('To_Model', pattern = '.asc$', full.names = T)

aoi<-  vect(fl[1]) #areaproject.shp"
inund <-  rast(fr[[2]])
inund <- classify(inund, cbind(0, NA))
appl <-  vect(fl[5]) #"MANZANA_CA2.shp"   


## raster and vector
colombia <- gadm('COL' , level = 2, path=tempdir())
colo <- gadm('COL' , level = 0, path=tempdir()) # to map

south_america <- geodata::gadm(country = c("ARG","BOL","BRA","CHL","COL","ECU","GUY","PRY","PER","SUR","URY","VEN" ,"PAN"), level = 0, path = "/Users/ccruzr/maps")
suda <- terra::union(south_america)


extM <- c(-75.681154, -75.330188, 10.226815, 10.577781)
bolivar<-colombia[colombia$NAME_1 == 'Bolívar']
cdi <- bolivar[bolivar$NAME_2 == 'Cartagena de Indias']

plot(extM)

appl_merged <- aggregate(appl, by = "USO")

floodAr <-  aggregate(inund, by = "FLOOD")
floodAr <- floodAr[floodAr$FLOOD %in% 1,]
plot(floodAr)

bolivar <- terra::project(bolivar, "EPSG:4326")  
aoi <- terra::project(aoi, "EPSG:4326")

map <- 
  tm_shape(crop(bolivar, c(-75.70, -75.40, 10.20, 10.60))) +
  tm_fill(fill = "#BDBDBD", fill_alpha = 0.7, fill.legend = tm_legend_hide()) +
  tm_shape(aoi) +
  tm_fill(col = "#ebebeb", border.col = "black", fill_alpha = 1) +

tm_shape(inund) +
  tm_raster(fill = c("darkblue"), 
            title = "",   # legend title
            labels = "Flooded area") +
  tm_scale_bar(
    position = c("left", "bottom"),
    text.size = 0.8  # size of the labels
  ) +
  tm_graticules(
    fill_alpha = 0.2,
    col = '#a7a7a7',
    labels.size = 0.4,
    n.y = 6, n.x = 3,
    labels.cardinal = TRUE,
    labels.inside.frame = FALSE
  ) +
  tm_compass(position = c(0.85, 0.95), size = 3) +   # numeric x,y in [0,1]
  
  tm_add_legend(
    type = "fill",
    labels = c( "Study Area", "Departments", "South America"),
    col = c( "#ebebeb", "#BDBDBD", "#BDBDBD"),
    width = 0.35   
  ) +
  tm_layout(
    legend.position = c(0.75, 0.2),
    legend.bg.color = "white",
    legend.bg.alpha = 0.8
  )



#extend colombia
vp_inset <- viewport(x = 0.28, y = 0.82, width = 0.35, height = 0.35)  # numeric values 0–1

sud1 <- 
  tm_shape(crop(colo, c(-82, -34, -56, 13))) +
  tm_fill(col = "#D9D9D9", fill_alpha = 0.6, border.col = "black", fill.legend = tm_legend_hide()) +
  
  tm_shape(suda) +
  tm_polygons(col = "#ebebeb", alpha = 0.7, border.col = NA) +   # South America fill only, no border
  
  tm_shape(colo) +
  tm_polygons(col = "#e7e6e6") +   # Colombia fill on top
  tm_borders(col = "black") +      # Colombia border
  
  tm_shape(as.polygons(ext(-75.681154, -73.85, 8.75, 10.87))) +
  tm_borders(lwd = 3, col = "black") +   # bounding box
  
  tm_grid(alpha = 0.4, col = '#8b8b8b', labels.size = 0.5, lwd = 0.2) +
  tm_layout(bg.color = "white")



tmap_save(map, insets_tm= sud1, insets_vp= vp_inset, filename="plots/StudyArea.jpeg", dpi = 300)


