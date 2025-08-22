library(terra)      # Load the terra package for spatial data processing

setwd("..")         # Set the working directory one level up

# List all shapefiles in the 'Outputs' folder with full paths
fil <- list.files('Outputs', pattern = '.shp', full.names = TRUE)
filnm <- list.files('Outputs', pattern = '.shp', full.names = FALSE)

# Remove the .shp extension from the filenames
filnm <- gsub('.shp', '', filnm)
filnm <- filnm[c(1:3)]

# Load an existing raster (used for resolution and other parameters)
is <- rast('To_Model/infra_servicios.asc')

# Load the first shapefile as a vector object

  tf <- rast()  # Create an empty raster
  tf <- rast(vec, resolution = res(is), crs = crs(is))

# Loop over all selected shapefiles
for (i in seq_along(filnm)) {
  veci <- vect(fil[1])

  vectmov <- veci[veci$MOVED == 'true']
  tfF <- rasterize(vectmov, tf, fun = sum)
  writeRaster(tfF, filename = paste0('Outputs/', filnm[i], '.tif'), overwrite = TRUE)
  print(i)
  print(minmax(tfF))
}
