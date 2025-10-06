library(terra)
library(tmap)
library(tidyverse)
library(sf)

setwd('..')
getwd()

fl <- list.files('To_Model', pattern = '.shp$', full.names = T)

aoi<-  vect(fl[1]) #areaproject.shp"
inund <-  vect(fl[4]) #inundacionproject.shp
appl <-  vect(fl[5]) #"MANZANA_CA2.shp"   

applT <- as.data.frame(appl)

#Usos redidenciales, además de estratos
appl<- appl[,c('COD_DANE_A','TP9_1_USO',
               'TP9_2_USO', 
               'TP9_3_USO',
                #'TP9_4_USO', # No tenido en cuenta, al no estar en la ciudad
               'TP19_EE_E1',
               'TP19_EE_E2',
               'TP19_EE_E3',
               'TP19_EE_E4',
               'TP19_EE_E5',
               'TP19_EE_E6',
               'TP19_EE_E9')]


applExt <- crop(mask(appl, aoi), aoi)
applExt <- project(applExt, crs(inund))
plot(applExt)
plot(inund)

inter_pol <- crop(mask(applExt, inund),inund )

plot(inter_pol)

#applT3 <- as.data.frame(inter_pol)
applExt$flood <- ifelse( applExt$COD_DANE_A  %in% inter_pol$COD_DANE_A, 1, 0)
#applT2 <- as.data.frame(applExt)
table(applExt$flood)


##Residential
cat <- as.data.frame(applExt) |>
  mutate(Uso = pmap_chr(list(Residencial =TP9_1_USO,
                             mixto = TP9_2_USO, 
                             No_residencial = TP9_3_USO), ~ {
                               vals <- c(...)        # capture values
                               names(vals)[which.max(vals)]  # return name of column with max
                             })) |>  
  mutate(Estrato = pmap_chr(list(E1 = TP19_EE_E1,
                                 E2 = TP19_EE_E2,
                                 E3 = TP19_EE_E3,
                                 E4 = TP19_EE_E4,
                                 E5 = TP19_EE_E5,
                                 E6 = TP19_EE_E6,
                                 E0 = TP19_EE_E9), ~ { ## No definido en el DANE
                                   vals2 <- c(...)
                                   names(vals2)[which.max(vals2)]
                                 }))

 peres<- as.data.frame(table(cat$Estrato))
 peres$per <- round(((peres$Freq * 100)/sum(peres$Freq)),1)

  

 applExt2 <- merge(applExt, cat, by = 'COD_DANE_A')

applExt2 <- applExt2[,c('COD_DANE_A', 'Uso', 'Estrato', 'flood')]
                     
plot(inund)
plot(applExt2, add = T)

applExt2$flood <- round(applExt$flood, 0)

tmap::tm_shape(applExt2) + tmap::tm_borders() + tmap::tm_fill(col = "flood", legend.show = TRUE)
tmap::tm_shape(applExt2) + tmap::tm_borders() + tmap::tm_fill(col = "Estrato", legend.show = TRUE)
tmap::tm_shape(applExt2) + tmap::tm_borders() + tmap::tm_fill(col = "Uso", legend.show = TRUE)

writeVector(applExt2, 'MANZANA_CAT.shp', overwrite = T)

applExt<- vect('MANZANA_CAT.shp')


## Adding new "estratos" shapefile

appl2 <-  vect('estratos/estratos.shp')

plot(inund)
plot(appl2, add = T)

inter_pol2 <- crop(mask(appl2, inund),inund )
appl2$flood <- ifelse( appl2$OBJECTID  %in% inter_pol2$OBJECTID, 1, 0)

cat2 <-as.data.frame(appl2) |>
  mutate(Uso = case_when(
    grepl("RESIDENCIAL", USODESUELO, ignore.case = TRUE) ~ "Residencial",
    grepl("SUELO DE EXPANSION", USODESUELO, ignore.case = TRUE) ~ "Expansion",
    TRUE ~ "Otro"
  )) |>
  select('OBJECTID', 'Uso', 'ESTRATO')

peres2 <- as.data.frame(table(cat2$ESTRATO))
peres2$per <- round(((peres2$Freq * 100)/sum(peres2$Freq)),1)

###
applE2 <- merge(appl2, cat2, by = 'OBJECTID')
applE2 <- applE2[,c('OBJECTID', 'Uso', 'ESTRATO', 'flood')]
applE2$flood <- round(applE2$flood, 0)


tmap::tm_shape(applE2) + tmap::tm_borders() + tmap::tm_fill(col = "flood", legend.show = TRUE)
tmap::tm_shape(applE2) + tmap::tm_borders() + tmap::tm_fill(col = "Estrato", legend.show = TRUE)
tmap::tm_shape(applE2) + tmap::tm_borders() + tmap::tm_fill(col = "Uso", legend.show = TRUE)

## add centroid in each polygon

x <- centroids(applE2, TRUE)
x2 <- crds(x)
applE2 <- cbind(applE2, x2)
plot(x)
names(x)
applE2P<- project(applE2, 'EPSG:9377')

writeVector(applE2, 'MANZANA_CAT2.shp', overwrite = T)
writeVector(applE2P, 'MANZANA_CAT2p.shp', overwrite = T)

writeVector(x, 'MANZANA_CAT2_points.shp', overwrite = T)


#------- Add other dimensions

applE2 <- vect('To_Model/MANZANA_CAT2.shp')
rasfil <- list.files('To_Model/Capas_Raster', pattern = '.tif$', full.names = T)

r1 <- project(rast(rasfil[1]),crs(applE2))
r1 <-  crop(mask(r1, applE2),applE2)

r2 <- project(rast(rasfil[2]),crs(applE2))
r2 <-  crop(mask(r2, applE2),applE2)

plot(r1)
plot(inund, add = T)

#
ro <- applE2[applE2$Uso  %in%  c("Residencial", 'Expansion'), ]

infra_servicios <- terra::extract(r1, ro, exact = FALSE, ID=TRUE, fun = max, na.rm = T)
infra_transporte <- terra::extract(r2, ro, exact = FALSE, ID=TRUE,  fun = max, na.rm = TRUE)

infra_servicios$infra_servicios <- round(infra_servicios$infra_servicios, 0)
infra_transporte$infra_transporte <- round(infra_transporte$infra_transporte, 0)

infra_ID <- data.frame(ID = 1:length(applE2))
infra_ID$infraes <- NA  # Adjust column name as needed
infra_ID$trans <- NA  # Adjust column name as needed

infra_ID[which(applE2$Uso %in%  c("Residencial", 'Expansion')), "infraes"] <- infra_servicios[, 2]  # assuming 2nd column is your value
infra_ID[which(applE2$Uso %in%  c("Residencial", 'Expansion')), "trans"] <- infra_transporte[, 2]  # assuming 2nd column is your value

names(infra_ID)[1] <- "OBJECTID"

applE2 <- merge(applE2, infra_ID, by = 'OBJECTID')

writeVector(applE2, 'MANZANA_CAT3.shp', overwrite = T)
#rm(list = setdiff(ls(), c("r2", "applE2")))
gc()


##Projection's conversion

ap <- vect('areaproject2.shp')
aT <- vect('MANZANA_CAT.shp')
is <- rast('To_Model/infra_servicios.asc')
it <- rast('To_Model/infra_transporte.asc')

app <-project(ap, 'EPSG:9377')
aTp <-project(aT, 'EPSG:9377')
isp <- project(is, 'EPSG:9377') 
itp <- project(it, 'EPSG:9377') 

writeVector(app, 'areaproject2p.shp', overwrite = T)
writeVector(aTp, 'MANZANA_CATp.shp', overwrite = T)
writeRaster(isp, 'infra_serviciosp.asc', overwrite = T)
writeRaster(itp, 'infra_transportep.asc', overwrite = T)

## household 

applE2 <- vect('MANZANA_CAT2p.shp')
applE2Flood <- applE2[applE2$flood %in% 1,]
table(applE2Flood$Estrato != 'E0')

#E0   E1   E2   E3   E4   E5   E6 
#1997 4360  966 1132 1094 1457  906 

## Categories based in Socioeconomic structure

applE <-  vect('10062025Manzanas_Econom/Manzanas_Econom.shp') #Manzanas_Econom.shp
applEP <- vect('MANZANA_CAT2.shp')
crs(applE)

applEP2 <- merge(applEP, applE[, c("OBJECTID", "Cat_econom")], by = "OBJECTID", all.x = TRUE)


tmap::tm_shape(applEP2) + tmap::tm_borders() + tmap::tm_fill(col = "flood", legend.show = TRUE)
tmap::tm_shape(applEP2) + tmap::tm_borders() + tmap::tm_fill(col = "Uso", legend.show = TRUE)
tmap::tm_shape(applEP2) + tmap::tm_borders() + tmap::tm_fill(col = "Cat_econom", legend.show = TRUE)

names(applEP2)[names(applEP2) == "Cat_econom"] <- "GRUPO"
applEP2$GRUPO <- paste0("G", applEP2$GRUPO)

peres3 <- as.data.frame(table(applEP2$GRUPO[applEP2$flood == 1]))
peres3$per <- round(((peres3$Freq * 100)/sum(peres3$Freq)),1)

#  Var1 Freq  per
#1    1  976  8.2
#2    2 4873 40.9
#3    3 1528 12.8
#4    4 1089  9.1
#5    5 1554 13.0
#6    6 1892 15.9

writeVector(applEP2, 'MANZANA_CAT4.shp', overwrite = T)


##Create flooed  raster
tflood <- rast()  # Create an empty raster
tflood <- rast(inund, resolution = res(is), crs = crs(is))

inund$flooded <- 1


tfloodR <- rasterize(inund, tflood, field = 'flooded')
plot(tfloodR)
tfloodRE <-resample(tfloodR, is)
plot(tfloodRE)
freq(tfloodRE)

tfloodRE2 <- ifel(is.na(tfloodRE), 0, tfloodRE)
plot(tfloodRE2)
freq(tfloodRE2)

tfloodRE3 <- mask(tfloodRE2, is)
plot(tfloodRE3)
freq(tfloodRE3)

fsum<- sum(tfloodRE3, tfloodRE, na.rm = TRUE)
plot(fsum)
freq(fsum)

fsum<- ifel(fsum %in% c(1,2), 1,     
     ifel(fsum == 0, 0, NA))
plot(fsum)
freq(fsum)
freq(tfloodRE)
intersect(fsum, tfloodRE)

writeRaster(fsum, filename = paste0('To_Model/floodedRast.asc'), overwrite = TRUE)

