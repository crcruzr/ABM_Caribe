library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)

#--- Calculate the number of household per "estrato" using an spacial approach

##------- vect files
setwd('..')
getwd()

fl <- list.files('To_Model', pattern = '.shp$', full.names = T)

aoi <-  vect(fl[1]) #areaproject2.shp"
inund <-  vect(fl[3]) #inundacionproject.shp
appl <-  vect(fl[5]) #"MGN_ANM_MANZANA.shp"
applEP2 <- project(appl, crs(aoi))

##------- raster files
rasfil <-list.files('To_Model',
             pattern = '.asc$',
             full.names = T)

r1 <- project(rast(rasfil[2]), crs(applEP2)) #infra_servicios.tif
r1 <-  crop(mask(r1, applEP2), applEP2)
r2 <- project(rast(rasfil[3]), crs(applEP2)) #infra_transporte.tif
r2 <-  crop(mask(r2, applEP2), applEP2)

## proportions of household per pixel
example <- rast("To_Model/Example_raster.asc")
example <- project(example, 'wgs84')

# Create a raster based on AOI with the desired resolution
baseR <- rast(aoi, res = res(example)[1])
baseR[] <- 1:ncell(baseR)
polygons <- mask(as.polygons(baseR, values = TRUE), aoi)
plot(polygons)

# create an empty object (vector)
polygons <- polygons[-(nrow(polygons)), -1]
polygons <- project(polygons, crs(aoi))

applEP2.c <-crop(applEP2, ext(polygons)) # Aoi extension because the shape cover all the country
applEP2.c$areaTot_m2 <- expanse(applEP2.c, unit = "m")

polygons <-cbind(polygons, as.data.frame(matrix(
    NA, nrow(polygons), ncol(applEP2.c) - ncol(polygons))))

names(polygons) <-names(applEP2.c)
polygons$id <- 1:nrow(polygons)
polygons$empty <- NA
polygons$area_m2 <- NA
polygons$area_percent <- NA
polygons2 <- data.frame(id = 1:nrow(polygons),
                        COD_DANE_A = NA,
                        empty = NA,
                        area_m2 = NA,
                        area_percent = NA,
                        areaTot_m2 = NA)

pb <-txtProgressBar(min = 0, max = 500, style = 3)  # style=3 shows percentage
#i<-1228
for (i in 1:nrow(polygons)) {
  #for (i in 1:500) {
  i.1 <- polygons[i,]
  api.1 <- crop(applEP2.c, ext(i.1))
  if (nrow(api.1) == 0) {
    polygons2$id[i] <- i
    polygons2$empty[i] <- 'yes'
  }
  else{
    api.1$area_m2 <- expanse(api.1, unit = "m")
    api.1$area_percent <-
      round((api.1$area_m2 / sum(api.1$area_m2)) * 100, 2)
    mr.i <- as.data.frame(api.1[which.max(api.1$area_percent),])

    polygons2$id[i] <- i
    polygons2$empty[i] <- 'not'
    polygons2$areaTot_m2[i] <- mr.i$areaTot_m2
    polygons2$area_m2[i] <- mr.i$area_m2
    polygons2$area_percent[i] <- mr.i$area_percent
    polygons2$COD_DANE_A[i] <- mr.i$COD_DANE_A

  }
  setTxtProgressBar(pb, i)  # update progress bar
}
close(pb)  # close progress bar when done

df_to_ccHsouse <- right_join(as.data.frame(applEP2), polygons2, by = "COD_DANE_A")
df_to_ccHsouse2 <- df_to_ccHsouse[df_to_ccHsouse$empty == 'not', ]

head(df_to_ccHsouse)
table(df_to_ccHsouse$empty)

## Matrix with: Categories based in the type of "uso", and also the most recurrent category pero polygon and their number of housedholds there.
dfccHouse <- as.data.frame(df_to_ccHsouse2) |>
  mutate(Uso = pmap_chr(list(
      Residencial = TP9_1_USO,
      mixto = TP9_2_USO,
      No_residencial = TP9_3_USO),~ {
      vals <- c(...)        # capture values
      names(vals)[which.max(vals)]  # return name of column with max
    })) |>
  mutate(Estrato = pmap_chr( # extract household category (most frequent)
    list(
      E1 = TP19_EE_E1,
      E2 = TP19_EE_E2,
      E3 = TP19_EE_E3,
      E4 = TP19_EE_E4,
      E5 = TP19_EE_E5,
      E6 = TP19_EE_E6,
      E0 = TP19_EE_E9 ),~ {
      vals2 <- c(...)
      names(vals2)[which.max(vals2)]}),
      householdNumber = pmap_dbl( # extract household number
    list(
      E1 = TP19_EE_E1,
      E2 = TP19_EE_E2,
      E3 = TP19_EE_E3,
      E4 = TP19_EE_E4,
      E5 = TP19_EE_E5,
      E6 = TP19_EE_E6,
      E0 = TP19_EE_E9),~ {
      vals2 <- c(...)
      max(vals2)})) |>
  select(  ## Select only certain celds in the final table
    COD_DANE_A,
    id,
    empty,
    area_m2,
    area_percent,
    areaTot_m2,
    Uso,
    Estrato,
    householdNumber,
    TP19_EE_E1,
    TP19_EE_E2,
    TP19_EE_E3,
    TP19_EE_E4,
    TP19_EE_E5,
    TP19_EE_E6
    )

# N_partial = (A_partial / A_total) * N_total #Proportion of households based in ther propotion in the full area of the polygon
#####
# N_partial = expected number of elements in the partial area
# A_partial = area of the subset
# A_total = total area
# N_total = total number of elements
dfccHouse$mihouseold <- (dfccHouse$area_m2/dfccHouse$areaTot_m2)*dfccHouse$householdNumber

## ---plots to explore the results 

# Table with the polygons that have more than the 80% of the are inside of the pixel
tograph <- dfccHouse |>
  filter(mihouseold > 0,
         area_percent > 80)
tographF <- dfccHouse |>
  filter(mihouseold > 0)



write.csv(tographF, 'tables/pixelapproachFull.csv')
write.csv(tograph, 'tables/pixelapproach_80.csv')


ggplot(tograph, aes(x = Estrato, y = mihouseold)) +
  geom_violin(fill = "skyblue", alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.size = 0.5) +
  labs(x = "Strata", y = "Household number", title = "") +
  theme_minimal()

p1 <-ggplot(tograph, aes(x = Estrato, y = mihouseold)) +
  geom_col(fill = "steelblue") +
  labs(x = "", y = "Frequency", title = "") +
  theme_minimal()

p2 <- ggplot(tograph, aes(x = Estrato, y = mihouseold)) +
  geom_violin(fill = "skyblue", alpha = 0.5, color = "steelblue") +  # violin
  geom_jitter(width = 0.05, alpha = 0.3, color = "#87030382", size = 0.5) +  # all points
  stat_summary(fun = mean, geom = "point", aes(color = "Mean"), size = 4,alpha = 0.9) +
  stat_summary(fun = median, geom = "point", aes(color = "Median"), size = 4, alpha = 0.9) +
  scale_color_manual(name = "", values = c("Mean" = "#FFD700", "Median" = "#1A8703")) +
  labs(x = "", y = "", title = "80% of the polygon in the pixel") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.8, 0.9),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
    ) +
  guides(color = guide_legend(override.aes = list(size = 3)))

flooded <- p1+p2 +  plot_annotation(title = "Household by Estrato - Pixel (80%) approach", theme = theme(plot.title = element_text(hjust = 0.5, face = 'bold')))
 ggsave("plots/HouseholdBStfloodedarea.png", plot = flooded, dpi = 300)

p2b <- ggplot(tographF, aes(x = Estrato, y = mihouseold)) +
  geom_violin(fill = "skyblue", alpha = 0.5, color = "steelblue", show.legend = FALSE) +  # violin
  geom_jitter(width = 0.05, alpha = 0.3, color = "#87030382", size = 0.5) +  # all points
  stat_summary(fun = mean, geom = "point", aes(color = "Mean"), size = 5,alpha = 0.9) +
  stat_summary(fun = median, geom = "point", aes(color = "Median"), size = 5, alpha = 0.9) +
  scale_color_manual(name = "", values = c("Mean" = "#FFD700", "Median" = "#1A8703")) +
  labs(x = "", y = "Number of households", title = "Without restrictions") +
  theme_minimal(base_size = 14) +
   theme(legend.position = "none",plot.title = element_text(hjust = 0.5))

flooded2 <- p2b+p2 +  plot_annotation(title = "", theme = theme(plot.title = element_text(hjust = 0.5, face = 'bold')))
ggsave("plots/HouseholdBStfloodedare_householdproportions.png", plot = flooded2, dpi = 300)

## ----  statistics
df_part <- tograph %>%
  group_by(Estrato) %>%
  summarise(
    nim = min(mihouseold, na.rm = TRUE),
    mean = mean(mihouseold, na.rm = TRUE),
    median = median(mihouseold, na.rm = TRUE),
    max = max(mihouseold, na.rm = TRUE),
    sd = sd(mihouseold, na.rm = TRUE)
  )

write.csv(df_part, 'tables/summary_household_pixelapproach80%.csv')

df_part2 <- tographF %>%
  group_by(Estrato) %>%
  summarise(
    nim = min(mihouseold, na.rm = TRUE),
    mean = mean(mihouseold, na.rm = TRUE),
    median = median(mihouseold, na.rm = TRUE),
    max = max(mihouseold, na.rm = TRUE),
    sd = sd(mihouseold, na.rm = TRUE)
  )
write.csv(df_part2, 'tables/summary_household_pixelapproachFull.csv')


###----- Using all the households 
dfccHouseFull <- as.data.frame(applEP2) |>
  mutate(Uso = pmap_chr(
    list(
      Residencial = TP9_1_USO,
      mixto = TP9_2_USO,
      No_residencial = TP9_3_USO),~ {
      vals <- c(...)        # capture values
      names(vals)[which.max(vals)]})) |>
  mutate(Estrato = pmap_chr( # extract household category (most frequent)
    list(
      E1 = TP19_EE_E1,
      E2 = TP19_EE_E2,
      E3 = TP19_EE_E3,
      E4 = TP19_EE_E4,
      E5 = TP19_EE_E5,
      E6 = TP19_EE_E6,
      E0 = TP19_EE_E9), ~ {
      vals2 <- c(...)
      names(vals2)[which.max(vals2)]}),
  householdNumber = pmap_dbl( # extract household number
    list(
      E1 = TP19_EE_E1,
      E2 = TP19_EE_E2,
      E3 = TP19_EE_E3,
      E4 = TP19_EE_E4,
      E5 = TP19_EE_E5,
      E6 = TP19_EE_E6,
      E0 = TP19_EE_E9),~ {
      vals2 <- c(...)
      max(vals2)}))|>
  select( COD_DANE_A, Uso, Estrato, householdNumber, TP19_EE_E1, TP19_EE_E2, TP19_EE_E3, TP19_EE_E4, TP19_EE_E5, TP19_EE_E6)

tograph2 <- dfccHouseFull |>
  filter(householdNumber > 0)

p3 <- ggplot(tograph2, aes(x = Estrato, y = householdNumber)) +
  geom_col(fill = "steelblue") +
  labs(x = "", y = "Frequency", title = "") +
  theme_minimal()

p4 <- ggplot(tograph2, aes(x = Estrato, y = householdNumber)) +
  geom_jitter(width = 0.05, alpha = 0.3, color = "darkred", size = 0.5) +  # all points
  geom_violin(fill = "skyblue", alpha = 0.5, color = "steelblue") +  # violin
  stat_summary(fun = mean, geom = "point", aes(color = "Mean"), size = 1,alpha = 0.7) +
  stat_summary(fun = median, geom = "point", aes(color = "Median"), size = 1, alpha = 0.7) +
  scale_color_manual(name = "", values = c("Mean" = "red", "Median" = "blue")) +
  labs(x = "", y = "Number of households") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.8, 0.9),
    legend.text = element_text(size = 14)) +
  guides(color = guide_legend(override.aes = list(size = 3)))

full <- p3+p4 +  plot_annotation(title = "Household by Estrato - Full city approach", theme = theme(plot.title = element_text(hjust = 0.5, face = 'bold')))
ggsave("Outputs/HouseholdBStFullCity_plot.png", plot = full, dpi = 300)

# Summary table
df_full_summ <- tograph2 %>%
  group_by(Estrato) %>%
  summarise(
    nim = min(householdNumber, na.rm = TRUE),
    mean= mean(householdNumber, na.rm = TRUE),
    median = median(householdNumber, na.rm = TRUE),
    #mode_area = Mode(householdNumber),
    max = max(householdNumber, na.rm = TRUE),
    sd = sd(householdNumber, na.rm = TRUE)
  )
write_csv(df_full_summ, 'Outputs/summary_household_fullCity.csv')


