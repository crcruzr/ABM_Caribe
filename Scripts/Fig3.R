
library(terra)
library(tmap)
library(tidyverse)
library(sf)

#setwd('..')
getwd('')
applEP <- vect('To_model/MANZANA_CAT6.shp')
applE <-  vect('To_model/10062025Manzanas_Econom/Manzanas_Econom.shp') #Manzanas_Econom.shp
crs(applE)

applEP2 <- merge(applEP, applE[, c("OBJECTID", "Cat_econom")], by = "OBJECTID", all.x = TRUE)

names(applEP2)[names(applEP2) == "Cat_econom"] <- "GRUPO2"
applEP2$GRUPO2 <- paste0("G", applEP2$GRUPO2)
setdiff(applEP2$GRUPO, applEP2$GRUPO2)

peres3 <- as.data.frame(table(applEP2$GRUPO[applEP2$FLOOD == 1]))
peres3$per <- round(((peres3$Freq * 100)/sum(peres3$Freq)),1)

#  Var1 Freq  per
#1    1  976  7.8
#2    2 4873 40.6
#3    3 1528 12.6
#4    4 1089  9.2
#5    5 1554 13.4
#6    6 1892 16.5

peresF <- as.data.frame(table(applEP2$GRUPO))
peresF$per <- round(((peresF$Freq * 100)/sum(peresF$Freq)),1)

# Add a label to each dataset
peres3$dataset <- "Flooded\nareas"
peresF$dataset <- "Total\npopulation"

# Combine into one dataframe
df_plot <- rbind(peres3, peresF)

# 100% stacked bar chart
p3 <-ggplot(df_plot, aes(x = Var1, y = Freq, fill = dataset)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "",
    x = "",
    y = "Proportion",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("Flooded\nareas" = "#56B4E9",  # blue
                               "Total\npopulation" = "#D55E00")) + # orange +
  theme_minimal() +
  theme(
    text = element_text(size = 16),            # increases all text by default
    plot.title = element_text(size = 20, face = "bold"), # larger title
    axis.title = element_text(size = 18),      # axis titles
    axis.text = element_text(size = 14),       # axis tick labels
    legend.title = element_text(size = 16,face = "bold"),    # legend title
    legend.text = element_text(size = 14)      # legend labels
  )
getwd()
p3
ggsave("plots/Fig3.tiff", plot = p3, width = 8, height = 6, dpi = 300)
