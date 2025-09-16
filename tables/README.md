# This folder has ---

- Raw data 
The table *mihouseold* and *mihouseold* contains the following columns:
- COD_DANE_A: polygon code
- id: identifier
- empty: empty field (if any)
- area_m2: area of the polygon in square meters
- area_percent: proportion of the polygon per pixel
- areaTot_m2: total area in square meters
- Uso: Type of use, based in the categories of the project
- Estrato: dominant “estrato” per polygon
- householdNumber: number of households per polygon
- TP19_EE_E1 to TP19_EE_E6: number of households by strata categories
- mihouseold: maximum number of households (most recurrent) per polygon, calculated based on the proportion of the polygon overlapping the pixel

The difference between the two files is that _pixelapproach_80.csv_ contains data for all polygons that were selected and overlap more than 80 percent of the pixel, whereas _pixelapproachFull.csv_ includes data for all selected polygons.

The tables summary_household_pixelapproach80 and summary_household_pixelapproachFull contain the following columns: Estrato (the stratum), nim (number of observations), mean (average value), median (median value), max (maximum value), and sd (standard deviation). The differences between them are the raw data (tables described before)