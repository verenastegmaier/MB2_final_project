########## Change Detection Analysis of the 2018 drought in the southern part of Sweden        ##########
           
           ##Script 1
##########  1. NDVI Analysis                                                                    ##########
##########  2. Classification of two Sentinel2 Satellite Images with following change detection ##########
           

# Data source:
## Sentinel 2 Data: https://scihub.copernicus.eu/dhus/#/home

# Main web sources:
## https://github.com/wegmann/R_scripts/blob/master/supervised_classification.R
## https://www.unescap.org/sites/default/files/Producing_land_cover_change_maps_and_statistics.pdf#page=20
## https://ggplot2.tidyverse.org/reference/
## https://cran.r-project.org/web/packages/MODISTools/vignettes/modistools-vignette.html
## https://jepsonnomad.github.io/tutorials/MODISTools_Intro.html
## https://stackoverflow.com/questions/68878571/how-to-download-precipitation-data-using-rnoaa
## https://cran.r-project.org/web/packages/MODISTools/MODISTools.pdf

# list of required packages ----------------------------------------------------------------

library(raster)
library(rgdal)
library(RStoolbox)
library(tidyverse)
library(maptools)
library(randomForest)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(MODISTools)
library(sf)
library(rnoaa)
library(dplyr)
library(lubridate)
library(patchwork)

#########################################################
##########                                     ##########
##########  1. NDVI Analysis of satellite data ##########
##########                                     ##########
#########################################################

# 2 scenes from before and after the drought event in 2018 were downloaded for the southern part of Sweden
# they were stacked and clipped by polygon in QGIS (bands = b,g,r,nir)

swe_0606 <- brick(list.files(path = ".", pattern = "swe_clip_0606.tif", full.names = TRUE))
swe_0731 <- brick(list.files(path = ".", pattern = "swe_clip_0731.tif", full.names = TRUE))

# plotting the two scenes as RGB composites

plotRGB(swe_0606,
        r = 3, g = 2, b = 1,
        axes = TRUE,
        stretch = "lin",
        main = "Sentinel2 True Color Composite 06.06.2018")


plotRGB(swe_0731,
        r = 3, g = 2, b = 1,
        axes = TRUE,
        stretch = "lin",
        main = "Sentinel2 True Color Composite 31.07.2018")


# computing NDVIs, which allows us to automatically calculate several different indices

ndvi_0606 <- spectralIndices(swe_0606, red="swe_clip_0606.3", nir="swe_clip_0606.4", indices = "NDVI")

plot(ndvi_0606,
     main = "NDVI 06.06.2018",
     axes = FALSE,
     box = FALSE)

ndvi_0731 <- spectralIndices(swe_0731, red="swe_clip_0731.3", nir="swe_clip_0731.4", indices = "NDVI")

plot(ndvi_0731,
     main = "NDVI 31.07.2018",
     axes = FALSE,
     box = FALSE)


# quick look at the histograms of the two NDVIs

hist_0606 <- hist(ndvi_0606)

hist_0731 <- hist(ndvi_0731)


# converting data into dataframes for ggplot

df_ndvi_0606 <- rasterToPoints(ndvi_0606, spatial = TRUE)
df_ndvi_0606 <- data.frame(df_ndvi_0606) 

df_ndvi_0731 <- rasterToPoints(ndvi_0731, spatial = TRUE)
df_ndvi_0731 <- data.frame(df_ndvi_0731) 


# adding new columns for manual labeling

df_ndvi_0606$ndvi_comb <- 'ndvi_0606' 
df_ndvi_0731$ndvi_comb <- 'ndvi_0731'


# plotting with combined dataframes

df_ndvi_both <- data.frame(df_ndvi_0606, df_ndvi_0731)

ggplot(data = df_ndvi_both)+
  geom_histogram(aes(x = NDVI, fill = "blue"), color = "lightgrey", alpha = 0.5, bins = 25)+
  geom_histogram(aes(x = NDVI.1, fill = "darkgreen"), col = "lightgrey", alpha = 0.6, bins = 25)+
  theme_minimal()+
  xlim(-0.5,1)+
  ylab("frequency")+ xlab("NDVI pixel value")+
  ggtitle("Distribution of NDVI pixel values from 06.06 and 31.07.2018")+
  scale_fill_manual(values = c("blue", "darkgreen"),
                    name = "NDVIs",
                    labels = c("NDVI_06.06", "NDVI_31.07"))

# calculating difference between the two rasters, low and negative values --> decreasing NDVI values

ndvi_diff <- ndvi_0731 - ndvi_0606

plot(ndvi_diff, aes(x = x, y = y, col = "NDVI")) 

hist(ndvi_diff, col = "darkgreen", xlab = "Diff. NDVI", 
     main = "Difference between NDVI values")


###################################################################
##########                                               ##########
##########    2. Classification and Change Detection    ##########
##########                                               ##########
###################################################################


# CLassification --------------------------------------------------------

td_0606 <- readOGR(list.files(path = ".", pattern = "training_data_0606.gpkg", full.names = TRUE, recursive = TRUE))

td_0731 <- readOGR(list.files(path = ".", pattern = "training_data_0731.gpkg", full.names = TRUE, recursive = TRUE))

# classification with superClass function
# holding out training data for validation

sc_0606 <- superClass(swe_0606, trainData = td_0606, responseCol = "id",
                      model = "rf", tuneLength = 1, trainPartition = 0.7)

sc_0731 <- superClass(swe_0731, trainData = td_0731, responseCol = "ID",
                      model = "rf", tuneLength = 1, trainPartition = 0.7)
print("classification done")

# Classes:
# class 1 = Forest
# Class 2 = Developed (Settlements, Streets, Infrastructure...)
# CLass 3 = Water
# CLass 4 = Vegetation
# Class 5 = Soil

hist(sc_0606$map, col = "darkgrey",
     xlab = "class", ylab = "count", main = "Count of pixels in each class")

hist(sc_0731$map, col = "darkgrey",
     xlab = "class",  ylab = "count", main = " Count of pixels in each class")

# expand right side of clipping rect. so we can place the legend outside of the map

par(xpd= TRUE, mar=par()$mar+c(0,0,0,6))

# plot classification map 06.06

plot(sc_0606$map,
     legend = FALSE,
     col = c("darkgreen", "black", "blue", "green", "saddlebrown"),
     main = "Classified Image 06.06")

legend(413000,6187000,
       legend = c("forest", "developed", "water", "vegetation", "soil"),
       fill = c("darkgreen", "black", "blue", "green", "saddlebrown"),
       border = FALSE,
       bty = "n",
       xpd = TRUE,
       title = "Legend")

# plot classification map 31.07

plot(sc_0731$map,
     legend = FALSE,
     col = c("darkgreen", "black", "blue", "green", "saddlebrown"),
     main = "Classified Image 31.07")

legend(413000,6187000,
       legend = c("forest", "developed", "water", "vegetation", "soil"),
       fill = c("darkgreen", "black", "blue", "green", "saddlebrown"),
       border = FALSE,
       bty = "n",
       xpd = TRUE,
       title = "Legend")

# settings back to default

dev.off()


# Validation --------------------------------------------------------------

# calculating confusion matrices
# sensitivity = Producers Accuracy
# Pos Pred Value = Users Accuracy

sc_0606$validation$performance
sc_0731$validation$performance

sc_0606_val <- as.data.frame(sc_0606$validation$performance$table)
sc_0731_val <- as.data.frame(sc_0731$validation$performance$table)

ggplot(data = sc_0606_val, aes(Prediction, Reference))+
  geom_point(aes(size = Freq))

ggplot(data = sc_0731_val, aes(Prediction, Reference))+
  geom_point(aes(size = Freq))


# Change Detection --------------------------------------------------------

# creating raster layer

map_sc_0606 <- sc_0606$map
plot(map_sc_0606, main = "Classification 06.06")

map_sc_0731 <- sc_0731$map
plot(map_sc_0731, main = "Classification 31.07")


# calculating change map by multiplying first image with 10 and adding values of second image

sc_change<- map_sc_0606*10 + map_sc_0731
class(sc_change)

ggplot(sc_change, aes(x,y, fill = factor(layer)))+
  geom_raster()+
  ggtitle("Change Map")+
  theme_minimal()

# converting raster into dataframe for filtering

df_sc_change <- as.data.frame(sc_change, xy = TRUE)
df_sc_change

# setting filter for change classes that indicate change from vegetation (incl.forest) to soil

fil_changeclass <- c("25", "45")

fil_changeclass <- df_sc_change %>%
  filter(layer %in% fil_changeclass)


ggplot(fil_changeclass, aes(x,y, fill = factor(layer)))+
  geom_tile()+
  theme_minimal()+
  ggtitle("Change classes from vegetation to soil")+
  scale_fill_manual("Change classes", values = c("darkgreen", "green"),
                    labels = c("forest -> soil", "vegetation -> soil"))+
  xlab("lon")+
  ylab("lat")+
  annotation_scale()+
  annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"), pad_y = unit(10, "cm"),
                         pad_x = unit(0.3, "cm"))


# Statistics of changed area ----------------------------------------------

# extracting all values to calculate the area that changed from vegetation (incl. forest) to soil --> 45 & 15

resSen <- res(sc_change)
veg_soil_change <- sum(sc_change[] == 45 | sc_change[] == 15) * prod(resSen) * 1e-06
veg_soil_change


# calculating change of areas with function that sets the resolution, calculates the area, 
# assigns class names and creates new dataframe with landcover classes and their corresponding area

calc_change_area <- function(map, td){
  map_freq <- freq(map, useNA = "no")
  resSen <- res(map)
  area_km2 <- map_freq[,"count"] * prod(resSen) * 1e-06
  classes <- unique(td$class_name)
  area <- data.frame(landcover = classes, area_km2 = area_km2)
  return(area)
}


area_0606 <- calc_change_area(map_sc_0606, td_0606)
area_0731 <- calc_change_area(map_sc_0731, td_0731)

# changing names of columns so we can differ between two dates

names(area_0731)[names(area_0731) == "landcover"] <- "landcover_0731"
names(area_0731)[names(area_0731) == "area_km2"] <- "area_km2_0731"
area_0731

ggplot(area_0606, aes(landcover, area_km2, fill = landcover))+
  geom_col()+
  geom_text(aes(label= signif(area_km2, digits = 3), angle = 270, vjust = -0.5), size = 3)+
  ggtitle("Area of each landcover class from 06.06")+
  xlab("landcover class")+
  ylab("Area (km²)")+
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("slategrey",
                               "darkgreen",
                               "sienna",
                               "yellowgreen",
                               "blue"))

ggplot(area_0731, aes(landcover_0731, area_km2_0731, fill = landcover_0731))+
  geom_col()+
  geom_text(aes(label= signif(area_km2_0731, digits = 3), angle = 270, vjust = -0.5), size = 3)+
  ggtitle("Area of each landcover class from 31.07")+
  xlab("landcover class")+
  ylab("Area (km²)")+
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("slategrey",
                               "darkgreen",
                               "sienna",
                               "yellowgreen",
                               "blue"))


# binding area dataframess together

change_area <- cbind(area_0606, area_0731)
change_area


# calculating statistics on the areas percentages
# difference of area in sqkm

change_area["diff_area"] <-  change_area["area_km2_0731"] - change_area["area_km2"]

# difference of percentage deviation
change_area["perc_dev"] <-  change_area["diff_area"] / change_area["area_km2"] * 100

# percentage of area per total area for both datasets
change_area["perc_0606"] <-  change_area["area_km2"] / sum(change_area["area_km2"]) * 100
change_area["perc_0731"] <-  change_area["area_km2_0731"] / sum(change_area["area_km2_0731"]) * 100                                                                

# percentage of change between two datasets
change_area["diff_perc"] <-  change_area["perc_0731"] - change_area["perc_0606"]
change_area

ggplot(change_area, aes(landcover_0731, diff_perc))+
  geom_col(fill = "darkgreen")+
  ggtitle("Change of percentage of classes area to total area")+
  ylab("Percentage of area [%]")+
  xlab("landcover class")+
  theme_minimal()

# Further Statistics on changed pixel values -----------------------------------------------------

# creating new dataframe with pixel values of class_0606 and class_0731
lc_pix <- data.frame(map_sc_0606 = values(map_sc_0606), map_sc_0731 = values(map_sc_0731))

# creating change/confusion matrix
landcover_change <- table(lc_pix)
head(landcover_change)
lc_mat <- (round(landcover_change, digits = 1))
lc_mat

#add sum
lc_mat2 <- (round(addmargins(landcover_change), digits = 1))
lc_mat2

write.table(lc_mat2, file = "lc_mat.csv", append = TRUE, sep = ",", col.names = NA,
             row.names = TRUE, quote = FALSE)

# calculating count of pixels in each class at 06.06 and 31.7
lc_0606 <- cbind(rowSums(round(table(lc_pix), digits = 1)))
lc_0731 <- cbind(colSums(round(table(lc_pix), digits = 1)))

# difference of count of pixels between the two dates
diff_pix <- cbind(c(lc_0731) - c(lc_0606))
diff_pix

# percentage change of pixels between the two dates
perc_diff <- (diff_pix/lc_0606)*100
perc_diff

# putting all results to one table together
change_table <- cbind(c(lc_0606), c(lc_0731), c(diff_pix), c(perc_diff))
change_table

# renaming columns
colnames(change_table) <- c("count_0606", "count_0731", 
                            "count_diff", "perc_diff")

# for ggplot converting into dataframe
df_change <- as.data.frame(change_table)
df_change

# adding new column of class names to df_change
td_classes <- unique(td_0606$class_name)
df_change <- cbind(df_change, td_classes)
df_change

ggplot(df_change, aes(x = td_classes, y = perc_diff))+
  geom_col(fill = "darkgreen")+
  ggtitle("Change of percentage of classes pixel to total count of pixels")+
  ylab("%")+
  xlab("landcover class")+
  theme_minimal()