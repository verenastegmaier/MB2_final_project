########## Change Detection Analysis of the 2018 drought in the southern part of Sweden        ##########
########## 1. NDVI Analysis                                                                    ##########
########## 2. Classification of two Sentinel2 Satellite Images with following change detection ##########
########## 3. Analysis and Comparison of MODIS NDVI, Temperature and Precipitation             ##########
##########    Time Series data                                                                 ##########                                                       ##########

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
##########  NDVI Analysis of satellite data    ##########
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
##########     Classification and Change Detection       ##########
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

##########################################################
##########                                      ##########
##########  MODIS time series analysis          ##########
##########  NDVI, Temperature and Precipitation ##########
##########                                      ##########
##########################################################

products <- mt_products()
head(products)

bands <- mt_bands(product = "MOD13Q1")
head(bands)

bands <- mt_bands(product = "MOD11A2")
head(bands)

dates <- mt_dates(product = "MOD13Q1", lat = 55.45, lon = 13.17)
head(dates)

# Download MODIS NDVI and Temperature data for year 2018 ------------------

mod_ndvi18 <- mt_subset(product ="MOD13Q1",
                        band = "250m_16_days_NDVI",
                        lat = 55.453811,
                        lon = 13.179940,
                        start = "2018-01-01",
                        end = "2018-12-30",
                        site_name = "NDVI_MODIS",
                        km_lr = 8,
                        km_ab = 8,
                        internal = TRUE,
                        progress = FALSE)

mod_vi18 <- mt_subset(product ="MOD13Q1",
                      band = "250m_16_days_VI_Quality",
                      lat = 55.453811,
                      lon = 13.179940,
                      start = "2018-01-01",
                      end = "2018-12-30",
                      site_name = "VI_MODIS",
                      km_lr = 8,
                      km_ab = 8,
                      internal = TRUE,
                      progress = FALSE)

mod_pr18 <- mt_subset(product ="MOD13Q1",
                      band = "250m_16_days_pixel_reliability",
                      lat = 55.453811,
                      lon = 13.179940,
                      start = "2018-01-01",
                      end = "2018-12-30",
                      site_name = "PR_MODIS",
                      km_lr = 8,
                      km_ab = 8,
                      internal = TRUE,
                      progress = FALSE)

mod_temp18 <- mt_subset(product ="MOD11A2",
                        band = "LST_Day_1km",
                        lat = 55.453811,
                        lon = 13.179940,
                        start = "2018-01-01",
                        end = "2018-12-30",
                        site_name = "TEMP_MODIS",
                        km_lr = 8,
                        km_ab = 8,
                        internal = TRUE,
                        progress = FALSE)
print("download done")


# cleaning the NDVI 2018 data -------------------------------------------------------
# VI values are in numbers with a base 10 counting system, need to be in binary
# make new dataframe for binary numbers

vi_bin18 <-  mod_vi18

first_k_bits <- function(int, k=16, reverse=T) {
  integer_vector <- as.integer(intToBits(int))[1:k]
  if(reverse) integer_vector <- rev(integer_vector)
  return(paste(as.character(integer_vector), collapse=""))
}

# binarise the VIQ values

vi18_bin_list <-  lapply(mod_vi18$value,
                         FUN = first_k_bits)
vi18_bin_vector <-  unlist(vi18_bin_list)
vi_bin18$value <-  as.character(vi18_bin_vector)

# compiling all dataframes into one list 

allbands18 <- list(vi_bin18,
                   mod_pr18,
                   mod_ndvi18)

# cleaning the bands
# Remove instances where pixel reliability is > 1, convert them into NA

pr18_filtered <-  lapply(allbands18,
                         function(x){
                           x$value[allbands18[[2]]$value > 1] <- NA
                           return(x)
                         })

# Mask out additional data based on land/water and snow/ice masks

vi18_filtered <-  lapply(pr18_filtered,
                         FUN = function(x){
                           x$value[substr(pr18_filtered[[1]]$value,
                                          start = 3, stop = 5) != "001"] <- NA
                           x$value[substr(pr18_filtered[[1]]$value,
                                          start = 2, stop = 2) != "0"] <- NA
                           return(x)
                         })

ndvi18_vi <- data.frame(vi18_filtered[3])

# Processing NDVI 2018 data -----------------------------------------------
# getting the mean ndvi of each month of the year 2018 and filtering
# out values under 0 (water...)

ndvi18_fil <- ndvi18_vi %>% 
  filter(value >= 0) %>% 
  mutate(calendar_date = ymd(calendar_date), Year = year(calendar_date), 
         Month = month(calendar_date, label = TRUE)) %>% 
  group_by(Month, Year) %>% 
  summarise(ndvi_mean = mean(value))

p_ndvi18 <- ggplot(ndvi18_fil, aes(Month, ndvi_mean / 10000)) +
  geom_line(group = 1, col = "darkgreen") +
  ylab("Mean NDVI Values") +
  ggtitle("Mean NDVI values per month in 2018") +
  theme_minimal()

p_ndvi18

# Processing MODIS Temperature Data --------------------------------------------------

hist(mod_temp18$value)

# filtering out 0 values, group data and getting mean of each month

temp18_fil <- mod_temp18 %>% 
  filter(value > 0) %>% 
  mutate(calendar_date = ymd(calendar_date), Year = year(calendar_date), 
         Month = month(calendar_date, label = TRUE)) %>% 
  group_by(Month, Year) %>% 
  summarise(temp_mean = mean(value))

p_temp18 <- ggplot(temp18_fil, aes(Month, temp_mean * 0.02 - 273.15)) +
  geom_point() +
  geom_line(group = 1) +
  ylab("Mean temperature [C°]") +
  ggtitle("Mean monthly temperatures 2018") +
  theme_minimal()

p_temp18

# adding new column with celsius values

temp18_fil
temp18_fil["temp_cel"] <- temp18_fil["temp_mean"] * 0.02 - 273.15

# plotting mean  Temp and  mean NDVI next to each other

p_ndvi18 + p_temp18 + theme_minimal()

ndvi_temp18 <- cbind(ndvi18_fil, temp18_fil)
ndvi_temp18

# Downloading MODIS NDVI and Temperature data for year 2017 ---------------------------------------------------------

mod_ndvi17 <- mt_subset(product ="MOD13Q1",
                        band = "250m_16_days_NDVI",
                        lat = 55.453811,
                        lon = 13.179940,
                        start = "2017-01-01",
                        end = "2017-12-30",
                        site_name = "NDVI_MODIS",
                        km_lr = 8,
                        km_ab = 8,
                        internal = TRUE,
                        progress = FALSE)

mod_vi17 <- mt_subset(product ="MOD13Q1",
                      band = "250m_16_days_VI_Quality",
                      lat = 55.453811,
                      lon = 13.179940,
                      start = "2017-01-01",
                      end = "2017-12-30",
                      site_name = "VI_MODIS",
                      km_lr = 8,
                      km_ab = 8,
                      internal = TRUE,
                      progress = FALSE)

mod_pr17 <- mt_subset(product ="MOD13Q1",
                      band = "250m_16_days_pixel_reliability",
                      lat = 55.453811,
                      lon = 13.179940,
                      start = "2017-01-01",
                      end = "2017-12-30",
                      site_name = "PR_MODIS",
                      km_lr = 8,
                      km_ab = 8,
                      internal = TRUE,
                      progress = FALSE)

mod_temp17 <- mt_subset(product ="MOD11A2",
                        band = "LST_Day_1km",
                        lat = 55.453811,
                        lon = 13.179940,
                        start = "2017-01-01",
                        end = "2017-12-30",
                        site_name = "TEMP_MODIS",
                        km_lr = 8,
                        km_ab = 8,
                        internal = TRUE,
                        progress = FALSE)
print("download done")

# cleaning the data the same way as the 2018 data
vi_bin17 <-  mod_vi17
vi17_bin_list <-  lapply(mod_vi17$value,
                         FUN = first_k_bits)
vi17_bin_vector <-  unlist(vi17_bin_list)
vi_bin17$value <-  as.character(vi17_bin_vector)

allbands17 <- list(vi_bin17,
                   mod_pr17,
                   mod_ndvi17)

pr17_filtered <-  lapply(allbands17,
                         function(x){
                           x$value[allbands17[[2]]$value > 1] <- NA
                           return(x)
                         })

vi17_filtered <-  lapply(pr17_filtered,
                         FUN = function(x){
                           x$value[substr(pr17_filtered[[1]]$value,
                                          start = 3, stop = 5) != "001"] <- NA
                           x$value[substr(pr17_filtered[[1]]$value,
                                          start = 2, stop = 2) != "0"] <- NA
                           return(x)
                         })

ndvi17_vi <- data.frame(vi17_filtered[3])

# getting the mean ndvi of each month of the year 2018 and filtering
# out values under 0 (water...)

ndvi17_fil <- ndvi17_vi %>% 
  filter(value >= 0) %>% 
  mutate(calendar_date = ymd(calendar_date), Year = year(calendar_date), 
         Month = month(calendar_date, label = TRUE)) %>% 
  group_by(Month, Year) %>% 
  summarise(ndvi_mean17 = mean(value))

p_ndvi17 <- ggplot(ndvi17_fil, aes(Month, ndvi_mean17 / 10000)) +
  geom_line(group = 1, col = "darkgreen") +
  ylab("Mean NDVI values") +
  ggtitle("Mean NDVI values 2017") +
  theme_minimal()

p_ndvi17

# Processing MODIS Temperature Data 2017 --------------------------------------------------

mod_temp17

temp17_fil <- mod_temp17 %>% 
  filter(value > 0) %>% 
  mutate(calendar_date = ymd(calendar_date), Year = year(calendar_date), Month = month(calendar_date, label = TRUE)) %>% 
  group_by(Month, Year) %>% 
  summarise(temp_mean17 = mean(value))

p_temp17 <- ggplot(temp17_fil, aes(Month, temp_mean17 * 0.02 - 273.15)) +
  geom_point() +
  geom_line(group = 1) +
  ylab("Mean temperature [C°]") +
  ggtitle("Mean monthly temperature 2017") +
  theme_minimal()

p_temp17

# calculating temperature in Celsius and adding it as new column

temp17_fil
temp17_fil["temp_cel"] <- temp17_fil["temp_mean17"] * 0.02 - 273.15

# plotting mean Temp and mean NDVI next to each other

p_ndvi17 + p_temp17 + theme_minimal()

ndvi_temp17 <- cbind(ndvi17_fil, temp17_fil)
ndvi_temp17

# Comparison of 2017 and 2018 NDVI and Temperature data -------------------------------------------------

p_temp17 + p_temp18

p_ndvi17 + p_ndvi18

# binding data together

temp_1718 <- cbind(temp17_fil, temp18_fil)
temp_1718

colnames(temp_1718) <- c("Month_17", "Year_17", "temp_mean17", "temp_cel17", 
                         "Month_18", "Year_18", "temp_mean18", "temp_cel18")


ndvi_1718 <- cbind(ndvi17_fil, ndvi18_fil)
ndvi_1718

colnames(ndvi_1718) <- c("Month_17", "Year_17", "ndvi_mean17", "Month_18", "Year_18", "ndvi_mean18")

# plotting data in one plot for comparison

ggplot(ndvi_1718, aes(x = Month_17))+
  geom_line(aes(y = ndvi_mean18/10000, col = "ndvi_mean18"), group = 1)+
  geom_line(aes(y = ndvi_mean17/10000, col = "ndvi_mean17"), group = 1)+
  theme_minimal()+
  xlab("Month")+
  ylab("Mean NDVI values")+
  ggtitle("Mean NDVI values of the years 2017 and 2018")

ggplot(temp_1718, aes(x = Month_17))+
  geom_line(aes(y = temp_cel17, col = "temp_cel17"), group = 1)+
  geom_line(aes(y = temp_cel18, col = "temp_cel18"), group = 1)+
  theme_minimal()+
  xlab("Month")+
  ylab("Mean monthly temperature [K]")+
  ggtitle("Mean monthly temperatures of the years 2017 and 2018")


# Downloading Precipitation data ------------------------------------------

# create dataframe for MODIS research region

lat_lon_df <- data.frame(id = "df",
                         lon = 13.179940,
                         lat = 55.453811)

# finding station closest to research region

near_df <- 
  meteo_nearby_stations(
    lat_lon_df = lat_lon_df,
    lat_colname = "lat",
    lon_colname = "lon",
    var = "PRCP",
    year_min = 2017,
    year_max = 2018,
    limit = 10,
  )

near_df # station Trelleborg nearest station

# extract preceitation data for this station

prcp <- 
  meteo_pull_monitors(
    monitors = near_df$df$id[1],
    date_min = "2017-01-01",
    date_max = "2018-12-30",
    var = "PRCP"
  )

hist(prcp$prcp)

# grouping data by year and month with sum of prec. per month

prcp17 <- prcp %>% 
  subset(date <= as.Date('2017-12-31')) %>% 
  mutate(year17 = year(date),
         month17 = month(date, label = TRUE)) %>%
  group_by(year17, month17) %>% 
  summarise(prcp17 = sum(prcp, na.rm = TRUE) / 10)

hist(prcp17$prcp17)  

prcp18 <- prcp %>% 
  subset(date >= as.Date('2018-01-01')) %>% 
  mutate(year18 = year(date),
         month18 = month(date, label = TRUE)) %>%
  group_by(year18, month18) %>% 
  summarise(prcp18 = sum(prcp, na.rm = TRUE) / 10)

hist(prcp18$prcp18)

ggplot(prcp17, aes(month17, prcp17))+
  geom_col(fill = "blue")+
  xlab("month")+
  ylab("precipitation [mm]")+
  ggtitle("Precipitation per month in 2017")+
  theme_minimal()

ggplot(prcp18, aes(month18, prcp18))+
  geom_col(fill = "blue")+
  xlab("month")+
  ylab("precipitation [mm]")+
  ggtitle("Precipitation per month in 2018")+
  theme_minimal()

# combining temp and prec data to one dataframe

climate_data <- cbind(temp_1718, prcp17, prcp18)  
climate_data

# plotting Temp and Prec for each year
# setting scaleFactor so we can plot two different y-axis

scaleFactor <- max(climate_data$temp_cel17)/max(climate_data$prcp17)

p_clim17 <- ggplot(data = climate_data, aes(x = month17))+
  geom_col(aes(y = prcp17 * scaleFactor), fill = "blue", alpha = 0.6)+
  geom_line(aes(y = temp_cel17), col = "red", size = 1.5, group = 1)+
  scale_y_continuous(name = "Mean monthly temperature [C°]",
                     sec.axis = sec_axis(~./scaleFactor, name = "Precipitation [mm]"))+
  xlab("Month")+
  theme_minimal()+
  ggtitle("Climate diagram of the year 2017")


scaleFactor2 <- max(climate_data$temp_cel18)/max(climate_data$prcp18)

p_clim18 <- ggplot(data = climate_data, aes(x = month18))+
  geom_col(aes(y = prcp18 * scaleFactor), fill = "blue", alpha = 0.6)+
  geom_line(aes(y = temp_cel18), col = "red", size = 1.5, group = 1)+
  scale_y_continuous(name = "Mean monthly temperature [C]°",
                     sec.axis = sec_axis(~./scaleFactor2, name = "Precipitation [mm]"))+
  xlab("Month")+
  theme_minimal()+
  ggtitle("Climate diagram of the year 2018")

p_clim17 + p_ndvi17

p_clim18 + p_ndvi18

# combining data to one big dataframe for further plotting

climate_data$ndvi_mean17 <- ndvi_temp17$ndvi_mean17
climate_data$ndvi_mean18 <- ndvi_temp18$ndvi_mean

scaleFactor3 <- max(climate_data$ndvi_mean17)/max(climate_data$prcp17)

# plotting NDVI compared to Prec 2017

p_ndvi_prec17 <- ggplot(data = climate_data, aes(x = month17))+
  geom_col(aes(y = prcp17 * scaleFactor3), fill = "blue", alpha = 0.5)+
  geom_line(aes(y = ndvi_mean17), col = "darkgreen", size = 1.5, group = 1)+
  scale_y_continuous(name = "Mean NDVI values * 10000",
                     sec.axis = sec_axis(~./scaleFactor3, name = "Precipitation [mm]"))+
  xlab("Month")+
  theme_minimal()+
  ggtitle("Precipitation and NDVI of the year 2017")

p_ndvi_prec17

scaleFactor4 <- max(climate_data$ndvi_mean18)/max(climate_data$prcp18)

# plotting NDVI compared to Prec 2018

p_ndvi_prec18 <- ggplot(data = climate_data, aes(x = month18))+
  geom_col(aes(y = prcp18 * scaleFactor4), fill = "blue", alpha = 0.5)+
  geom_line(aes(y = ndvi_mean18), col = "darkgreen", size = 1.5, group = 1)+
  scale_y_continuous(name = "Mean NDVI values * 10000",
                     sec.axis = sec_axis(~./scaleFactor4, name = "Precipitation [mm]"))+
  xlab("Month")+
  theme_minimal()+
  ggtitle("Precipitation and NDVI of the year 2018")

p_ndvi_prec18
