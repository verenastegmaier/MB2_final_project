           ## Script 2
##########  Analysis and Comparison of MODIS NDVI, Temperature and Precipitation       ##########
##########  Time Series data                                                           ##########  


# load packages -----------------------------------------------------------
# if not done already, run the packages from Script 1

###############################################################
##########                                           ##########
##########  MODIS time series analysis               ##########
##########  NDVI, Temperature and Precipitation data ##########
##########                                           ##########
###############################################################

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