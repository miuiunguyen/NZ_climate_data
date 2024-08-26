## Set up working directory
rm(list=ls())
setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/")
getwd()

## THIS IS TO COMBINE ALL THE PRECIPITATION .DAT FILE
## load all packages
library(tidyverse) #to use %>% 
library(dplyr) # to select var
library(tidyr) #to pivot col to row number
library(data.table) #to use rbind list
library(zoo) #to use rollapply

## 2007-2021
for (f in 2007:2021) {
  mypath <- paste("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/VCSN_1976_2022_datfile/vcsn_",f,"/",sep="")
  #mypath <- paste("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/VCSN_1976_2022_datfile/vcsn_2007")
  
    ###############################
## Step 1; combine all.dat file
###-----------------------#####

##Import & combine all csv files of precipitation
all_dat <- rbindlist(mapply(c, (
  #read file path
  list.files(path = mypath, pattern = "*.dat",full.names = TRUE) %>%
    #read all file contents
    lapply(read.table, header = TRUE, sep = ",", encoding = "UTF-8")), (
      list.files( path = mypath, pattern = "*.dat", full.names = TRUE) %>%
        #read all file names
        basename() %>%
        #combine all and unlist
        as.list()), SIMPLIFY = FALSE), fill = T)
all_dat$Date <- format(as.Date(all_dat$Date, format = "%d/%m/%Y"), "%m-%d-%Y")


##########################################################
####--Step 2: choose only rain and convert data to wide ##
## -----------------------------------------------------##

### Since the file is too large, we choose only rain variables for our project
data <- all_dat %>% dplyr::select("Agent","Lat","Longt", "Date", "Rain" ) %>% 
  spread(Date, Rain) # convert to wide data
assign(paste("data_",f,sep=""), data.frame(data))
}

## combine all data to wide format
#merged_data <- merge(merge(data_2007, data_2008, by = c("Agent", "Lat", "Longt"), data_2009, by = c("Agent", "Lat", "Longt")))

merged_data <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  data_2007,data_2008, by = c("Agent", "Lat", "Longt")),
  data_2009, by = c("Agent", "Lat", "Longt")),
  data_2010, by = c("Agent", "Lat", "Longt")),
  data_2011, by = c("Agent", "Lat", "Longt")),
  data_2012, by = c("Agent", "Lat", "Longt")),
  data_2013, by = c("Agent", "Lat", "Longt")),
  data_2014, by = c("Agent", "Lat", "Longt")),
  data_2015, by = c("Agent", "Lat", "Longt")),
  data_2016, by = c("Agent", "Lat", "Longt")),
  data_2017, by = c("Agent", "Lat", "Longt")),
  data_2018, by = c("Agent", "Lat", "Longt")),
  data_2019, by = c("Agent", "Lat", "Longt")),
  data_2020, by = c("Agent", "Lat", "Longt")),
  data_2021, by = c("Agent", "Lat", "Longt"))

merged_data <- merged_data %>% rename(location=Agent,
                                      lat=Lat,
                                      lon=Longt) 

#########################################################
####--Step 3: transfer lat long to nztm coordinate ---###
## -----------------------------------------------------##

# Load required libraries
library(rgdal)
library(sp)
# Define the WGS84 CRS object
wgs84 <- CRS("+init=epsg:4326")
# Define the NZTM CRS object
nztm <- CRS("+init=epsg:2193")
# prepare data frame
test <- merged_data %>% dplyr::select(location, lon, lat)
# Create an empty data frame to store the NZTM coordinates
nztm_coords <- data.frame(location = numeric(), lon = numeric(), lat = numeric(), easting = numeric(), northing = numeric())

# Loop over each location and convert its WGS84 coordinates to NZTM
for (i in seq_len(nrow(test))) {
  # Create a SpatialPoints object with the WGS84 coordinates
  point_wgs84 <- SpatialPoints(cbind(test$lon[i], test$lat[i]), proj4string = wgs84)
  
  # Transform the WGS84 coordinates to NZTM
  point_nztm <- spTransform(point_wgs84, nztm)
  
  # Get the easting and northing coordinates
  easting <- point_nztm@coords[1, 1]
  northing <- point_nztm@coords[1, 2]
  
  # Add the NZTM coordinates to the data frame
  nztm_coords[i, "easting"] <- easting
  nztm_coords[i, "northing"] <- northing
  nztm_coords[i, "location"] <- test$location[i]
  nztm_coords[i, "lon"] <- test$lon[i]
  nztm_coords[i, "lat"] <- test$lat[i]
}

# Merge the data frames on the "location" column
precip_all <- merge(nztm_coords,merged_data, by = c("location","lon","lat"))

library(openxlsx)
write.xlsx(precip_all, "precip_all_update.xlsx", sheetName = "precip_all", rowNames = FALSE)


## break file to send to datalab
precip_all <- read_excel("precip_all_update.xlsx")

precip_all_1 <- precip_all[1:1000,]
write.xlsx(precip_all_1, "precip_all_1.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_2 <- precip_all[1001:2000,]
write.xlsx(precip_all_2, "precip_all_2.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_3 <- precip_all[2001:3000,]
write.xlsx(precip_all_3, "precip_all_3.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_4 <- precip_all[3001:4000,]
write.xlsx(precip_all_4, "precip_all_4.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_5 <- precip_all[4001:5000,]
write.xlsx(precip_all_5, "precip_all_5.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_6 <- precip_all[5001:6000,]
write.xlsx(precip_all_6, "precip_all_6.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_7 <- precip_all[6001:7000,]
write.xlsx(precip_all_7, "precip_all_7.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_8 <- precip_all[7001:8000,]
write.xlsx(precip_all_8, "precip_all_8.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_9 <- precip_all[8001:9000,]
write.xlsx(precip_all_9, "precip_all_9.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_10 <- precip_all[9001:10000,]
write.xlsx(precip_all_10, "precip_all_10.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_11 <- precip_all[10001:11000,]
write.xlsx(precip_all_11, "precip_all_11.xlsx", sheetName = "precip_all", rowNames = FALSE)

precip_all_12 <- precip_all[11001:11491,]
write.xlsx(precip_all_12, "precip_all_12.xlsx", sheetName = "precip_all", rowNames = FALSE)

