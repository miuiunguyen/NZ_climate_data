rm(list=ls())
setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/")
getwd()

## load all packages use
library(tidyverse) #to use %>% 
library(dplyr) # to select var
library(tidyr) #to pivot col to row number
library(data.table) #to use rbind list
library(zoo) #to use rollapply

### DRAFT DATA HERE ####
#########
library(readxl)
merged_quan <- read_excel("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/events_2/events_2015.xlsx")

############################################################
####--Step 4: Calculate distance between two list GPS ---###
## -----------------------------------------------------####
### STEP 4.1: First, convert nztm coord to gps to be easier to calculate distance in R
library(rgdal)
library(sp)
library(mapproj) #main package to convert
library(readxl) #read excel file
loc <- read_excel("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/test_location.xlsx")

# Define the CRS for WGS84 projection
wgs84_proj <- CRS("+init=epsg:4326")
# Define the NZTM projection and the WGS84 (latitude/longitude) projection
nztm_proj <- CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m")
#wgs84_proj <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# prepare data frame
loc_convert <- loc %>% select(x_coord,y_coord)
# Create an empty data frame to store the NZTM coordinates
latlon_coords <- data.frame(location = numeric(), lon = numeric(), lat = numeric(), easting = numeric(), northing = numeric())

# Loop over each location and convert its WGS84 coordinates to NZTM
for (i in seq_len(nrow(loc_convert))) {
  # Convert NZTM coordinates to WGS84 (latitude/longitude)
  coords_wgs84 <- spTransform(SpatialPoints(loc_convert, proj4string = nztm_proj), wgs84_proj)
  
  # Get the lat and lon
  lon <- coords_wgs84@coords[i, 1]
  lat <- coords_wgs84@coords[i, 2]
  
  # Add the NZTM coordinates to the data frame
  
  latlon_coords[i, "lat"] <- lat
  latlon_coords[i, "lon"] <- lon
  latlon_coords[i, "location"] <- loc$id[i]
  latlon_coords[i, "easting"] <- loc$x_coord[i]
  latlon_coords[i, "northing"] <- loc$y_coord[i]
}
library(sp)
library(geosphere) #to use dist()
### read excel file of location list
latlon_coords
 # Define two lists of GPS points as latitude and longitude coordinates
points1 <- latlon_coords %>% select(lon,lat) %>% data.frame()
points2 <- merged_quan %>% select(lon,lat) %>% data.frame()

for (j in 1:5) {
  #for (j in 1:nrow(points1)) {
  # transfer to matrix
  point <- as.matrix(points1)[j,]
  #point <- as.matrix(points1)
  points_list <- as.matrix(points2)
  
  # Initialize minimum distance as infinity and closest point as NULL
  min_dist <- Inf
  closest_point  <- NULL
  closest <- data.frame(meshblock = numeric(), lon = numeric(), lat = numeric(), min_dist = numeric())
  
  # Loop through each point in points_list
  for (i in 1:nrow(points_list)) {
    # Calculate distance between the two points in meters
    dist_meters <- distm(point, points_list[i,], fun = distHaversine) 
    # Update minimum distance and closest point if this distance is smaller
    if (dist_meters < min_dist) {
      min_dist<- dist_meters
      closest_point<- points_list[i,]
    }
  }
  
  min_dist
  ## data frame of closest point
  closest_point <- as.data.frame(closest_point)
  #closest_point_rev <- rev(t(closest_point))
  location <- latlon_coords$location
  closest[j, "lon"] <- closest_point[1,]
  closest[j, "lat"] <- closest_point[2,]
  closest[j, "min_dist"] <- min_dist
  closest[j, "meshblock"] <- location[j]
  
  assign(paste("clo_",j,sep=""), data.frame(closest))
}

## COMBINE ALL CLOSEST DATA
Names <- ls(pattern = "clo_")
L <- mget(Names)
clo_all <- do.call("rbind", L) %>% na.omit()

## 4.3 MERGE WITH PRECIPITATION DATA
precip_all <- read_excel("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/precip_all_update.xlsx")
distance <- merge(clo_all, precip_all, by = c("lat","lon"))

## CALCULATE QUANTILE, NUMBER OF EXTREM EVENTS, AVERAGE PRECIPITATION 
### BY LAST MONTH, LAST 3 MONTHS, LAST 6 MONTHS, LAST 12 MONTHS
#### example of final data in datalab: from distance data to add month and year
distance[, 1] <- NULL
distance[, 1] <- NULL

day <- c(23,1,3,5,13)
month <- c(1,3,5,7,12)
year <- c(2008,2012, 2020, 2018, 2017)
distance[["day"]] <- c(23,1,3,5,13)
distance[["month"]] <- c(1,3,5,7,12)
distance[["year"]] <- c(2008,2012, 2020, 2018, 2017)
distance[["hhid"]] <- c(1,2,3,4,5)
distance[["meshblock"]] <- c(1,2,3,4,5)

## remove X number in the data of names column
colnames(distance) <- gsub("X0", "", colnames(distance))
colnames(distance) <- gsub("X", "", colnames(distance))


#calculate average precipitation
for (i in 1:5) {
  month <- distance$month[i]
  year <- distance$year[i]
  if (distance$day[i] < 10) {
    date <- paste0(distance$month[i],".0",distance$day[i],".",distance$year[i])
  } else {
    date <- paste0(distance$month[i],".",distance$day[i],".",distance$year[i])
  }
  
  day_survey <- which(names(distance[i,]) == date) #count the position of variables date
  day_survey_1<- day_survey- 1
  day_survey_2 <- day_survey - 2
  day_survey_3 <- day_survey - 3
  day_survey_365 <- day_survey - 365
  
  all_day_survey <- distance [i,] %>% select(all_of(day_survey))
  all_day_survey_365 <- distance [i,] %>% select(hhid,meshblock,day,month,year,
                                                        day_survey:day_survey_365)
  colnames(all_day_survey_365) <- c("hhid",
                                    "meshblock","day","month","year",0:365)
  assign(paste("all_data_",i,sep=""), data.frame(all_day_survey_365))
}
## COMBINE ALL DATA
Names <- ls(pattern = "all_data_")
L <- mget(Names)
day_survey_all <- do.call("rbind", L) %>% na.omit()
## IN DATALAB: REMEMBER TO EXPORT TO EXCEL FILE HERE



