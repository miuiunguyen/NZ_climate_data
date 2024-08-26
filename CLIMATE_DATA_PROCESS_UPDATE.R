## Set up working directory
rm(list=ls())
setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/")
getwd()

######################## SOME NOTE ###############
## We have daily data .dat file with each day file inside each year file
## Therefore, we conduct to steps
## Step 1: combine all.dat file
## Step 2: Choose only rain data, then pivot days into wide column since long column make data so long
##--------Then calculate quantile threshole and number of extreme events which exceed quantile number
## Step 3: Convert GPS lat and long into NZTM coord since we need coord as in meshblock dataset
##-------- step 3 is just for backup, incase we need nztm coord to compare
## Step 4: calculate the min distance between meshblock list and 11491 list of this dataset
## Step 5: combine and keep only data of meshblock GPS, which we need in the survey

## load all packages use
library(tidyverse) #to use %>% 
library(dplyr) # to select var
library(tidyr) #to pivot col to row number
library(data.table) #to use rbind list
library(zoo) #to use rollapply

for (f in 2007) {
mypath <- paste("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/VCSN_1976_2022_datfile/vcsn_",f,"/",sep="")
#mypath <- "/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/VCSN_1976_2022_datfile/vcsn_2007"
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

#########################################################
####--Step 2: choose only rain and calculate extreme events 
## -----------------------------------------------------##

### Since the file is too large, we choose only rain variables for our project
data <- all_dat %>% dplyr::select("Agent","Lat","Longt", "Date", "Rain" ) %>% 
  spread(Date, Rain) # convert to wide data


### calculate number of extreme event based on percentile values and duration
### create data frame to add quantile number
quan <- data.frame(location = numeric(), lon = numeric(), lat = numeric(), 
                   quantile_99_1day = numeric(), quantile_98_1day = numeric(), quantile_95_1day = numeric(),
                   events_99_1day = numeric(), events_98_1day = numeric(), events_95_1day = numeric(),
                   quantile_99_2day = numeric(), quantile_98_2day = numeric(), quantile_95_2day = numeric(),
                   events_99_2day = numeric(), events_98_2day = numeric(), events_95_2day = numeric(),
                   quantile_99_3day = numeric(), quantile_98_3day = numeric(), quantile_95_3day = numeric(),
                   events_99_3day = numeric(), events_98_3day = numeric(), events_95_3day = numeric(),
                   quantile_99_4day = numeric(), quantile_98_4day = numeric(), quantile_95_4day = numeric(),
                   events_99_4day = numeric(), events_98_4day = numeric(), events_95_4day = numeric(),
                   quantile_99_5day = numeric(), quantile_98_5day = numeric(), quantile_95_5day = numeric(),
                   events_99_5day = numeric(), events_98_5day = numeric(), events_95_5day = numeric()
)

for (i in seq_len(nrow(data))) {
  
  rolling_sum <- data [i,] %>% 
    dplyr::select(ends_with(paste("/",f,sep="")))
  ## calculate quantile 1 day
  sum_1day <- rollapply(as.numeric(rolling_sum),width = 1, FUN = sum, align = "right", fill = NA)
  sum_1day_vector <- as.vector(sum_1day) 
  sum_1day_vector <- na.omit(sum_1day_vector)
  sum_1day_vector <- subset(sum_1day_vector,sum_1day_vector>0)
  sum_1day_vector <- unlist(sum_1day_vector)
  
  quantile_99_1day <- quantile(sum_1day_vector, probs = 0.99)
  quantile_98_1day <- quantile(sum_1day_vector, probs = 0.98)
  quantile_95_1day <- quantile(sum_1day_vector, probs = 0.95)
  
  events_99_1day <- sum(sum_1day > quantile(sum_1day_vector, probs = 0.99))
  events_98_1day <- sum(sum_1day > quantile(sum_1day_vector, probs = 0.98))
  events_95_1day <- sum(sum_1day > quantile(sum_1day_vector, probs = 0.95))
  
  quan[i, "location"] <- data$Agent[i]
  quan[i, "lon"] <- data$Longt[i]
  quan[i, "lat"] <- data$Lat[i]
  quan[i, "quantile_99_1day"] <- quantile_99_1day
  quan[i, "quantile_98_1day"] <- quantile_98_1day
  quan[i, "quantile_95_1day"] <- quantile_95_1day
  quan[i, "events_99_1day"] <- events_99_1day
  quan[i, "events_98_1day"] <- events_98_1day
  quan[i, "events_95_1day"] <- events_95_1day
  
  ## calculate quantile 2 day
  sum_2day <- rollapply(as.numeric(rolling_sum),width = 2, FUN = sum, align = "right", fill = NA)
  sum_2day_vector <- as.vector(sum_2day) 
  sum_2day_vector <- na.omit(sum_2day_vector)
  sum_2day_vector <- subset(sum_2day_vector,sum_2day_vector>0)
  sum_2day_vector <- unlist(sum_2day_vector)
  
  quantile_99_2day <- quantile(sum_2day_vector, probs = 0.99)
  quantile_98_2day <- quantile(sum_2day_vector, probs = 0.98)
  quantile_95_2day <- quantile(sum_2day_vector, probs = 0.95)
  
  events_99_2day <- sum(sum_1day > quantile(sum_2day_vector, probs = 0.99))
  events_98_2day <- sum(sum_1day > quantile(sum_2day_vector, probs = 0.98))
  events_95_2day <- sum(sum_1day > quantile(sum_2day_vector, probs = 0.95))
  
  quan[i, "location"] <- data$Agent[i]
  quan[i, "lon"] <- data$Longt[i]
  quan[i, "lat"] <- data$Lat[i]
  quan[i, "quantile_99_2day"] <- quantile_99_2day
  quan[i, "quantile_98_2day"] <- quantile_98_2day
  quan[i, "quantile_95_2day"] <- quantile_95_2day
  quan[i, "events_99_2day"] <- events_99_2day
  quan[i, "events_98_2day"] <- events_98_2day
  quan[i, "events_95_2day"] <- events_95_2day
  
  ## calculate quantile 3 day
  sum_3day <- rollapply(as.numeric(rolling_sum),width = 3, FUN = sum, align = "right", fill = NA)
  sum_3day_vector <- as.vector(sum_3day) 
  sum_3day_vector <- na.omit(sum_3day_vector)
  sum_3day_vector <- subset(sum_3day_vector,sum_3day_vector>0)
  sum_3day_vector <- unlist(sum_3day_vector)
  
  quantile_99_3day <- quantile(sum_3day_vector, probs = 0.99)
  quantile_98_3day <- quantile(sum_3day_vector, probs = 0.98)
  quantile_95_3day <- quantile(sum_3day_vector, probs = 0.95)
  
  events_99_3day <- sum(sum_1day > quantile(sum_3day_vector, probs = 0.99))
  events_98_3day <- sum(sum_1day > quantile(sum_3day_vector, probs = 0.98))
  events_95_3day <- sum(sum_1day > quantile(sum_3day_vector, probs = 0.95))
  
  quan[i, "location"] <- data$Agent[i]
  quan[i, "lon"] <- data$Longt[i]
  quan[i, "lat"] <- data$Lat[i]
  quan[i, "quantile_99_3day"] <- quantile_99_3day
  quan[i, "quantile_98_3day"] <- quantile_98_3day
  quan[i, "quantile_95_3day"] <- quantile_95_3day
  quan[i, "events_99_3day"] <- events_99_3day
  quan[i, "events_98_3day"] <- events_98_3day
  quan[i, "events_95_3day"] <- events_95_3day
  
  ## calculate quantile 4 day
  sum_4day <- rollapply(as.numeric(rolling_sum),width = 4, FUN = sum, align = "right", fill = NA)
  sum_4day_vector <- as.vector(sum_4day) 
  sum_4day_vector <- na.omit(sum_4day_vector)
  sum_4day_vector <- subset(sum_4day_vector,sum_4day_vector>0)
  sum_4day_vector <- unlist(sum_4day_vector)
  
  quantile_99_4day <- quantile(sum_4day_vector, probs = 0.99)
  quantile_98_4day <- quantile(sum_4day_vector, probs = 0.98)
  quantile_95_4day <- quantile(sum_4day_vector, probs = 0.95)
  
  events_99_4day <- sum(sum_1day > quantile(sum_4day_vector, probs = 0.99))
  events_98_4day <- sum(sum_1day > quantile(sum_4day_vector, probs = 0.98))
  events_95_4day <- sum(sum_1day > quantile(sum_4day_vector, probs = 0.95))
  
  quan[i, "location"] <- data$Agent[i]
  quan[i, "lon"] <- data$Longt[i]
  quan[i, "lat"] <- data$Lat[i]
  quan[i, "quantile_99_4day"] <- quantile_99_4day
  quan[i, "quantile_98_4day"] <- quantile_98_4day
  quan[i, "quantile_95_4day"] <- quantile_95_4day
  quan[i, "events_99_4day"] <- events_99_4day
  quan[i, "events_98_4day"] <- events_98_4day
  quan[i, "events_95_4day"] <- events_95_4day
  
  ## calculate quantile 5 day
  sum_5day <- rollapply(as.numeric(rolling_sum),width = 5, FUN = sum, align = "right", fill = NA)
  sum_5day_vector <- as.vector(sum_5day) 
  sum_5day_vector <- na.omit(sum_5day_vector)
  sum_5day_vector <- subset(sum_5day_vector,sum_5day_vector>0)
  sum_5day_vector <- unlist(sum_5day_vector)
  
  quantile_99_5day <- quantile(sum_5day_vector, probs = 0.99)
  quantile_98_5day <- quantile(sum_5day_vector, probs = 0.98)
  quantile_95_5day <- quantile(sum_5day_vector, probs = 0.95)
  
  events_99_5day <- sum(sum_1day > quantile(sum_5day_vector, probs = 0.99))
  events_98_5day <- sum(sum_1day > quantile(sum_5day_vector, probs = 0.98))
  events_95_5day <- sum(sum_1day > quantile(sum_5day_vector, probs = 0.95))
  
  quan[i, "location"] <- data$Agent[i]
  quan[i, "lon"] <- data$Longt[i]
  quan[i, "lat"] <- data$Lat[i]
  quan[i, "quantile_99_5day"] <- quantile_99_5day
  quan[i, "quantile_98_5day"] <- quantile_98_5day
  quan[i, "quantile_95_5day"] <- quantile_95_5day
  quan[i, "events_99_5day"] <- events_99_5day
  quan[i, "events_98_5day"] <- events_98_5day
  quan[i, "events_95_5day"] <- events_95_5day
}

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
test <- quan %>% dplyr::select(location, lon, lat)
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
merged_quan <- merge(nztm_coords,quan, by = c("location","lon","lat"))

library(openxlsx)
write.xlsx(merged_quan, paste("events_",f,".xlsx",sep = ""), sheetName = paste("quantile_",f,sep = ""), rowNames = FALSE)
}

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

## Now we have latlon_coords data frame with lat and lon after converting
## STEP 4.2: CALCULATE THE DISTANCE BETWEEN latlon_coords and merged_quan


### DRAFT DATA HERE ####
#########
library(readxl)
merged_quan <- read_excel("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/events_2/events_2015.xlsx")

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

#for (i in 2){
#calculate average precipitation
test <- data.frame(hhid = numeric(), meshblock = numeric(), day = numeric(), month = numeric(), year = numeric(), 
                   rain_day_survey = numeric(),rain_day_survey_1 = numeric(),
                   freq_rain_day_survey_30 = numeric(), freq_rain_day_survey_90 = numeric(), freq_rain_day_survey_183 = numeric(), freq_rain_day_survey_365 = numeric(), 
                   quantile_99_survey_30 = numeric(), quantile_99_survey_90 = numeric(), quantile_99_survey_183 = numeric(), quantile_99_survey_365 = numeric(), 
                   quantile_95_survey_30 = numeric(), quantile_95_survey_90 = numeric(), quantile_95_survey_183 = numeric(), quantile_95_survey_365 = numeric(), 
                   events_99_survey_30 = numeric(), events_99_survey_90 = numeric(), events_99_survey_183 = numeric(), events_99_survey_365 = numeric(), 
                   events_95_survey_30 = numeric(), events_95_survey_90 = numeric(), events_95_survey_183 = numeric(), events_95_survey_365 = numeric() 
)

#for (i in seq_len(nrow(distance))) {
for (i in 1:5) {
  month <- distance$month[i]
  year <- distance$year[i]
  if (distance$day[i] < 10) {
    date <- paste0(distance$month[i],".0",distance$day[i],".",distance$year[i])
  } else {
    date <- paste0(distance$month[i],".",distance$day[i],".",distance$year[i])
  }

day_survey <- which(names(distance[i,]) == date) #count the position of variables date
day_survey_1 <- day_survey - 1
day_survey_30 <- day_survey - 30
day_survey_90 <- day_survey - 90
day_survey_183 <- day_survey - 183
day_survey_365 <- day_survey - 365

all_day_survey <- distance [i,] %>% select(all_of(day_survey))
all_day_survey_1 <- distance [i,] %>% select(all_of(day_survey_1))
all_day_survey_30 <- distance [i,] %>% select(all_of(day_survey_30:day_survey_1))
all_day_survey_90 <- distance [i,] %>% select(all_of(day_survey_90:day_survey_1))
all_day_survey_183 <- distance [i,] %>% select(all_of(day_survey_183:day_survey_1))
all_day_survey_365 <- distance [i,] %>% select(all_of(day_survey_365:day_survey_1))

## calculate important values
rain_day_survey <- na.omit(as.vector(rollapply(as.numeric(all_day_survey),width = 1, FUN = sum, align = "right", fill = NA)))
rain_day_survey_1 <- na.omit(as.vector(rollapply(as.numeric(all_day_survey_1),width = 1, FUN = sum, align = "right", fill = NA)))
rain_day_survey_30 <- na.omit(as.vector(rollapply(as.numeric(all_day_survey_30),width = 1, FUN = sum, align = "right", fill = NA)))
rain_day_survey_90 <- na.omit(as.vector(rollapply(as.numeric(all_day_survey_90),width = 1, FUN = sum, align = "right", fill = NA)))
rain_day_survey_183 <- na.omit(as.vector(rollapply(as.numeric(all_day_survey_183),width = 1, FUN = sum, align = "right", fill = NA)))
rain_day_survey_365 <- na.omit(as.vector(rollapply(as.numeric(all_day_survey_365),width = 1, FUN = sum, align = "right", fill = NA)))

## count rain days 
n_rain_day_survey_30 <- length(subset(rain_day_survey_30,rain_day_survey_30>0))
n_rain_day_survey_90 <- length(subset(rain_day_survey_90,rain_day_survey_90>0))
n_rain_day_survey_183 <- length(subset(rain_day_survey_183,rain_day_survey_183>0))
n_rain_day_survey_365 <- length(subset(rain_day_survey_365,rain_day_survey_365>0))

## frequency of rain days
freq_rain_day_survey_30 <- n_rain_day_survey_30/length(rain_day_survey_30)*100
freq_rain_day_survey_90 <- n_rain_day_survey_30/length(rain_day_survey_90)*100
freq_rain_day_survey_183 <- n_rain_day_survey_30/length(rain_day_survey_183)*100
freq_rain_day_survey_365 <- n_rain_day_survey_30/length(rain_day_survey_365)*100

## calculate extreme events
quantile_99_survey_30 <- quantile(rain_day_survey_30, probs = 0.99)
quantile_99_survey_90 <- quantile(rain_day_survey_90, probs = 0.99)
quantile_99_survey_183 <- quantile(rain_day_survey_183, probs = 0.99)
quantile_99_survey_365 <- quantile(rain_day_survey_365, probs = 0.99)

quantile_95_survey_30 <- quantile(rain_day_survey_30, probs = 0.95)
quantile_95_survey_90 <- quantile(rain_day_survey_90, probs = 0.95)
quantile_95_survey_183 <- quantile(rain_day_survey_183, probs = 0.95)
quantile_95_survey_365 <- quantile(rain_day_survey_365, probs = 0.95)

events_99_survey_30 <- sum(rain_day_survey_30 > quantile_99_survey_30)
events_99_survey_90 <- sum(rain_day_survey_90 > quantile_99_survey_90)
events_99_survey_183 <- sum(rain_day_survey_183 > quantile_99_survey_183)
events_99_survey_365 <- sum(rain_day_survey_365 > quantile_99_survey_365)

events_95_survey_30 <- sum(rain_day_survey_30 > quantile_95_survey_30)
events_95_survey_90 <- sum(rain_day_survey_90 > quantile_95_survey_90)
events_95_survey_183 <- sum(rain_day_survey_183 > quantile_95_survey_183)
events_95_survey_365 <- sum(rain_day_survey_365 > quantile_95_survey_365)

test[i, "hhid"] <- distance$hhid[i]
test[i, "meshblock"] <- distance$meshblock[i]
test[i, "day"] <- distance$day[i]
test[i, "month"] <- distance$month[i]
test[i, "year"] <- distance$year[i]

test[i, "rain_day_survey"] <- rain_day_survey
test[i, "rain_day_survey_1"] <- rain_day_survey_1
test[i, "freq_rain_day_survey_30"] <- freq_rain_day_survey_30
test[i, "freq_rain_day_survey_90"] <- freq_rain_day_survey_90
test[i, "freq_rain_day_survey_183"] <- freq_rain_day_survey_183
test[i, "freq_rain_day_survey_365"] <- freq_rain_day_survey_365

test[i, "quantile_99_survey_30"] <- quantile_99_survey_30
test[i, "quantile_99_survey_90"] <- quantile_99_survey_90
test[i, "quantile_99_survey_183"] <- quantile_99_survey_183
test[i, "quantile_99_survey_365"] <- quantile_99_survey_365

test[i, "quantile_95_survey_30"] <- quantile_95_survey_30
test[i, "quantile_95_survey_90"] <- quantile_95_survey_90
test[i, "quantile_95_survey_183"] <- quantile_95_survey_183
test[i, "quantile_95_survey_365"] <- quantile_95_survey_365

test[i, "events_99_survey_30"] <- events_99_survey_30
test[i, "events_99_survey_90"] <- events_99_survey_90
test[i, "events_99_survey_183"] <- events_99_survey_183
test[i, "events_99_survey_365"] <- events_99_survey_365

test[i, "events_95_survey_30"] <- events_95_survey_30
test[i, "events_95_survey_90"] <- events_95_survey_90
test[i, "events_95_survey_183"] <- events_95_survey_183
test[i, "events_95_survey_365"] <- events_95_survey_365
}
## IN DATALAB: REMEMBER TO EXPORT TO EXCEL FILE HERE

##### TMAX DATASET
tmax_all <- read_excel("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/quantile_events/tmax_all_1.xlsx")
distance_tmax <- merge(clo_all, tmax_all, by = c("lat","lon"))

distance_tmax[, 1] <- NULL
distance_tmax[, 1] <- NULL

day <- c(23,1,3,5,13)
month <- c(1,3,5,7,12)
year <- c(2008,2012, 2020, 2018, 2017)
distance_tmax[["day"]] <- c(23,1,3,5,13)
distance_tmax[["month"]] <- c(1,3,5,7,12)
distance_tmax[["year"]] <- c(2008,2012, 2020, 2018, 2017)
distance_tmax[["hhid"]] <- c(1,2,3,4,5)
distance_tmax[["meshblock"]] <- c(1,2,3,4,5)

## remove X number in the data of names column
colnames(distance_tmax) <- gsub("X0", "", colnames(distance_tmax))
colnames(distance_tmax) <- gsub("X", "", colnames(distance_tmax))

#for (i in 2){
#calculate average precipitation
test_tmax <- data.frame(hhid = numeric(), meshblock = numeric(), day = numeric(), month = numeric(), year = numeric(), 
                   tmax_day_survey = numeric(),tmax_day_survey_1 = numeric(),
                   ave_tmax_day_survey_30 = numeric(), ave_tmax_day_survey_90 = numeric(), ave_tmax_day_survey_183 = numeric(), ave_tmax_day_survey_365 = numeric() 
)

#for (i in seq_len(nrow(distance))) {
for (i in 1:5) {
  month <- distance_tmax$month[i]
  year <- distance_tmax$year[i]
  if (distance_tmax$day[i] < 10) {
    date <- paste0(distance_tmax$month[i],".0",distance_tmax$day[i],".",distance_tmax$year[i])
  } else {
    date <- paste0(distance_tmax$month[i],".",distance_tmax$day[i],".",distance_tmax$year[i])
  }
  
  day_survey <- which(names(distance_tmax[i,]) == date) #count the position of variables date
  day_survey_1 <- day_survey - 1
  day_survey_30 <- day_survey - 30
  day_survey_90 <- day_survey - 90
  day_survey_183 <- day_survey - 183
  day_survey_365 <- day_survey - 365
  
  tmax_all_day_survey <- distance_tmax [i,] %>% select(all_of(day_survey))
  tmax_all_day_survey_1 <- distance_tmax [i,] %>% select(all_of(day_survey_1))
  tmax_all_day_survey_30 <- distance_tmax [i,] %>% select(all_of(day_survey_30:day_survey_1))
  tmax_all_day_survey_90 <- distance_tmax [i,] %>% select(all_of(day_survey_90:day_survey_1))
  tmax_all_day_survey_183 <- distance_tmax [i,] %>% select(all_of(day_survey_183:day_survey_1))
  tmax_all_day_survey_365 <- distance_tmax [i,] %>% select(all_of(day_survey_365:day_survey_1))
  
  ## calculate important values
  tmax_day_survey <- na.omit(as.vector(rollapply(as.numeric(tmax_all_day_survey),width = 1, FUN = sum, align = "right", fill = NA)))
  tmax_day_survey_1 <- na.omit(as.vector(rollapply(as.numeric(tmax_all_day_survey_1),width = 1, FUN = sum, align = "right", fill = NA)))
  tmax_day_survey_30 <- na.omit(as.vector(rollapply(as.numeric(tmax_all_day_survey_30),width = 1, FUN = sum, align = "right", fill = NA)))
  tmax_day_survey_90 <- na.omit(as.vector(rollapply(as.numeric(tmax_all_day_survey_90),width = 1, FUN = sum, align = "right", fill = NA)))
  tmax_day_survey_183 <- na.omit(as.vector(rollapply(as.numeric(tmax_all_day_survey_183),width = 1, FUN = sum, align = "right", fill = NA)))
  tmax_day_survey_365 <- na.omit(as.vector(rollapply(as.numeric(tmax_all_day_survey_365),width = 1, FUN = sum, align = "right", fill = NA)))
  
  ## frequency of rain days
  ave_tmax_day_survey_30 <- sum(tmax_day_survey_30)/30
  ave_tmax_day_survey_90 <- sum(tmax_day_survey_90)/90
  ave_tmax_day_survey_183 <- sum(tmax_day_survey_183)/183
  ave_tmax_day_survey_365 <- sum(tmax_day_survey_365)/365
  
  
  test_tmax[i, "hhid"] <- distance_tmax$hhid[i]
  test_tmax[i, "meshblock"] <- distance_tmax$meshblock[i]
  test_tmax[i, "day"] <- distance_tmax$day[i]
  test_tmax[i, "month"] <- distance_tmax$month[i]
  test_tmax[i, "year"] <- distance_tmax$year[i]
  
  test_tmax[i, "tmax_day_survey"] <- tmax_day_survey
  test_tmax[i, "tmax_day_survey_1"] <- tmax_day_survey_1
  test_tmax[i, "ave_tmax_day_survey_30"] <- ave_tmax_day_survey_30
  test_tmax[i, "ave_tmax_day_survey_90"] <- ave_tmax_day_survey_90
  test_tmax[i, "ave_tmax_day_survey_183"] <- ave_tmax_day_survey_183
  test_tmax[i, "ave_tmax_day_survey_365"] <- ave_tmax_day_survey_365
  
}

## export text_tmax to excel

##################################################################
######### PAST CODE TO CALCULATE THE EVENTS BEFORE MONTHS ########
##################################################################
  if (month==1) {
    rolling_sum_12 <- distance [i,] %>% 
      select(ends_with(paste(year-1,sep="")))
  } else {
    if (month == 2) {
      rolling_sum_12 <- distance [i,] %>% 
        dplyr::select(starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
        dplyr::select(starts_with("12."),
                      starts_with("11."),
                      starts_with("10."),
                      starts_with("9."),
                      starts_with("8."),
                      starts_with("7."),
                      starts_with("6."),
                      starts_with("5."),
                      starts_with("4."),
                      starts_with("3."),
                      starts_with("2."),ends_with(paste(year,sep="")))
    } else { 
      if (month == 3) {
        rolling_sum_12 <- distance [i,] %>% 
          dplyr::select(starts_with("2."),
                        starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
          dplyr::select(starts_with("12."),
                        starts_with("11."),
                        starts_with("10."),
                        starts_with("9."),
                        starts_with("8."),
                        starts_with("7."),
                        starts_with("6."),
                        starts_with("5."),
                        starts_with("4."),
                        starts_with("3."),ends_with(paste(year,sep="")))
      } else { 
        if (month == 4) {
          rolling_sum_12 <- distance [i,] %>% 
            dplyr::select(starts_with("3."),
                          starts_with("2."),
                          starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
            dplyr::select(starts_with("12."),
                          starts_with("11."),
                          starts_with("10."),
                          starts_with("9."),
                          starts_with("8."),
                          starts_with("7."),
                          starts_with("6."),
                          starts_with("5."),
                          starts_with("4."),ends_with(paste(year,sep="")))
        } else { 
          if (month == 5) {
            rolling_sum_12 <- distance [i,] %>% 
              dplyr::select(starts_with("4."),
                            starts_with("3."),
                            starts_with("2."),
                            starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
              dplyr::select(starts_with("12."),
                            starts_with("11."),
                            starts_with("10."),
                            starts_with("9."),
                            starts_with("8."),
                            starts_with("7."),
                            starts_with("6."),
                            starts_with("5."),ends_with(paste(year,sep="")))
          } else { 
            if (month == 6) {
              rolling_sum_12 <- distance [i,] %>% 
                dplyr::select(starts_with("5."),
                              starts_with("4."),
                              starts_with("3."),
                              starts_with("2."),
                              starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                dplyr::select(starts_with("12."),
                              starts_with("11."),
                              starts_with("10."),
                              starts_with("9."),
                              starts_with("8."),
                              starts_with("7."),
                              starts_with("6."),ends_with(paste(year,sep="")))
            } else { 
              if (month == 7) {
                rolling_sum_12 <- distance [i,] %>% 
                  dplyr::select(starts_with("6."),
                                starts_with("5."),
                                starts_with("4."),
                                starts_with("3."),
                                starts_with("2."),
                                starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                  dplyr::select(starts_with("12."),
                                starts_with("11."),
                                starts_with("10."),
                                starts_with("9."),
                                starts_with("8."),
                                starts_with("7."),ends_with(paste(year,sep="")))
              } else { 
                if (month == 8) {
                  rolling_sum_12 <- distance [i,] %>% 
                    dplyr::select(starts_with("7."),
                                  starts_with("6."),
                                  starts_with("5."),
                                  starts_with("4."),
                                  starts_with("3."),
                                  starts_with("2."),
                                  starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                    dplyr::select(starts_with("12."),
                                  starts_with("11."),
                                  starts_with("10."),
                                  starts_with("9."),
                                  starts_with("8."),ends_with(paste(year,sep="")))
                } else { 
                  if (month == 9) {
                    rolling_sum_12 <- distance [i,] %>% 
                      dplyr::select(starts_with("8."),
                                    starts_with("7."),
                                    starts_with("6."),
                                    starts_with("5."),
                                    starts_with("4."),
                                    starts_with("3."),
                                    starts_with("2."),
                                    starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                      dplyr::select(starts_with("12."),
                                    starts_with("11."),
                                    starts_with("10."),
                                    starts_with("9."),ends_with(paste(year,sep="")))
                  } else { 
                    if (month == 10) {
                      rolling_sum_12 <- distance [i,] %>% 
                        dplyr::select(starts_with("9."),
                                      starts_with("8."),
                                      starts_with("7."),
                                      starts_with("6."),
                                      starts_with("5."),
                                      starts_with("4."),
                                      starts_with("3."),
                                      starts_with("2."),
                                      starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                        dplyr::select(starts_with("12."),
                                      starts_with("11."),
                                      starts_with("10."),ends_with(paste(year,sep="")))
                    } else { 
                      if (month == 11) {
                        rolling_sum_12 <- distance [i,] %>% 
                          dplyr::select(starts_with("10."),
                                        starts_with("9."),
                                        starts_with("8."),
                                        starts_with("7."),
                                        starts_with("6."),
                                        starts_with("5."),
                                        starts_with("4."),
                                        starts_with("3."),
                                        starts_with("2."),
                                        starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                          dplyr::select(starts_with("12."),
                                        starts_with("11."),ends_with(paste(year,sep="")))
                      } else { 
                        if (month == 12) {
                          rolling_sum_12 <- distance [i,] %>% 
                            dplyr::select(starts_with("11."),
                                          starts_with("10."),
                                          starts_with("9."),
                                          starts_with("8."),
                                          starts_with("7."),
                                          starts_with("6."),
                                          starts_with("5."),
                                          starts_with("4."),
                                          starts_with("3."),
                                          starts_with("2."),
                                          starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
                            dplyr::select(starts_with("12."),ends_with(paste(year,sep="")))
            }}}}}}}}}}}}
  
  sum_12 <- rollapply(as.numeric(rolling_sum_12),width = 1, FUN = sum, align = "right", fill = NA)
  sum_12_vector <- as.vector(sum_12) 
  sum_12_vector <- na.omit(sum_12_vector)
  sum_12_vector <- subset(sum_12_vector,sum_12_vector>0)
  sum_12_vector <- unlist(sum_12_vector)
  
  ave_last_12month <- sum(sum_12)/12
  quantile_99_last_12month <- quantile(sum_12, probs = 0.99)
  quantile_95_last_12month <- quantile(sum_12_vector, probs = 0.95)
  events_99_last_12month <- sum(sum_3 > quantile_99_last_12month)
  events_95_last_12month <- sum(sum_3 > quantile_95_last_12month)
  
  test[i, "hhid"] <- distance$hhid[i]
  test[i, "meshblock"] <- distance$meshblock[i]
  test[i, "month"] <- distance$month[i]
  test[i, "year"] <- distance$year[i]
  test[i, "ave_last_12month"] <- ave_last_12month
  test[i, "quantile_99_last_12month"] <- quantile_99_last_12month
  test[i, "quantile_95_last_12month"] <- quantile_95_last_12month
  test[i, "events_99_last_12month"] <- events_99_last_12month
  test[i, "events_95_last_12month"] <- events_95_last_12month


## calculate sum and quantile 1 day for last 6 months
if (!month %in% c(1, 2, 3, 4, 5, 6)) {
  rolling_sum_6 <- distance [i,] %>% 
    dplyr::select(starts_with(paste(month-6,".",sep="")),
                  starts_with(paste(month-5,".",sep="")),
                  starts_with(paste(month-4,".",sep="")),
                  starts_with(paste(month-3,".",sep="")),
                  starts_with(paste(month-2,".",sep="")),
                  starts_with(paste(month-1,".",sep=""))) %>% 
    select(ends_with(paste(year,sep="")))
} else {
  if (month == 1) {
    rolling_sum_6 <- distance [i,] %>% 
      dplyr::select(starts_with("12."),
                    starts_with("11."),
                    starts_with("10."),
                    starts_with("9."),
                    starts_with("8."),
                    starts_with("7.")) %>% 
      select(ends_with(paste(year-1,sep="")))
  } else { 
    if (month == 2) {
      rolling_sum_6 <- distance [i,] %>% 
        dplyr::select(starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
        dplyr::select(starts_with("12."),
                      starts_with("11."),
                      starts_with("10."),
                      starts_with("9."),
                      starts_with("8."),ends_with(paste(year,sep="")))
    } else{
      if (month == 3) {
        rolling_sum_6 <- distance [i,] %>% 
          dplyr::select(starts_with("2."),
                        starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
          dplyr::select(starts_with("12."),
                        starts_with("11."),
                        starts_with("10."),
                        starts_with("9."),ends_with(paste(year,sep="")))
      } else{
        if (month == 4) {
          rolling_sum_6 <- distance [i,] %>% 
            dplyr::select(starts_with("3."),
                          starts_with("2."),
                          starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
            dplyr::select(starts_with("12."),
                          starts_with("11."),
                          starts_with("10."),ends_with(paste(year,sep="")))
        } else{
          if (month == 5) {
            rolling_sum_6 <- distance [i,] %>% 
              dplyr::select(starts_with("4."),
                            starts_with("3."),
                            starts_with("2."),
                            starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
              dplyr::select(starts_with("12."),
                            starts_with("11."),ends_with(paste(year,sep="")))
          } else{
        rolling_sum_6 <- distance [i,] %>% 
          dplyr::select(starts_with("5."),
                        starts_with("4."),
                        starts_with("3."),
                        starts_with("2."),
                        starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
          dplyr::select(starts_with("12."),ends_with(paste(year,sep="")))
          }}}}}}

sum_6 <- rollapply(as.numeric(rolling_sum_6),width = 1, FUN = sum, align = "right", fill = NA)
sum_6_vector <- as.vector(sum_6) 
sum_6_vector <- na.omit(sum_6_vector)
sum_6_vector <- subset(sum_6_vector,sum_6_vector>0)
sum_6_vector <- unlist(sum_6_vector)

ave_last_6month <- sum(sum_6)/6
quantile_99_last_6month <- quantile(sum_6, probs = 0.99)
quantile_95_last_6month <- quantile(sum_6_vector, probs = 0.95)
events_99_last_6month <- sum(sum_3 > quantile_99_last_6month)
events_95_last_6month <- sum(sum_3 > quantile_95_last_6month)

test[i, "hhid"] <- distance$hhid[i]
test[i, "meshblock"] <- distance$meshblock[i]
test[i, "month"] <- distance$month[i]
test[i, "year"] <- distance$year[i]
test[i, "ave_last_6month"] <- ave_last_6month
test[i, "quantile_99_last_6month"] <- quantile_99_last_6month
test[i, "quantile_95_last_6month"] <- quantile_95_last_6month
test[i, "events_99_last_6month"] <- events_99_last_6month
test[i, "events_95_last_6month"] <- events_95_last_6month


## calculate sum and quantile 1 day for last 3 months
if (!month %in% c(1, 2, 3)) {
  rolling_sum_3 <- distance [i,] %>% 
    dplyr::select(starts_with(paste(month-3,".",sep="")),
                  starts_with(paste(month-2,".",sep="")),
                  starts_with(paste(month-1,".",sep=""))) %>% 
    select(ends_with(paste(year,sep="")))
} else {
  if (month == 1) {
    rolling_sum_3 <- distance [i,] %>% 
      dplyr::select(starts_with("12."),
                    starts_with("11."),
                    starts_with("10.")) %>% 
             select(ends_with(paste(year-1,sep="")))
} else { 
  if (month == 2) {
  rolling_sum_3 <- distance [i,] %>% 
    dplyr::select(starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
    dplyr::select(starts_with("12."),
                  starts_with("11."),ends_with(paste(year,sep="")))
  } else{
    rolling_sum_3 <- distance [i,] %>% 
      dplyr::select(starts_with("2."),
                    starts_with("1."),ends_with(paste(year-1,sep=""))) %>% 
      dplyr::select(starts_with("12."),,ends_with(paste(year,sep="")))
  }
}
}
sum_3 <- rollapply(as.numeric(rolling_sum_3),width = 1, FUN = sum, align = "right", fill = NA)
sum_3_vector <- as.vector(sum_3) 
sum_3_vector <- na.omit(sum_3_vector)
sum_3_vector <- subset(sum_3_vector,sum_3_vector>0)
sum_3_vector <- unlist(sum_3_vector)

ave_last_3month <- sum(sum_3)/3
quantile_99_last_3month <- quantile(sum_3, probs = 0.99)
quantile_95_last_3month <- quantile(sum_3_vector, probs = 0.95)
events_99_last_3month <- sum(sum_3 > quantile_99_last_3month)
events_95_last_3month <- sum(sum_3 > quantile_95_last_3month)

test[i, "hhid"] <- distance$hhid[i]
test[i, "meshblock"] <- distance$meshblock[i]
test[i, "month"] <- distance$month[i]
test[i, "year"] <- distance$year[i]
test[i, "ave_last_3month"] <- ave_last_3month
test[i, "quantile_99_last_3month"] <- quantile_99_last_3month
test[i, "quantile_95_last_3month"] <- quantile_95_last_3month
test[i, "events_99_last_3month"] <- events_99_last_3month
test[i, "events_95_last_3month"] <- events_95_last_3month

## calculate sum and quantile 1 day for last month
if (month != 1) {
  rolling_sum <- distance [i,] %>% 
    dplyr::select(starts_with(paste(month-1,".",sep=""))) %>% select(ends_with(paste(year,sep="")))
} else {
  rolling_sum <- distance [i,] %>% 
    dplyr::select(starts_with("12.")) %>% select(ends_with(paste(year-1,sep="")))
}

  sum_1day <- rollapply(as.numeric(rolling_sum),width = 1, FUN = sum, align = "right", fill = NA)
  sum_1day_vector <- as.vector(sum_1day) 
  sum_1day_vector <- na.omit(sum_1day_vector)
  sum_1day_vector <- subset(sum_1day_vector,sum_1day_vector>0)
  sum_1day_vector <- unlist(sum_1day_vector)
  
  ave_last_1month <- sum(sum_1day_vector)
  quantile_99_1month <- quantile(sum_1day_vector, probs = 0.99)
  #quantile_98_1day <- quantile(sum_1day_vector, probs = 0.98)
  quantile_95_1month <- quantile(sum_1day_vector, probs = 0.95)
  
  events_99_1month <- sum(sum_1day > quantile_99_1month)
  #events_98_1day <- sum(sum_1day > quantile(sum_1day_vector, probs = 0.98))
  events_95_1month <- sum(sum_1day > quantile_95_1month)
  
  test[i, "hhid"] <- distance$hhid[i]
  test[i, "meshblock"] <- distance$meshblock[i]
  test[i, "month"] <- distance$month[i]
  test[i, "year"] <- distance$year[i]
  test[i, "ave_last_1month"] <- ave_last_1month
  test[i, "quantile_99_1month"] <- quantile_99_1month
  test[i, "quantile_95_1month"] <- quantile_95_1month
  test[i, "events_99_1month"] <- events_99_1month
  test[i, "events_95_1month"] <- events_95_1month
}
         
