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
## Step 4: calculate the min distance between meshblock list and 11491 list of this dataset
## Step 5: combine and keep only data of meshblock GPS, which we need in the survey

## load all packages use
library(tidyverse) #to use %>% 
library(dplyr) # to select var
library(tidyr) #to pivot col to row number
library(data.table) #to use rbind list
library(zoo) #to use rollapply

for (f in 2015) {
mypath <- paste("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/VCSN_1976_2022/vcsn_",f,"/",sep="")

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
quan <- data.frame(location = numeric(), Long = numeric(), Lat = numeric(), 
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
  quan[i, "Long"] <- data$Longt[i]
  quan[i, "Lat"] <- data$Lat[i]
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
  quan[i, "Long"] <- data$Longt[i]
  quan[i, "Lat"] <- data$Lat[i]
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
  quan[i, "Long"] <- data$Longt[i]
  quan[i, "Lat"] <- data$Lat[i]
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
  quan[i, "Long"] <- data$Longt[i]
  quan[i, "Lat"] <- data$Lat[i]
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
  quan[i, "Long"] <- data$Longt[i]
  quan[i, "Lat"] <- data$Lat[i]
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
test <- quan %>% dplyr::select(location, Long, Lat)
# Create an empty data frame to store the NZTM coordinates
nztm_coords <- data.frame(location = numeric(), Long = numeric(), Lat = numeric(), easting = numeric(), northing = numeric())

# Loop over each location and convert its WGS84 coordinates to NZTM
for (i in seq_len(nrow(test))) {
  # Create a SpatialPoints object with the WGS84 coordinates
  point_wgs84 <- SpatialPoints(cbind(test$Long[i], test$Lat[i]), proj4string = wgs84)
  
  # Transform the WGS84 coordinates to NZTM
  point_nztm <- spTransform(point_wgs84, nztm)
  
  # Get the easting and northing coordinates
  easting <- point_nztm@coords[1, 1]
  northing <- point_nztm@coords[1, 2]
  
  # Add the NZTM coordinates to the data frame
  nztm_coords[i, "easting"] <- easting
  nztm_coords[i, "northing"] <- northing
  nztm_coords[i, "location"] <- test$location[i]
  nztm_coords[i, "Long"] <- test$Long[i]
  nztm_coords[i, "Lat"] <- test$Lat[i]
}

# Merge the data frames on the "location" column
merged_data <- merge(nztm_coords,quan, by = c("location","Long","Lat"))

############################################################
####--Step 4: Calculate distance between two list GPS ---###
## -----------------------------------------------------####


library(openxlsx)
write.xlsx(merged_data, paste("events_",f,".xlsx",sep = ""), sheetName = paste("quantile_",f,sep = ""), rowNames = FALSE)
}