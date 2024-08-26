### CALLING DATASET FROM NETCDF FILE TO DATAFRAME
## Set up working directory
rm(list=ls())
setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA")
getwd()

# load the ncdf4 package
library(ncdf4)
# set path and filename
ncpath <- "/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/"
#Reading file
daily_rain <- "VCSN_Rain5k_2015"  #file name
ncfname <- paste(daily_rain, ".nc", sep = "")
dname <- "precipitation_amount"
ncin <- nc_open(ncfname) #opening NetCDF file
print(ncin) #print basic information about the file

#Getting longitude values
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

#Getting latitude values
lat <- ncvar_get(ncin, "latitude", verbose = F)
nlat <- dim(lat)
head(lat)

#Reading the time variable
t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)
head(t)

#get the variable and its attributes, filtering the longitude and lagitude values
daily_rain_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(daily_rain_array)

#Close nc file
nc_close(ncin)

# Convert the time units string into a readable format
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
library(chron)
chron(t, origin = c(tmonth, tday, tyear))

# Replace NetCDF missing values to NA
daily_rain_array[daily_rain_array == fillvalue$value] <- NA

#Convert the nlon by nlat by nt array into a nlon by nlat by nt matrix
daily_rain_vec._long <- as.vector(daily_rain_array)
length(daily_rain_vec._long)

# Reshape vector into a matrix using the matrix() function
daily_rain_mat <- matrix(daily_rain_vec._long, nrow = nlon * nlat, ncol = nt)
dim(daily_rain_mat)
head(na.omit(daily_rain_mat,20))

# The expand.grid() function is used to create the pairs of values of longitude and latitude
lonlat <- expand.grid(lon, lat)
daily_rain_df <- data.frame(cbind(lonlat, daily_rain_mat))
options(width = 110)

#Rename Var1 and Var2 columns to Long and Lat and the remaining columns to Date
daily_rain_df <- daily_rain_df %>% dplyr::rename (Lat = Var2)
daily_rain_df <- daily_rain_df %>% dplyr::rename (Long = Var1)

### add location variables as location_1, location_2,...
daily_rain <- na.omit(daily_rain_df)
library(tibble)
daily_rain <- daily_rain %>%
  rownames_to_column("location") 

##############################################################
### convert lat/long to easting/northing NZTM ################
#### Note: since the dataset is lat/long but GPS in datalab is NZTM, we convert to nztm coordinates
# Load required libraries
library(rgdal)
library(sp)

# Define the WGS84 CRS object
wgs84 <- CRS("+init=epsg:4326")
# Define the NZTM CRS object
nztm <- CRS("+init=epsg:2193")
# prepare data frame
test <- daily_rain %>% dplyr::select(location, Long, Lat)
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

# Print the resulting data frame with the NZTM coordinates
csvpath <- "/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/"
csvname <- "latlon_to_nztm.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(nztm_coords,csvfile, row.names=FALSE, sep=",")

###----------------------------------------------------------#####
################# HERE IS ALSO THE CODE FOR NZTM TO GPS ##########
###----------------------------------------------------------#####
library(rgdal)
library(sp)
library(mapproj)

### read excel file of location list
library(readxl)
loc <- read_excel("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/3rd_paper/Rainfall-NIWA/test_location.xlsx")

# Define the NZTM projection and the WGS84 (latitude/longitude) projection
nztm_proj <- CRS("+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +datum=WGS84 +units=m +no_defs")
wgs84_proj <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# prepare data frame
test <- data.frame(loc)
# Create an empty data frame to store the NZTM coordinates
nztm_coords <- data.frame(location = numeric(), lon = numeric(), lat = numeric(), easting = numeric(), northing = numeric())

# Loop over each location and convert its WGS84 coordinates to NZTM
for (i in seq_len(nrow(test))) {
  # Convert NZTM coordinates to WGS84 (latitude/longitude)
  coords_wgs84 <- spTransform(SpatialPoints(test, proj4string = nztm_proj), wgs84_proj)
  
  # Get the lat and lon
  lat <- coords_wgs84@coords[1, 1]
  lon <- coords_wgs84@coords[1, 2]
  
  # Add the NZTM coordinates to the data frame
  
  nztm_coords[i, "lat"] <- lat
  nztm_coords[i, "lon"] <- lon
  nztm_coords[i, "location"] <- test$id[i]
  nztm_coords[i, "easting"] <- test$x_coord[i]
  nztm_coords[i, "northing"] <- test$y_coord[i]
}
