# Date: 11/3/2020
# Author: Matt Duggan

# Script for processing the Argos data from Verhoeven et al. 2020 and formatting for nestR

#project: inferring breeding success from tags
#species: Black-tailed Godwit (BTGO, Limosa limosa)

#
#begin script
#

#### Load the packages ####

#clean old objects from R
rm(list=ls(all=TRUE))

foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#load
foo(c("dplyr", "stats","leaflet","raster","shiny","shinydashboard","tidyr","lubridate","DT"))


#see outputs in standard (not scientific notation)
options(scipen = 999)

#location of the working directory (input data)
setwd("C:/Users/14064/Dropbox/BTGO Movmement Study/BTGO git old/bltg-movement/")

#read in the data
movedata <- read.csv("./CSV/Argos_BTGO_Haanmeer.csv"); head(movedata)

unique(movedata$tag.local.identifier)

#take individuals identification, timestamp, long, and lat 
movedata_nestR <- select(movedata, tag.local.identifier, timestamp, location.lat, location.long)

#rename columns to standard format
movedata_nestR <- rename(movedata_nestR, c("ID" = "tag.local.identifier", "date_time" = "timestamp", "latitude" = "location.lat", "longitude" = "location.long"))
movedata_nestR$b <- -1

#add year to the burst column
movedata_nestR$ID <- paste(movedata$tag.local.identifier, substring(movedata$timestamp, 1, 4))

#replace " " with "-"
movedata_nestR$ID <- gsub(" ", "-", movedata_nestR$ID);head(movedata_nestR)

#Put date into POSIXct format
movedata_nestR$date_time <- as.POSIXct(as.character(movedata_nestR$date_time), tz="GMT")

movedata_nestR$t <- as.character(movedata_nestR$date_time); head(movedata_nestR)

as.vector(unique(movedata_nestR$ID))

birdyear <- as.data.frame(unique(movedata_nestR$ID)); head(birdyear); nrow(birdyear)

#
#for(i in 1:nrow(birdyear)){  tmp <- dplyr::filter(movedata_nestR, ID == birdyear[i,]) write.csv(tmp, paste("C:/Users/14064/Dropbox/BTGO Movmement Study/MigrateNest/split_clearned_Argos_tracks/",as.character(birdyear[i,]),".csv", sep=""))}


nest_data <- read.csv("C:/Users/14064/Dropbox/BTGO Movmement Study/bltg-movement/CSV/Argos Data/Mo's Birds/Verhoeven_ARGOS_info.csv"); head(nest_data)

nest_data <- dplyr::filter(nest_data, nestloc_UTM_W != "NA")

coordinates(nest_data) <- c("nestloc_UTM_W", "nestloc_UTM_N")
proj4string(nest_data) <- CRS("+proj=utm +zone=31 +datum=WGS84")

longlat_nest <- spTransform(nest_data, CRSobj="+proj=longlat +zone=31 +datum=WGS84")
longlat_locs_nest <- data.frame(as(longlat_nest, "SpatialPoints"))

nest_data_1 <- cbind(nest_data, longlat_locs_nest)
head(nest_data_1)

#write.csv(nest_data_1,"C:/Users/14064/Dropbox/BTGO Movmement Study/MigrateNest/Verhoeven_ARGOS_info_latlong.csv")
