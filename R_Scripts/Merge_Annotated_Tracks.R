#Date April 7 2021
#Luke Wilde and Matt Duggan

# Process data for annotation in supervised learning models
library(stringr) 
library(tidyverse)
source("Functions/Data_Prep.R")

#Pathway to Field Note Data
pathway <- "Data/GPS/Field_Notes/"
#Pathway to annotated track directory
AnnotatedTrackDirectory <- "Data/GPS/Annotated/Nesting_On_Nest"
#Pathway to track directory
TrackDirectory <- "Data/GPS/Bird_Year_Cleaned_Tracks"

#Read in Field Note Information
ObsBeh <- read_csv(paste0(pathway,"Observed_Behaviors.csv"))
Fates <- read_csv(paste0(pathway,"Nest_Fates.csv")) 
NestLoc <- read_csv(paste0(pathway,"Nest_Locations.csv"))


#Add associated Transmitter ID with the respective color code. Match is found 
#in behavior observations(ObsBeh) and fate of the nest(Fates)
ObsBeh <- ObsBeh %>% 
  add_column(Fates[match(ObsBeh[[1]], Fates[[1]]), 4]) %>% 
  rename(ID = TransmitterID)


#convert UTM to longlat coordinates 
NestLoc[,c(2,3)] <- UTMtoLL(NestLoc$Easting, NestLoc$Northing)
NestLoc <- NestLoc %>% 
            rename(lng = Easting,
                   lat = Northing) %>% 
            select(-Comment)

#Make a unique ID for each bird year, such that ID is 
#<TRANSMITTER_ID>-<YEAR_DEPLOYED> in nest locations
NestLoc.ID <- makeID(NestLoc[[1]], Fates[[7]])


#Read in the annotated track information during the nesting phase
AnnotatedTrackfiles <- list.files(AnnotatedTrackDirectory)
AnnotatedTracks <- lapply(paste0(AnnotatedTrackDirectory,"/",AnnotatedTrackfiles), read_csv)

#
for(i in 1:length(AnnotatedTracks)){
  
  #Reformat annotated tracks with an additional behavior column
  AnnotatedTracks[[i]] <- AnnotatedTracks[[i]] %>% 
                            add_column(1) %>% 
                            rename(ID = 1,
                                   Behavior = "1")
  
  #
  
  
}


for(i in 1:19){
nesting.period <- read_csv((paste0("./Annotated/",files[i]))) #paste0 does not add a whitespace
head(nesting.period)

nesting.period[7] <- 1; names(nesting.period)[7] <- "behavior"; names(nesting.period)[1] <- "ID"


#bring in track to merge
str <- substr(files[i],21,29)
test <- read.csv(str_replace_all(paste("./split_cleaned_tracks_2/",str,".csv"),pattern=" ", repl="")); head(test)
test.data <- test[,c(2,3,4)]
test.data[,4] <- rep(str_sub(as.character(substr(files[i],21,29)),1,4), nrow(test.data))
colnames(test.data)[4] <- "ID" 
test.data <- test.data[,c(1:4)]

#creates Id as ID-Year
test.data[,ncol(test.data)+1] <- substring(test.data[,1],1,4)
colnames(test.data)[ncol(test.data)]<-"Year"
test.data[,6] <- paste0(test.data[,4], "-", test.data[,5])
test.data <- test.data[,c(6,2,3,1)]

#convert UTM to latlon coordinates 
coordinates(test.data) <- c("longitude", "latitude")
proj4string(test.data) <- CRS("+proj=longlat +zone=31 +datum=WGS84")

#UTM is in meters, but is based on zones - the zone of the region in Netherlands is 31N or 32N (western Netherlands)
longlat <- spTransform(test.data, CRSobj="+proj=longlat +zone=31 +datum=WGS84")
longlat_locs <- data.frame(as(longlat, "SpatialPoints"))
colnames(longlat_locs) <- c("lng", "lat")
test.data <- data.frame(test.data)[,-5]
test.data[,c(2,3)] <- longlat_locs
colnames(test.data)[c(2,3)] <- c("lng", "lat")
nest_loc<-data.frame()
#find nest site
nest_loc<- nest_id[match(test.data[,1], nest_id[,1]), c(2,3)][1,]
nest_loc<- cbind(test.data[1,1], nest_loc[1,c(1,2)], test.data[1,c(4)], 2)
colnames(nest_loc) <- c(colnames(test.data), "icons")
test.data <- cbind(test.data, 1)
colnames(test.data)[5] <- c("icons")
test.data <- rbind(nest_loc, test.data)
test.data$icons <- as.numeric(test.data$icons)
test.data <- test.data %>% filter(lat > 51)
test.data <- test.data[,1:4];names(test.data)[1]<- "ID";head(test.data)
test.data[5:7] <- NA; names(test.data)[c(5:7)] <- c("icons", "index", "behavior"); head(test.data)
test.data[7] <- -1


#join
loop_object[[i]] <- rbind(nesting.period,test.data[!test.data$date_time %in% nesting.period$date_time,])

}

nesting.period1 <- as.data.frame(do.call(rbind, loop_object)); head(nesting.period1); tail(nesting.period1)

nesting.period1 <- nesting.period1 %>% filter(icons != 2)


write.csv(nesting.period1, "./GPS Annotated/Nest Model/train_Haanmeer_gps_nesting.period.csv")



loop_object <- vector("list", length(1:19))
for(i in 1:19){
  nest.loc <- read.csv((paste0("./Annotated/",files[i+1]))) #paste0 does not add a whitespace
  head(nest.loc)
  
  nest.loc[7] <- 2; names(nest.loc)[7] <- "behavior"; names(nest.loc)[1] <- "ID"
  
  
  #bring in track to merge
  str <- substr(files[i],21,29)
  test <- read.csv(str_replace_all(paste("./split_cleaned_tracks_2/",str,".csv"),pattern=" ", repl="")); head(test)
  test.data <- test[,c(2,3,4)]
  test.data[,4] <- rep(str_sub(as.character(substr(files[i],21,29)),1,4), nrow(test.data))
  colnames(test.data)[4] <- "ID" 
  test.data <- test.data[,c(1:4)]
  
  #creates Id as ID-Year
  test.data[,ncol(test.data)+1] <- substring(test.data[,1],1,4)
  colnames(test.data)[ncol(test.data)]<-"Year"
  test.data[,6] <- paste0(test.data[,4], "-", test.data[,5])
  test.data <- test.data[,c(6,2,3,1)]
  
  #convert UTM to latlon coordinates 
  coordinates(test.data) <- c("longitude", "latitude")
  proj4string(test.data) <- CRS("+proj=longlat +zone=31 +datum=WGS84")
  
  #UTM is in meters, but is based on zones - the zone of the region in Netherlands is 31N or 32N (western Netherlands)
  longlat <- spTransform(test.data, CRSobj="+proj=longlat +zone=31 +datum=WGS84")
  longlat_locs <- data.frame(as(longlat, "SpatialPoints"))
  colnames(longlat_locs) <- c("lng", "lat")
  test.data <- data.frame(test.data)[,-5]
  test.data[,c(2,3)] <- longlat_locs
  colnames(test.data)[c(2,3)] <- c("lng", "lat")
  nest_loc<-data.frame()
  #find nest site
  nest_loc<- nest_id[match(test.data[,1], nest_id[,1]), c(2,3)][1,]
  nest_loc<- cbind(test.data[1,1], nest_loc[1,c(1,2)], test.data[1,c(4)], 2)
  colnames(nest_loc) <- c(colnames(test.data), "icons")
  test.data <- cbind(test.data, 1)
  colnames(test.data)[5] <- c("icons")
  test.data <- rbind(nest_loc, test.data)
  test.data$icons <- as.numeric(test.data$icons)
  test.data <- test.data %>% filter(lat > 51)
  test.data <- test.data[,1:4];names(test.data)[1]<- "ID";head(test.data)
  test.data[5:7] <- NA; names(test.data)[c(5:7)] <- c("icons", "index", "behavior"); head(test.data)
  test.data[7] <- -1
  
  
  #join
  loop_object[[i]] <- rbind(nest.loc,test.data[!test.data$date_time %in% nest.loc$date_time,])
  
}

nest.loc.data <- as.data.frame(do.call(rbind, loop_object))

nest.loc.data <- nest.loc.data %>% filter(icons != 2)

write.csv(nest.loc.data, "./GPS Annotated/Nest Model/train_Haanmeer_gps_nest.loc.csv")

