library(sp)

#'Convert standard UTM coordinates to latitude and longitude
#' 
#'@param x Easting or Longitude coordinates 
#'@param y Northing or Latitude coordinates 
#'@param zone 

UTMtoLL <- function(x, y, coord.start = "utm", coord.end = "longlat", zone = "31", datum="WGS84"){
  
  #Placeholder table
  locUTM <- tibble(x, y)
  
  #convert UTM (Easting and Northing) to longlat coordinates 
  coordinates(locUTM) <- c(1,2)
  proj4string(locUTM) <- CRS(paste0("+proj=", 
                                    coord.start,
                                    " +zone=",
                                    zone, 
                                    " +datum=", 
                                    datum ))
  
  #UTM is in meters, but is based on zones - the zone of the region in 
  #Netherlands is 31N or 32N (western Netherlands)
  locLongLat <- spTransform(locUTM, 
                            CRSobj=paste0("+proj=",
                                          coord.end,
                                          " +zone=",
                                          zone, 
                                          " +datum=", 
                                          datum ))
  locLongLat <- tibble(data.frame(as(locLongLat, "SpatialPoints")))
  
  return(tibble(locLongLat))
}


#'Makes ID for each bird such that ID is <TRANSMITER_ID>-<YEAR_DEPLOYED> respective
#'to the NestID. 
#' 
#'@param x Dataframe containing year deployed, transmitterID, and Nest ID
#'@param y 
 
makeBurst <- function(x,y){
  
  #Match Nest ID (NestLoc) to the Nest ID (Fates) to include Transmitter ID 
  #and Year Deployed in NestLoc
  burst <-  burst %>%
                add_column(y[match(x, y), c("TransmitterID", "YearDeployed")]) %>% 
                unite("ID", 4:5, sep="-", remove=TRUE) %>% 
                relocate(ID)
  
  return(burst)
}


#TODO make a xytb

#TODO For each Burst what was the hatch date, did it hatch, last seen, nest location

#MAKE AN APP
