## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE, root.dir = "..")
#library(m2b)
library(dplyr)
library(leaflet)
library(raster)
library(shiny)
library(shinydashboard)
library(tidyr)
library(lubridate)
library(crosstalk)
library(DT)
library(stringr)
library(rgdal)


## ----Find Id and colorcode------------------------------------------------------------------------------------------------------
beh <- read.csv("gpsdata/uva_brood_monitor_hist.csv", header = T)
colour <- read.csv("gpsdata/uva.csv", header = T) 

#Match color code with ID
beh[,7] <- colour[match(beh[,1], colour[,1]), 4]
colnames(beh)[7]<-"ID"


## ----nest data reformating------------------------------------------------------------------------------------------------------

nest <- read.csv("gpsdata/uva_nest_locs.csv", header = T)
nest <- nest[,-4]

#convert UTM to latlon coordinates 
coordinates(nest) <- c("Easting", "Northing")
proj4string(nest) <- CRS("+proj=utm +zone=31 +datum=WGS84")

#UTM is in meters, but is based on zones - the zone of the region in Netherlands is 31N or 32N (western Netherlands)
longlat_nest <- spTransform(nest, CRSobj="+proj=longlat +zone=31 +datum=WGS84")
longlat_locs_nest <- data.frame(as(longlat_nest, "SpatialPoints"))

nest <- data.frame(nest)[,-4]
nest[,c(2,3)]<-longlat_locs_nest
colnames(nest)[c(2,3)] <- c("lng", "lat") 

nest[,4] <- colour[match(nest[,1], colour[,7]), 4]
nest[,5] <- colour[match(nest[,1], colour[,7]), 5]
nest[,4] <- as.character(nest[,4])
nest[,5] <- as.character(nest[,5])
nest[,4] <- paste0(nest[,4], "-", nest[,5])
nest_id <- nest[, c(4,2,3)]
colnames(nest_id)[1] <- "ID" 

head(nest_id)


## ----Reformatting and Cleaning Data---------------------------------------------------------------------------------------------
format <- function(x){
    test <- read.csv(str_replace_all(string=paste("split_cleaned_tracks_2/",x,".csv"),
                                     pattern=" ", repl=""))
    
    test.data <- test[,c(2,3,4)]
    test.data[,4] <- rep(str_sub(as.character(x),1,4), nrow(test.data))
    colnames(test.data)[4] <- "ID" 
    test.data <- test.data[,c(1:4)]
    
    test.data[,ncol(test.data)+1] <- substring(test.data[,1],1,4)
    colnames(test.data)[ncol(test.data)]<-"Year"
    test.data[,6] <- paste0(test.data[,4], "-", test.data[,5])
    test.data <- test.data[,c(6,2,3,1)]
    
    #convert UTM to latlon coordinates 
    coordinates(test.data) <- c("longitude", "latitude")
    proj4string(test.data) <- CRS("+proj=longlat +zone=31 +datum=WGS84")
    
    #UTM is in meters, but is based on zones - the zone of the region 
    #in Netherlands is 31N or 32N     (western Netherlands)
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
    return(test.data)
}


## ----test data reformatting-----------------------------------------------------------------------------------------------------
### Enter Bird ID-YYYY here 

bird_burst10022012 <- "1002-2012"
bird_burst10012012 <- "1001-2012"

test.data <- format(bird_burst10022012)
test1.data <- format(bird_burst10012012)

tests <- c(bird_burst10022012, bird_burst10012012)


## ----shiny----------------------------------------------------------------------------------------------------------------------
test.data$index <- c(1:nrow(test.data))

icons <- makeAwesomeIcon(
    icon = 'ion-egg',
    iconColor = 'black',
    library = 'ion',
    markerColor = "blue"
)
icons2 <- makeAwesomeIcon(
    icon = 'ion-close',
    iconColor = "red",
    markerColor = "red",
    library = 'ion'
)
eggIcons <- awesomeIconList(icons2, icons)

makeMenuItem <- function(x){
    return(menuItem(x, tabName = x))
}

ui <- dashboardPage(
    dashboardHeader(title = str_trim(paste("Black-Tailed Godwit", as.character(test.data$V6)[1],"")),
                    dropdownMenuOutput("messageMenu")),
    skin = "green",
    dashboardSidebar(
        sidebarMenu( 
            #for(i in 1:length(tests)){
            #makeMenuItem(tests[i])
            #}),
            menuItem("1002-2012", tabName = "1002-2012"),
            menuItem("1002-2012 Migration", tabName = "1002-2012 Migration")),
        collapsed = T
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "1002-2012",
                    #The leaflet map
                    fluidRow(
                        box(leafletOutput("mymap"), 
                            DTOutput("tab3")),
                        
                    )
            ),
            
            
            tabItem(tabName = "1002-2012 Migration",
                    #The leaflet map
                    fluidRow(
                        box(leafletOutput("migrationMap")),
                    )
            )
            
        )
    )
)



server <- function(input, output, session) {
    sd <- SharedData$new(test.data)
    output$mymap <- renderLeaflet({
        leaflet(sd) %>% 
            addTiles() %>% 
            flyTo(test.data[1,2], test.data[1,3], zoom = 20) %>% 
            addScaleBar(position="bottomleft") %>% 
            addAwesomeMarkers(icon = ~eggIcons[test.data$icons],
                              popup=paste ("<b>Date: </b>", 
                                           test.data[,4], 
                                           "<br>",
                                           "<b>Index: </b>",
                                           test.data[,5], 
                                           "<br>"),
                              label = test.data$index)%>%
            addProviderTiles(providers$OpenStreetMap, 
                             options = providerTileOptions(maxZoom = 30)) %>%
            addMeasure(
                position = "bottomleft", 
                activeColor = "#3D535D",
                completedColor = "#7D4479"
            )%>%
            addMiniMap(
                toggleDisplay = TRUE,
                tiles = providers$Esri.OpenStreetMap,
                minimized = TRUE
            ) 
        
    })
    #
    output$tab3 <- DT::renderDataTable(datatable(sd, 
                                                 extensions=c("Scroller", "Buttons"), 
                                                 style="bootstrap", 
                                                 class="compact", 
                                                 width="100%",
                                                 escape = FALSE,
                                                 options=list(dom = 'Bfrtip', 
                                                              deferRender=TRUE, 
                                                              scrollY=300, 
                                                              scroller=TRUE,
                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                                 ),
                                                 callback = JS("table.rows().every(function(i, tab, row) {
                                        var $this = $(this.node());
                                        $this.attr('id', this.data()[0]);
                                        $this.addClass('shiny-input-checkboxgroup');
                                        });
                                        Shiny.unbindAll(table.table().node());
                                        Shiny.bindAll(table.table().node());"),
                                                 rownames = FALSE),
                                       server = FALSE
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
