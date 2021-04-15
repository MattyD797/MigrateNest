# Date April 7 2021
# Author Luke Wilde
#Script to build random forest model on GPS tagged BTGO to discern nesting from non-nesting

#variables of interest are: step length, turning angle, distance to water, revisit, residence Time, 
#other variables could be: days since arrival, days before departure, sliding window of variance, standard deviation (to capture autocorrelation)

#create function to load and install (missing) packages

#install.packages("m2b")
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

foo(c("randomForest", "m2b", "moveHMM", "momentuHMM", "dplyr"))



data <- read.csv("./GPS Annotated/Nest Model/train_Haanmeer_gps_nesting.period_v1.csv"); head(data)

str(data)
data_1 <- data[,c(3,4,5,6,2)]; str(data_1)

names(data_1)[1] <- "x"
names(data_1)[2] <- "y"
names(data_1)[3] <- "t"
names(data_1)[4] <- "b"
names(data_1)[5] <- "id"

str(data_1)
head(data_1)

data_1$x <- as.numeric(data_1$x)
data_1$y <- as.numeric(data_1$y)
data_1$b <- as.character(data_1$b)
data_1$id <- as.character(data_1$id)
data_1$t <- as.POSIXct(data_1$t, format = "%Y-%m-%d %H:%M")
str(data_1); summary(data_1)
unique(data_1$id)

try <- data_1 %>% filter(id == "1001-2012"| id == "1002-2012"| id == "1008-2013")

xytb <- xytb(try, desc="BTGO Birds", winsize=seq(3,15,12), idquant=seq(0,1,.25))

plot(xytb)

xytb_rf <- modelRF(xytb,type="actual",ntree=501, mtry = 10)
