# Date April 7 2021
# Author Luke Wilde
#Script to build random forest model on GPS tagged BTGO to discern nesting from non-nesting

#variables of interest are: step length, turning angle, distance to water, revisit, residence Time, 
#other variables could be: days since arrival, days before departure, sliding window of variance, standard deviation (to capture autocorrelation)

# -1 = no observation
# 1 = incubating
# 2 = foraging 
# 3 = dead bird
# 4 = chick tending
# 5 = migrating



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

foo(c("randomForest", "m2b", "moveHMM", "momentuHMM", "dplyr", "caret","mlbench"))

rm(list=ls(all=TRUE))

as.POSIXct(Sys.time(), origin = "1970-01-01")

setwd("C:/Users/14064/Dropbox/BTGO Movmement Study/MigrateNest/")
files <- list.files("RF tracks")

myfiles <- lapply(paste0("./RF tracks/",files[1:25]), read.csv)


data <- rbind(myfiles[[1]], myfiles[[2]], myfiles[[3]], myfiles[[4]], myfiles[[5]], myfiles[[6]], myfiles[[7]], myfiles[[8]], myfiles[[9]], myfiles[[10]], myfiles[[11]], myfiles[[12]], myfiles[[13]], myfiles[[14]], myfiles[[15]], myfiles[[16]], myfiles[[17]], myfiles[[18]], myfiles[[19]], myfiles[[20]], myfiles[[21]], myfiles[[22]], myfiles[[23]], myfiles[[24]], myfiles[[25]])

#,myfiles[[9]]

str(data)
data_1 <- data[,c(4,3,6,5,1)]; str(data_1); head(data_1)

names(data_1)[1] <- "x"
names(data_1)[2] <- "y"
names(data_1)[3] <- "t"
names(data_1)[4] <- "b"
names(data_1)[5] <- "id"

str(data_1)
head(data_1);tail(data_1)

data_1$x <- as.numeric(data_1$x)
data_1$y <- -1*as.numeric(data_1$y)
data_1$b <- as.character(data_1$b)
data_1$id <- as.character(data_1$id)
data_1$t <- as.POSIXct(data_1$t, format = "%m/%d/%Y %H:%M:%S", origin = "1970-01-01")
str(data_1); head(data_1); tail(data_1)
unique(data_1$id)

data_1 <- data_1[complete.cases(data_1),]

#make two stage
data_1$b[data_1$b>1 ] <- 2 #& data_1$b<5

#track_CAGA_005 is the example data set included in the package
str(track_CAGA_005); str(data_1)

data_1$t <- as.POSIXct(data_1$t, format = "%m/%d/%Y %H:%M:%S", origin = "1970-01-01")

#build the xytb object
xytb <- xytb(data_1, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25))
#ex <- xytb(track_CAGA_005, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25))

xytb@xyt
#ex


#preview, color coded
plot(xytb)

#model with random forest
#watch out that the first t is not missing - if so, you will get a funky error "Error in `+.POSIXt`(as.POSIXct(origin, tz = "GMT", ...), x) :    binary '+' is not defined for "POSIXt" objects"
nest_rf <- modelRF(xytb, type = "actual", nob = "-1", ntree = 1201, mtry = 60)
#mtry is better as increases!





resRF(nest_rf) #to view the out of bag error limit
resRF(nest_rf, "importance") # importance of individual variables
resRF(nest_rf,"confusion") # view the confusion matrix and the statistics by each class

resB(nest_rf, nob="-1") #this now presents the behavioral states as predicted, vs the one observed - compared in time

resB(nest_rf, nob="-1", "space") # and then viewed in space

resB(nest_rf,"density", nob="-1") # finally density

nest_modRF <- extractRF(nest_rf)
plot(nest_modRF)






#make a prediction for new datasets
predict(nest_modRF, )






as.POSIXct(Sys.time(), origin = "1970-01-01")

setwd("C:/Users/14064/Dropbox/BTGO Movmement Study/MigrateNest/")
files <- list.files("RF tracks")

myfiles <- lapply(paste0("./RF tracks/",files[1:25]), read.csv)


data <- rbind(myfiles[[1]], myfiles[[2]], myfiles[[3]], myfiles[[4]], myfiles[[5]], myfiles[[6]], myfiles[[7]], myfiles[[8]], myfiles[[9]], myfiles[[10]], myfiles[[11]], myfiles[[12]], myfiles[[13]], myfiles[[14]], myfiles[[15]], myfiles[[16]], myfiles[[17]], myfiles[[18]], myfiles[[19]], myfiles[[20]], myfiles[[21]], myfiles[[22]], myfiles[[23]], myfiles[[24]], myfiles[[25]])

#,myfiles[[9]]

str(data)
data_1 <- data[,c(4,3,6,5,1)]; str(data_1); head(data_1)

names(data_1)[1] <- "x"
names(data_1)[2] <- "y"
names(data_1)[3] <- "t"
names(data_1)[4] <- "b"
names(data_1)[5] <- "id"

str(data_1)
head(data_1);tail(data_1)

data_1$x <- as.numeric(data_1$x)
data_1$y <- -1*as.numeric(data_1$y)
data_1$b <- as.character(data_1$b)
data_1$id <- as.character(data_1$id)
data_1$t <- as.POSIXct(data_1$t, format = "%m/%d/%Y %H:%M:%S", origin = "1970-01-01")
str(data_1); head(data_1); tail(data_1)
unique(data_1$id)

data_1 <- data_1[complete.cases(data_1),]

#make two stage
data_1$b[data_1$b>-1] <- 2 #& data_1$b<5

#track_CAGA_005 is the example data set included in the package
str(data_1)

data_1$t <- as.POSIXct(data_1$t, format = "%m/%d/%Y %H:%M:%S", origin = "1970-01-01")

#build the xytb object
xytb <- xytb(data_1, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25))
#ex <- xytb(track_CAGA_005, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25))

xytb@xyt
#ex


#preview, color coded
plot(xytb)

#model with random forest
#watch out that the first t is not missing - if so, you will get a funky error "Error in `+.POSIXt`(as.POSIXct(origin, tz = "GMT", ...), x) :    binary '+' is not defined for "POSIXt" objects"
chick_rf <- modelRF(xytb, type = "actual", nob = "-1", ntree = 1201, mtry = 60)
#mtry is better as increases!





resRF(chick_rf) #to view the out of bag error limit
resRF(chick_rf, "importance") # importance of individual variables
resRF(chick_rf,"confusion") # view the confusion matrix and the statistics by each class

resB(chick_rf, nob="-1") #this now presents the behavioral states as predicted, vs the one observed - compared in time

resB(chick_rf, nob="-1", "space") # and then viewed in space

resB(chick_rf,"density", nob="-1") # finally density

chick_modRF <- extractRF(chick_rf)
plot(chick_modRF)
