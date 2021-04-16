library(stringr)

setwd("C:/Users/14064/Dropbox/BTGO Movmement Study/MigrateNest/")
files <- list.files("cleaned_tracks")

# files <- str_sub(files, end=-5)

myfiles <- lapply(paste0("C:/Users/14064/Dropbox/BTGO Movmement Study/cleaned_tracks/",files), read.csv, skip = 4)
dir.create("split_cleaned_tracks_2")
setwd("split_cleaned_tracks_2")
j<-1
for(file in myfiles){
  file$date_time <- as.POSIXct(file$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "CET" )
  file$year <- format(file$date_time, format = "%Y")
  splitCSV <- split(file, file$year)
  
  for(i in 1:length(splitCSV)){
    print(getwd())
    filename <- paste0(strsplit(files[j], "_")[[1]][1], "-", unique(format(file$date_time, format = "%Y"))[i], ".csv")
    print(paste0(strsplit(list.files("cleaned_tracks")[j], "_")[[1]][1]))
    write.csv(splitCSV[[i]][-1], filename)
  }
  
  j = j+1
}
setwd("..")
getwd()
j <- 1
for(file in myfiles){
  
  j <-  j+1
}