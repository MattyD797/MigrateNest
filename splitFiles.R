files <- list.files("cleaned_tracks")

myfiles <- lapply(paste0("cleaned_tracks/",files), read.csv)
dir.create("split_cleaned_tracks")
setwd("split_cleaned_tracks")
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