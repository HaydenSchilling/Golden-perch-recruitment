# rainfall data download
#Jeffrey, S.J., Carter, J.O., Moodie, K.B. and Beswick, A.R. (2001). Using spatial interpolation to construct a comprehensive archive of Australian climate data, Environmental Modelling and Software, Vol 16/4, pp 309-330. DOI: 10.1016/S1364-8152(01)00008-1.  
#

# Download large file
library(curl)

url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/2015.monthly_rain.nc"
tmp <- "test monthly raindata.nc"
curl_download(url, tmp)

for(i in 1989:2023){
  url <- paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/",as.character(i),".monthly_rain.nc")
  tmp <- paste0("Rain Data/",as.character(i)," monthly raindata.nc")
  curl_download(url, tmp) 
}



#### Now attempt to understand the data
library(raster)

mydata <- stack("Rain Data/1993 monthly raindata.nc")
plot(mydata) ### looks like a stack of 12 (1 for each month with a gridded total monthly rainfall)

library(sf)
MDB <- sf::read_sf("../Jerom/RMaps/mdb_boundary_GDA1994.shp")
plot(MDB)

### Test cropping then get value - does crop but to rectangle extent
# tt <- crop(mydata, MDB)
# plot(tt[[1]])
# mean(getValues(tt[[1]]), na.rm=T)

## Test extract values directly
xx <- extract(mydata[[1]], MDB, fun = mean) # different to above - this one is correct


### What about WRPA specific (this is northern MDB)
WRPA <- read_sf("../database exploration/WRPA March2023/WRPA edit March2023.shp")
plot(WRPA)
northernMDB <- WRPA[WRPA$SWWRPANAME %in% c("Namoi", "Macquarie-Castlereagh",
                                           "Barwon-Darling Watercourse", "Intersecting Streams",
                                           "Gwydir", "New South Wales Border Rivers", 
                                           "Queensland Border Rivers-Moonie", "Condamine-Balonne",
                                           "Warrego-Paroo-Nebine"),]
plot(northernMDB)

## Test extract values for northern MDB
xx <- extract(mydata[[1]], northernMDB) # different to above - this one is correct
yy <- mean(unlist(xx))


### Ok make a loop to get monthly northern MDB rainfall ####
library(tidyverse)
rain_files <- list.files("Rain Data/", full.names = T)

WRPA <- read_sf("../database exploration/WRPA March2023/WRPA edit March2023.shp")
plot(WRPA)
northernMDB <- WRPA[WRPA$SWWRPANAME %in% c("Namoi", "Macquarie-Castlereagh",
                                           "Barwon-Darling Watercourse", "Intersecting Streams",
                                           "Gwydir", "New South Wales Border Rivers", 
                                           "Queensland Border Rivers-Moonie", "Condamine-Balonne",
                                           "Warrego-Paroo-Nebine"),]
plot(northernMDB)

## Test extract values for northern MDB
# xx <- extract(mydata[[1]], northernMDB) # different to above - this one is correct
# yy <- mean(unlist(xx))
year_seq <- seq(1989:2023) # not sure why this doesn't work

extracted_data <- data.frame()

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(rain_files), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=") 

for(i in 1:length(rain_files)){
  mydata <- stack(rain_files[i])
  temp_data <- data.frame(Northern_MDB_Rainfall = rep(as.numeric(NA),12), 
                          Year = rep(as.numeric(NA),12),
                          Month= rep(as.numeric(NA),12))
  for(k in 1:nlayers(mydata)){
  xx <- raster::extract(mydata[[k]], northernMDB)
  yy <- mean(unlist(xx))
  temp_data$Northern_MDB_Rainfall[k] <- yy
  temp_data$Month[k] <- k
  temp_data$year[k] <- year_seq[i]
  }
  extracted_data <- extracted_data %>% bind_rows(temp_data)
  setTxtProgressBar(pb, i)
}
extracted_data$Year = 1988+extracted_data$year

write_csv(extracted_data, "Monthly Rain northern MDB.csv")
nlayers(mydata)

ggplot(extracted_data, aes(x = Year, y = Northern_MDB_Rainfall)) + geom_line()+
  facet_wrap(~Month)

rain_summary <- extracted_data %>% filter(Month >7) %>% group_by(Year) %>%
  summarise(Aug_Dec_total_rain = sum(Northern_MDB_Rainfall, na.rm=T))


