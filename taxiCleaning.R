
#libraries
library(tidyverse)
library(caret)
library(DataExplorer)
library(tidygeocoder)
library(chron)
library(stringr)
library(naniar)

## Load in data
taxi.train <- read_csv("/Users/graceedriggs/Documents/Stat 495/Taxi-Duration/train.csv")
taxi.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/Taxi-Duration/test.csv")
weather <- read_csv("/Users/graceedriggs/Documents/Stat 495/Taxi-Duration/nyc_weather.csv")
weather$date <- as.Date(weather$date, tryFormats = c("%d-%m-%Y"))
#replace all T's with 0
weather$`snow depth` <- ifelse(weather$`snow depth` == "T", 0.01 ,weather$`snow depth`)
weather$`snow fall` <- ifelse(weather$`snow fall` == "T", 0.01 ,weather$`snow fall`)
weather$precipitation <- ifelse(weather$precipitation == "T", 0.01 ,weather$precipitation)

taxi <- bind_rows(taxi.train, taxi.test)
plot_missing(taxi)

## plot 
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

world <- ne_countries(scale = "medium", returnclass = "sf")

(map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = taxi, aes(x = pickup_longitude, y = pickup_latitude), size = 3, 
             shape = 23, fill = "darkred") +
    geom_point(data = taxi, aes(x = dropoff_longitude, y = dropoff_latitude), size = 3, 
               shape = 23, fill = "blue") +
    coord_sf(xlim = c(-130, -55), ylim = c(30, 50), expand = FALSE))


## format Date
taxi$dayofweek <- weekdays.POSIXt(taxi$pickup_datetime)

taxi$pickup_date <- as.Date(str_sub(taxi$pickup_datetime, 0, -10))
#taxi$dropoff_date <- as.Date(str_sub(taxi$dropoff_datetime, 0, -10))
#taxi$pickup_time <- (str_sub(taxi$pickup_datetime, -8))
#taxi$dropoff_time <- (str_sub(taxi$dropoff_datetime, -8))

## Add weather
taxi <- inner_join(taxi, weather, by = c("pickup_date" = "date"))

## calculate distance travelled
library(geosphere)

taxi$distance <- clean_taxi$distance
  
for (i in 1:nrow(taxi)){
  taxi$distance[i] <- distm(c(taxi$pickup_longitude[i], taxi$pickup_latitude[i]), c(taxi$dropoff_longitude[i], taxi$dropoff_latitude[i]), fun = distHaversine)
} #distance in meters


plot_missing(taxi)
str(taxi)

## write out
write.csv(taxi,"/Users/graceedriggs/Documents/STAT 495/Taxi-Duration/TaxiCleaned.csv", row.names = FALSE)
