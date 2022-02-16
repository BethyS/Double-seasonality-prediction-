## 
## ## 
## ## Bethelhem S.: Select an area randomly
## #
Select_area <- function(seed)
{
packages = c(# Core Tidyverse
  "lubridate",
  "tidyverse",
  "readxl")
#lapply(packages, install.packages, character.only = TRUE)
lapply(packages, require, character.only = TRUE)
rm(packages)
options(stringsAsFactors = FALSE)
options(scipen = 999) 
cat("\014")

setwd("G:/Phd/mobile data forecasting/demisse thesis/data")
#setwd("G:/sarima/Getinet/Rthesis")
## functions to be used
# Get the world polygon and extract UK
set.seed(seed)
Base_Stations_longtiude_and_latitude <- readxl::read_excel("Base Stations longtiude and latitude.xlsx")

point_data <- data.frame (ID = as.numeric(c(1)),longitude= as.numeric(runif(1, 38.65, 38.95)),latitude= as.numeric(runif(1, 8.85, 9.1)))

make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$latitude)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}
# here is the data frame for all circles
myCircles <- make_circles(point_data, 1)
max_p_lat <- max(myCircles$lat)
min_p_lat <- min(myCircles$lat)
max_p_lon <- max(myCircles$lon)
min_p_lon <- min(myCircles$lon)


# seleceted RNC
Selected_RNC= dplyr::filter(Base_Stations_longtiude_and_latitude,between(Latitude,min_p_lat,max_p_lat)&between(Longitude,min_p_lon,max_p_lon) )

# plot
ggplot() + geom_point(data=Base_Stations_longtiude_and_latitude, aes(x=Longitude, y=Latitude))+
  geom_polygon(data = myCircles, aes(lon, lat, group = ID), color = "red", alpha = 0)


rm(Base_Stations_longtiude_and_latitude,max_p_lat,min_p_lat,max_p_lon,min_p_lon)

Selected_RNC

}
