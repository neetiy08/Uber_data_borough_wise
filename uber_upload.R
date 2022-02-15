# Load the .csv files with record of uber calls
## needed packages
library(lubridate)
library(ggmap)

apr14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-apr14.csv")
may14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-may14.csv")

#jun14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jun14.csv")
#jul14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv")
#aug14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv")
#sep14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv")

## combine into one data set
data <- rbind(apr14,may14)

## use the lubridate library to make the date/time stamps readable
library(lubridate)
# split Date/Time into separate columns in data
data$Date.Time <- mdy_hms(data$Date.Time)
data$Year <- factor(year(data$Date.Time))
data$Month <- factor(month(data$Date.Time))
data$Day <- factor(day(data$Date.Time))
data$Weekday <- factor(wday(data$Date.Time))
data$Hour <- factor(hour(data$Date.Time))
data$Minute <- factor(minute(data$Date.Time))
data$Second <- factor(second(data$Date.Time))

## check what is in data now
head(data, n=10)

## google maps api used to show data on the map. get api key first
# to get API key read instructions here, 
## https://developers.google.com/maps/documentation/javascript/get-api-key
## Dont forget to enable
##   static map API, otherwise things won't work

library(ggmap)
mykey="XXXXX" ## XXXX is the api-key from google cloud console 
ggmap::register_google(key = mykey )

## get nyc map
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71),
	maptype = "terrain", zoom = 11)
## play with zoom level to see what happens, pick another city if you like by changing lon and lat
nymap <- ggmap(nyc_map)

## show the map on plot
nymap

#check the range of longitude and latitude in the plot
nyclon=c(-74.2,-73.8)
nyclat=c(40.57,40.85)

## check max/min of Lon and Lat to see that things fall outside our map
max(data$Lon)
max(data$Lat)
min(data$Lat)
min(data$Lon)

## find data points that fall within our map
lst <- which( (data$Lon < nyclon[2]) &  (data$Lon > nyclon[1]) &
    (data$Lat < nyclat[2]) & (data$Lat > nyclat[1]) )
data2 <- data[lst,]

## data2 is now points in our map area

set.seed(20)
#clusters <- kmeans(data2[,2:3],6)
#clusters <- kmeans(data2[,2:3],7)
clusters <- kmeans(data2[,2:3],5)

## This is a real toy model. Only start of trip is recorded
## If the start and end of each trip were recorded, then there is a good
## chance of really getting borough boundaries, one issue is that the rivers/water form natural boundaries,
## except maybe where bridges and tunnels to some blending.
## What are the assumptions here ?
##a) model of distance is a euclidean measure, using latitude and longitude as x and y, which is clearly not correct
## but is useful to see what clustering does etc.

## add a borough column to our data frame, which is really the cluster label
data2$Borough <- as.factor(clusters$cluster)


## add the data points to the map, coloring them by the cluster id
nymap2 <- nymap +  geom_point(aes(x = Lon, y = Lat, color = as.factor(Borough),stroke=0, shape="."),
       data = data2)
nymap2 <- nymap2  + guides(colour = guide_legend(title="Borough",
       override.aes = list(size=8))) ## increase size of legend symbols

nymap2



#Match the the boroughs (clusters) formed is matched against the real boroughs. The cluster number corresponds to the following boroughs:
#1 Brooklyn
#2 Bronx
#3 Manhattan
#4 Staten Island
#5 Queens
#It is rough, as can be seen from the boundaries spilling over geographic borders. 
