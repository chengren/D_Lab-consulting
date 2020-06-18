library(tidycensus)
library(leaflet)
library(dplyr)
library(sf)
library(stringr)
library(ggmap)
library(tidyverse)

#base map
data <- read.csv("https://countlove.org/data/data.csv")
colnames(data)
data $Date <- as.Date(data$Date)
floydProtests <- data %>%
  filter(Date > "2020-05-25",
         grepl(', CA', Location),
         grepl('racial justice', Tags))

########## Gecoding
#Need Google
nyc <- geocode("New York City")
#a dataframe     
#lon   lat
#<dbl> <dbl>
#1 -74.0  40.7
add <- geocode(as.character(floydProtests$Location))
floydProtests <- cbind(floydProtests,add)

#inital map
m<-leaflet() %>%
  addTiles() %>%
  setView(lng=-120,lat=37.8,zoom=5.5)%>%
  addProviderTiles(providers$CartoDB.Positron) #show the blank area
m#print m

##get census data
ca_pop <- get_acs(geography = "county", 
                     variables = c("B02001_003E", 'B01003_001'),
                     state = "CA",
                     geometry = TRUE) 
ca_pop_blm <- ca_pop 
ca_pop_blm <- ca_pop_blm %>%select(-c(moe))%>%spread(key='variable',value='estimate')
ca_pop_blm$aap <- 0
ca_pop_blm$aap <- round(ca_pop_blm$B02001_003/ca_pop_blm$B01003_001*100,2)#% of Africa America

pal <- colorBin(palette = "viridis", domain = ca_pop_blm$aap, bins = 6)
ca_pop_blm %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(aap)) %>%
  addCircles(lng = protest$Lon, lat = protest$Lat, radius = 2)%>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ aap,
            title = "% of Africa America",
            opacity = 1)








