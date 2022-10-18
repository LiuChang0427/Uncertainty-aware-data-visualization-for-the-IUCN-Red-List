library(ggplot2)
library(plotly)
library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer) 
library(ggmap)
library(rworldmap)

##物种分布数据读取
amphibians_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/plotData/AMPHIBIANS/AMPHIBIANS.shp")
mammal_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/plotData/MAMMALS_all/MAMMALS.shp")
sharksrays_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/plotData/SHARKS_RAYS_CHIMAERAS/SHARKS_RAYS_CHIMAERAS.shp")
reptiles_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/plotData/REPTILES_all/REPTILES.shp")


## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(
google_map<-map_data("world")
