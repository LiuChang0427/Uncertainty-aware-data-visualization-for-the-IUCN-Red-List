library(ggmap)
library(ggplot2)
library(plotly)
library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(rworldmap)
library(viridis)
data_point_pufferfish<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/SHARKS_RAYS_CHIMAERAS/PUFFERFISH_points/PUFFERFISH_points.csv")
data_point_pufferfish$category

## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(test)//不是regional数据？？？
world<-map_data("world")


ggplot()+
  geom_polygon(data = world,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
  geom_point(aes(x=data_point_pufferfish$longitude,y=data_point_pufferfish$latitude,color=data_point_pufferfish$category))+
  scale_size_continuous( range=c(1,10))+
  scale_fill_manual(
    values = c(EX = "#440154",
               EW = "#462C7A",
               CR = "#3A518A",
               EN = "#2C718E",
               VU = "#22908B",
               NT = "#ABDB31",
               LC ="#5FC760",
               DD = "#A6A5AA",
               total = "#FDE725")
  )+
  ggtitle("Distribution of pufferfish")+
  labs(
    x = "Longitude",
    y="Latitude",
    fill = "Red List Category"
  ) +
  coord_map()

#plotly绘制交互式地图:太丑了！！！
map<-ggplot()+
  geom_polygon(data = world,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
  geom_point(aes(x=data_point_pufferfish$longitude,y=data_point_pufferfish$latitude,color=data_point_pufferfish$category))+
  scale_size_continuous( range=c(1,10))+
  scale_fill_manual(
    values = c(EX = "#440154",
               EW = "#462C7A",
               CR = "#3A518A",
               EN = "#2C718E",
               VU = "#22908B",
               NT = "#ABDB31",
               LC ="#5FC760",
               DD = "#A6A5AA",
               total = "#FDE725")
  )+
  ggtitle("Distribution of pufferfish")+
  labs(
    x = "Longitude",
    y="Latitude",
    fill = "Red List Category"
  ) +
  coord_map()
ggplotly(map)






