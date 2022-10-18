#谷歌地图+multipolygong数据
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
## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(
google_map<-map_data("world")

#谷歌地图+multipolygon
mammalAll_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Mammals哺乳类动物/MAMMALS_all/MAMMALS.shp")
data<-head(mammalAll_spatial,50)
map<-ggplot()+
  geom_polygon(data = google_map,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
  geom_point(data=data,aes(x=SHAPE_Leng,y=SHAPE_Area,color=category,size=SHAPE_Area))+
  scale_size_area()+
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
  ggtitle("Distribution of Species")+
  labs(
    x = "Longitude",
    y="Latitude",
    fill = "Red List Category"
  ) +
  coord_map()
ggplotly(map,tooltip = "text")



map2<-ggplot()+
  geom_polygon(data = google_map,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
  geom_sf(data=data,aes(color=category,size=SHAPE_Area)) +
  scale_size_area()+
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
  ggtitle("Distribution of Species")+
  labs(
    x = "Longitude",
    y="Latitude",
    fill = "Red List Category"
  ) 

ggplotly(map2,tooltip = "text")


#smallmaltiple绘图+无交互

ggplot(data) +
  geom_polygon(data = google_map,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
  geom_sf(fill = category) +
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
  theme_minimal() +
  facet_wrap(vars(category))



