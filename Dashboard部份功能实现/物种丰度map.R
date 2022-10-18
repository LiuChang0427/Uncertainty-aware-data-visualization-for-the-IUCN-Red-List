library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(raster)
library(sp)
library(shiny)
library(RColorBrewer)
library(ggmap)

richnessMammals<-raster("/Users/mitaonaicha/Desktop/IUCN_Species\ Richness\ and\ Range\ Rarity\ Data/4.\ Mammals/Mammals_2021_Richness/Mammals_2021_Richness.tif")

richnessMammals_df<-as.data.frame(richnessMammals,xy=TRUE)

#2021Mammals丰度数据图
ggplot(richnessMammals_df) +
 aes(x = x, y = y, colour = Mammals_2021_Richness) +
 geom_point(shape = "circle", 
 size = 1.5) +
 scale_color_viridis_c(option = "viridis", direction = -1) +
 theme_minimal()


#2021Mammals濒危数据
thrMammals<-raster("/Users/mitaonaicha/Desktop/IUCN_Species\ Richness\ and\ Range\ Rarity\ Data/4.\ Mammals/Mammals_2021_THR_Richness/Mammals_2021_THR_Richness.tif")

thrMammals_df<-as.data.frame(thrMammals,xy=TRUE)
ggplot(thrMammals_df) +
  aes(x = x, y = y, colour = Mammals_2021_THR_Richness) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  theme_minimal()


# 丰度数据-濒危物种数据
combineRS<-(richnessMammals-thrMammals)
combineRS_df<-as.data.frame(combineRS,xy=TRUE)
ggplot(combineRS_df) +
 aes(x = x, y = y, colour = layer) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_viridis_c(option = "viridis", direction = -1) +
 theme_minimal()


# 丰度数据-濒危物种数据
combineRS_2<-(richnessMammals+thrMammals)
combineRS_2df<-as.data.frame(combineRS_2,xy=TRUE)

library(ggplot2)

ggplotly(combineRS_2df) +
 aes(x = x, y = y, colour = layer) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_viridis_c(option = "viridis", direction = -1) +
 theme_minimal()

# 丰都数据+谷歌地图
## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(test)//不是regional数据？？？
world<-map_data("world")

richnessMammals<-raster("/Users/mitaonaicha/Desktop/IUCN_Species\ Richness\ and\ Range\ Rarity\ Data/4.\ Mammals/Mammals_2021_Richness/Mammals_2021_Richness.tif")

richnessMammals_df<-as.data.frame(richnessMammals,xy=TRUE)

map<-ggplot() +
  geom_polygon(data = world,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
  geom_point(data=richnessMammals_df,aes(x = x, y = y, colour = Mammals_2021_Richness),shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  theme_minimal()
ggplotly(map)


