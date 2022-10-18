library(ggplot2)
library(plotly)
library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer) 

##物种分布数据+地图
mammalAll_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Mammals哺乳类动物/MAMMALS_all/MAMMALS.shp")
data<-head(mammalAll_spatial,20)
class(mammalAll_spatial$SHAPE_Area)



ggplot(data) +
  aes(color = category,size=SHAPE_Area) +
  geom_sf(size = 1.2) +
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
  labs(
    x = "Longitude",
    y="Latitude",
    subtitle = "All Mammals Distribution",
    fill = "Red List Category"
  ) +
  theme_minimal()

# 交互式地图
map<-ggplot(data) +
  aes(fill = category) +
  geom_sf(size = 1.2) +
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
  labs(
    x = "Longitude",
    y="Latitude",
    subtitle = "All Mammals Distribution",
    fill = "Red List Category"
  ) +
  theme_minimal()

ggplotly(map)


#smallmultiple

ggplot(data) +
  aes(fill = category) +
  geom_sf(size = 1.0) +
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
  labs(fill = "Red List Category") +
  theme_minimal() +
  facet_wrap(vars(category))




