library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(ggmap)
library(dplyr)
library(leaflet)
library(DataEditR)
library(shinyWidgets)

RLIndex<-read.csv(file="/Users/mitaonaicha/Desktop/redlist_index_data/20221001redlist-data.csv",sep = ",",header = T)
names(RLIndex)[1]
names(RLIndex)[names(RLIndex)=="Entity"]<-"region"
names(RLIndex)[4]
names(RLIndex)[names(RLIndex)=="X15.5.1...Red.List.Index...ER_RSK_LST"]<-"RL_Index"

## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(test)//不是regional数据？？？
mapdata<-map_data("world")