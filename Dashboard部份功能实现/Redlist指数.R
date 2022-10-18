library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(ggmap)
library(dplyr)
library(leaflet)
library(DataEditR)
library(shinyWidgets)
library(ggrepel)


RLIndex<-read.csv(file="/Users/mitaonaicha/Desktop/redlist_index_data/20221001redlist-data.csv",sep = ",",header = T)
names(RLIndex)[1]
names(RLIndex)[names(RLIndex)=="Entity"]<-"region"
names(RLIndex)[4]
names(RLIndex)[names(RLIndex)=="X15.5.1...Red.List.Index...ER_RSK_LST"]<-"RL_Index"
View(RLIndex)
class(RLIndex$Year)


#长数据转宽数据
RLIndex_wide<-dcast(
  data = RLIndex,
  formula = region+Code~Year,
  value.var ="RL_Index"
)


## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(test)//不是regional数据？？？
mapdata<-map_data("world")
View(mapdata)
RLIndex<-filter(RLIndex,Year==2022)
##4.合并数据
mapdata_new<-left_join(mapdata,RLIndex,by="region")

# Redlist 指数地图
map_RL<-ggplot(mapdata_new)+
  aes(x=long,y=lat,group=group,fill="grey")+
  geom_polygon(aes(fill=RL_Index,map_id=region))+
  scale_fill_viridis_b(option="inferno",direction=1,limit=c(0,1.5),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
  theme_minimal() +
  labs(fill="Redlist Index",title = "Red List Index 1993-2022") +
  theme(legend.position = "bottom")+
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))
                  
ggplotly(map_RL) 


mapdata_new2<-left_join(mapdata,RLIndex_wide,by="region")

# Redlist 指数地图
map_RL<-ggplot(mapdata_new2)+
  aes(x=long,y=lat,group=group,fill="grey",alpha=0.4)+
  geom_polygon(aes(fill=`1998`))+
  scale_fill_viridis_b(option="inferno",direction=1,limit=c(0,1.5),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
  theme_minimal() +
  labs(fill="Redlist Index") +
  theme(legend.position = "bottom")+
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size = 5))

ggplotly(map_RL)


# RL指数的分组折线图
RLIndex_new<-head(RLIndex,200)
mapline<-ggplot(RLIndex_new) +
  aes(x = Year, y = RL_Index, colour = region) +
  geom_point(size=0.5)+
  geom_line()+
  ylim(c(0,1.2))+
  scale_fill_viridis() +
  theme_minimal() +
  theme(legend.position = "top")

ggplotly(mapline)








