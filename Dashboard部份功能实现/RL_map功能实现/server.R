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

server <- function(input, output, session) { 
  
  output$map_RL<-renderPlotly({
    ## 1.Get API Key
    googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"
    
    ## 2. Register key
    register_google(key=googleMap_api)
    
    ##3.Generate map(test)//不是regional数据？？？
    mapdata<-map_data("world")
    
    RLIndex_new_filter<-filter(RLIndex,Year==input$Select_year)
    ##4.合并数据
    mapdata_new<-left_join(mapdata,RLIndex_new_filter,by="region")
    
    # Redlist 指数地图
    map_RL<-ggplot(mapdata_new)+
      aes(x=long,y=lat,group=group,fill="grey")+
      geom_polygon(aes(fill=RL_Index))+
      scale_alpha_manual(values=c(1, .5)) +
      scale_fill_viridis_b(option="inferno",direction=1,limit=c(0,1.5),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
      theme_minimal() +
      labs(fill="Redlist Index") +
      theme(legend.position = "bottom")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6))
    
    ggplotly(map_RL) 
    
    
  })
  

  
  

}#end of server

