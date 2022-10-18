library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(ggmap)
library(rworldmap)

server <- function(input, output,session) {
  datasetInput<-reactive({
    switch(input$Species,
           "Mammals"=mammal_spatial,
           "Amphibians"=amphibians_spatial,
           "Birds"=NULL,
           "Sharks & Rays"=sharksrays_spatial,
           "Reptiles"=reptiles_spatial)
  })
  
  output$distribution_Plot <- renderPlotly({
    if(input$slider ==0){
      data=datasetInput()
    }else{
      colm<-as.numeric(input$slider)
      data=head(datasetInput(),colm)
    }
    ## 1.Get API Key
    googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"
    
    ## 2. Register key
    register_google(key=googleMap_api)
    
    ##3.Generate map(
    google_map<-map_data("world")
    ##4.绘图
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
    
    
  })
  
  
  
  
  # 4个Info Box:
  output$Species_Assessed <-renderValueBox({
    valueBox(value="147,517",
            subtitle="more tha 147,500 species have been assessed for IUCN Red List",
            icon("angle-double-right"),color="red")
  })
  
  output$Goal<-renderInfoBox({
    infoBox(title="Goal:",value="160,000",fill=T,color="yellow")
  })
  
  output$Species_Remaining<-renderInfoBox({
    infoBox(title="Species_Remaining:",value="12,483",fill=T,color="light-blue")
  })
  
  output$t4th <-renderInfoBox({
    infoBox(title="还没想好:",value="Song Mino Love ya！",fill=T,color="olive")
  })

  
}