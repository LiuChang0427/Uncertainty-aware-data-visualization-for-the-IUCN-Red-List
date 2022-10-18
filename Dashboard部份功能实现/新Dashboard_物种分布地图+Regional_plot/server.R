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
library(graphics)
library(sp)
library(reshape2)
library(RColorBrewer) 
library(ggmap)
library(viridis)

server <- function(input, output,session) {
  #物种分布地图
  datasetInput_distribution<-reactive({
    switch(input$Species,
           "Mammals"=mammal_spatial,
           "Amphibians"=amphibians_spatial,
           "Birds"=bird_spatial,
           "Sharks & Rays"=Sharksrays_spatial,
           "Marine Fish"=marineFish_spatial,
           "Reptiles"=reptiles_spatial)
  })
  output$distribution_Plot <- renderPlotly({
    if(input$slider ==0){
      data_distribution=datasetInput_distribution()
    }else{
      colm<-as.numeric(input$slider)
      data_distribution=head(datasetInput_distribution(),colm)
    }
    ## 1.Get API Key
    googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"
    
    ## 2. Register key
    register_google(key=googleMap_api)
    
    ##3.Generate map(
    google_map<-map_data("world")
    ##4.绘图
    map<-ggplot()+
      geom_polygon(data = google_map,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
      geom_sf(data=data_distribution,aes(color=category,size=SHAPE_Area)) +
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
      )+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6),
            axis.title = element_text(size = 8))
    
    ggplotly(map,tooltip = "text")
    
  })
  
  output$smallmultiples<-renderPlot({
    if(input$slider ==0){
      data_sm=datasetInput_distribution()
    }else{
      colm<-as.numeric(input$slider)
      data_sm=head(datasetInput_distribution(),colm)
    }
    ## 1.Get API Key
    googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"
    
    ## 2. Register key
    register_google(key=googleMap_api)
    
    ##3.Generate map(
    google_map<-map_data("world")
    ggplot() +
      geom_polygon(data = google_map,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3)+
      geom_sf(data=data_sm,aes(fill = category)) +
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
      facet_wrap(vars(category))+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6))
    
  })
  
  #global/regional plot
  output$Countries_Region<-renderTable({
    if(Selection=="Global"){
      Countries_Region=ShowAll
    }else{
      Countries_Region=datasetInput
    }
  })
  
  datasetInput<-reactive({
    switch (input$Countries_Region,
            "00. Show All"=ShowAll,
            "01. Antarctic"=Antarctic,
            "02. Caribbean Islands"=Caribbean_Islands,
            "03. East Asia"=East_Asia,
            "04. Europe"=Europe,
            "05. Mesoamerica"=Mesoamerica,
            "06. North Africa"=North_Africa,
            "07. North America"=North_America,
            "08. North Asia"=North_Asia,
            "09. Oceania"=Oceania,
            "10. South America"=South_America,
            "11. South And Southeast Asia"=SouthandSoutheastAsia,
            "12. Saharan Africa"=Saharan_Africa,
            "13. West And Central Asia"=WestandCentralAsia
    )
  })
  
  datasetInput_category<-reactive({
    switch (input$Countries_Region,
            "00. Show All"=ShowAll_category,
            "01. Antarctic"=Antarctic_category,
            "02. Caribbean Islands"=Caribbean_Islands_category,
            "03. East Asia"=East_Asia_category,
            "04. Europe"=Europe_category,
            "05. Mesoamerica"=Mesoamerica_category,
            "06. North Africa"=North_Africa_category,
            "07. North America"=North_America_category,
            "08. North Asia"=North_Asia_category,
            "09. Oceania"=Oceania_category,
            "10. South America"=South_America_category,
            "11. South And Southeast Asia"=SouthandSoutheastAsia_category,
            "12. Saharan Africa"=Saharan_Africa_category,
            "13. West And Central Asia"=WestandCentralAsia_category
    )
  })
  
  output$region_Plot <- renderPlotly({
    data_new<-head(datasetInput(), n = input$obs)
    regionData_new<-melt(data=data_new,
                         id.vars = c("Name","Plants.","Fungi.","Chromists."),
                         measure.vars = c("Mammals","Birds","Reptiles.","Amphibians","Fishes.","Molluscs.","Other.Inverts.","Total"),
                         variable.name ="g.region",
                         value.name="Distribution")
    P<-ggplot(data=regionData_new) +
      aes(x = g.region, fill = Name, weight = as.numeric(Distribution))+
      geom_bar() +
      scale_fill_viridis_d() +
      labs(x = "Major Group",y="", fill = "Countries' Name") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "top") 
    ggplotly(P)
  })
  
  ## category_species绘图
  output$category_Species_Plot<- renderPlot({
    data_category<-head(datasetInput_category(), n = input$obs)
    catgory_species_new<-melt(data=data_category,
                              id.vars=c("Name","Total"),
                              measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"),
                              variable.name = "RL_Category",
                              value.name = "Amount")
    catgory_species_new$RL_Category<-factor(catgory_species_new$RL_Category,ordered = T,levels = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"))
    catgory_species_new$Total<-gsub(",","", catgory_species_new$Total)%>% as.numeric()
    catgory_species_new$Amount<-gsub(",","",catgory_species_new$Amount)%>% as.numeric()
    
    data_new<-head(catgory_species_new,n=input$obs)
    
    map2<-ggplot(data_new) +
      aes(x = Name, y = Amount, fill = RL_Category) +
      geom_col() +
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
      labs(x = "Countries", y = "Amount")+
      coord_flip()+
      theme_minimal() +
      theme(legend.position = "top")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6),
            axis.title = element_text(size = 8))
    print(map2)
  })
  
  # DD
  output$uncertainty_plot<-renderPlotly({
    data_un<-head(datasetInput_category(), n = input$obs)
    
    # Uncertainty
    data_uncertainty_byregion<-melt(data=data_un,
                                    id.vars="Name",
                                    measure.vars = c("DD","Total"),
                                    variable.name = "Uncertainty",
                                    value.name = "Amount")
    #charater转成数字
    data_uncertainty_byregion$Amount<-gsub(",","",data_uncertainty_byregion$Amount) %>% as.numeric()
    #绘图：
    map3<-ggplot(data_uncertainty_byregion) +
      aes(x = Name, y = Amount, fill = Uncertainty) +
      geom_col() +
      scale_fill_manual(
        values = c(DD = "#A6A5AA",Total = "#FDE725")
      )+
      labs(x = "Countries' Name",fill = "Uncertainty") +
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=3),
            axis.text.y = element_text(size=3),
            axis.title = element_text(size = 10)) +
      theme(axis.text.x=element_text(size=rel(0.6), angle=40))
    ggplotly(map3)
  })
  
  
}