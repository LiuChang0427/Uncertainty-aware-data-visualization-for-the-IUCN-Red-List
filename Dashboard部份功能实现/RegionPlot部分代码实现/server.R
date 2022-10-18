library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(reshape2)
library(DataEditR)
library(plotly)
library(viridis)
library(tidyverse)
library(DT)
library(ggthemes)

shinyServer(function(input, output) {
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
  
  output$summary<-renderPrint({
    dataset<-datasetInput()
    summary(dataset)
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
      scale_fill_tableau(
        palette = "Tableau 20",
        type = "regular",
        direction = 1
      )+
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
        values = c(EX = "#462C7A",
                   EW = "#d1615d",
                   CR = "#f1a2a9",
                   EN = "#e7ca60",
                   VU = "#2C718E",
                   NT.or.LR.nt = "#85b6b2",
                   LC.or.LR.lc ="#6a9f58",
                   DD = "#b8b0ac",
                   Total = "#a87c9f")
      )+
      labs(x = "Countries", y = "Amount")+
      coord_flip()+
      theme_minimal() +
      theme(legend.position = "top")
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
        values = c(DD = "#b8b0ac",
                   Total = "#a87c9f")
      )+
      labs(x = "Countries' Name",fill = "Uncertainty") +
      theme(axis.text.x=element_text(size=rel(0.6), angle=40))
    ggplotly(map3)
  })
    


  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

})
