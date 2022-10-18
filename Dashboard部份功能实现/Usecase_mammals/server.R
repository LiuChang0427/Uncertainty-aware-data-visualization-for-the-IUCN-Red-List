library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(reshape2)
library(DT)
library(tidyverse)
library(ggfittext)
library(ggplot2)
library(ggrepel)
library(plotly)


server <- function(input, output,session) { 
  datasetInput<-reactive({
    switch (input$radio,
            "01. Mammalia"=Mammalia, 
            "02. Amphibian"=Amphibian, 
            "03. Reptilian"=Reptilian,
            "04. Insecta"=Insecta)
  })
  
  #Usecase
  #01 7Categories
  output$sevenCatgories <-renderPlotly({
    data_input<- datasetInput()
    Animal_byRLCategory<-melt(data=data_input,
                               id.vars=c("Name","Total"),
                               measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"),
                               variable.name = "RL_Category",
                               value.name = "Amount")
    Animal_byRLCategory$RL_Category<-factor(Animal_byRLCategory$RL_Category,ordered = T,levels = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"))
    Animal_byRLCategory$Total<-gsub(",","",Animal_byRLCategory$Total)%>% as.numeric()
    Animal_byRLCategory$Amount<-gsub(",","",Animal_byRLCategory$Amount) %>% as.numeric()
    
    map_Animal<-ggplot(Animal_byRLCategory) +
      aes(x = reorder(Name,-Amount), y = Amount, fill = RL_Category) +
      geom_col() +
      scale_fill_manual(
        values = c(EX = "#440154",
                   EW = "#462C7A",
                   CR = "#3A518A",
                   EN = "#2C718E",
                   VU = "#22908B",
                   NT.or.LR.nt = "#ABDB31",
                   LC.or.LR.lc ="#5FC760",
                   DD = "#A6A5AA",
                   Total = "#FDE725")
      )+
      labs(x = "Name", y = "Amount") +
      coord_flip() +
      theme_minimal() +
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=4),
            axis.text.y = element_text(size=6))+
      theme(axis.text.x=element_text(size=rel(0.6), angle=40))
    
    ggplotly(map_Animal)
    
  })
  
  #02 DD VS Total
  output$DDvsTotal<-renderPlotly({
    data_input<- datasetInput()
    DD <-melt(data=data_input,
             id.vars="Name",
             measure.vars = c("DD","Total"),
             variable.name = "Data_deficiency",
             value.name = "Amount")
    #charater转成数字
    DD$Amount<-gsub(",","",DD$Amount) %>% as.numeric()
    
    #绘图
    DD_plot<-ggplot(DD) +
      aes(x=reorder(Name,-Amount), y = Amount, fill = Data_deficiency) +
      geom_col(position=position_fill()) +
      scale_y_continuous(labels = scales::percent_format())+
      scale_fill_manual(
        values = c(DD = "#A6A5AA",Total = "#FDE725")
      )+ 
      labs(x="Name",y = "Percentage",fill = "Data Deficiency") +
      coord_flip() +
      theme_minimal()+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6))+
      theme(axis.text.x=element_text(size=rel(0.6), angle=40))    
    ggplotly(DD_plot)
  })
  
  #03 Pie
  output$pieChart<-renderPlot({
    total_data<-Mammalia[28,]
    
    total_dataNew <- mutate(total_data,NE=6577-5969,total_inDB=6577)
    
    
    total<-melt(data=total_dataNew,
                id.vars="Name",
                measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD","NE"),
                variable.name = "parts&whole",
                value.name = "Amount")
    total$Amount<-gsub(",","",total$Amount) %>% as.numeric()
    
    total.data <- total %>%
      arrange(desc(`parts&whole`)) %>%
      mutate(percentage = Amount /6577)
    
    pieChart<-ggplot(data=total.data)+
      aes(x=2, y = Amount, fill = `parts&whole`) +
      geom_bar(stat="identity",width=1,position = 'stack',alpha=0.95)+
      coord_polar(theta ="y",start = 0)+
      scale_fill_manual(
        values = c(EX = "#440154",
                   EW = "#462C7A",
                   CR = "#3A518A",
                   EN = "#2C718E",
                   VU = "#22908B",
                   NT.or.LR.nt = "#ABDB31",
                   LC.or.LR.lc ="#5FC760",
                   DD = "#A6A5AA",
                   NE="#807C93")
      )+
      theme_void()+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5))+
      geom_label_repel(data = total.data,
                       aes(label = str_c(Amount,"\n",scales::percent(percentage))),
                       size = 4, show.legend = FALSE,segment.size = 0.5,position = position_stack(vjust = 0.5),vjust="center",hjust="center") +
      guides(fill = guide_legend(title = "Red List Category"))+
      xlim(0.5, 2.5)
    
    print(pieChart)
  })
  
  #04Errorbar
  output$Errorbar<-renderPlot({})
  
  
  
  output$Scholar<-renderDataTable({
    
  })#end of renderDataTable

}#end of server
