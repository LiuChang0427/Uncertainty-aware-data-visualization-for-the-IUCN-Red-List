library(shiny)
library(httr)
library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(reshape2)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
library(ggmap)
library(dplyr)
library(leaflet)
library(DataEditR)
library(DT)
library(ggfittext)
library(ggrepel)
library(RColorBrewer) 
library(rworldmap)
library(graphics)
library(sp)
library(sf)
library(ggthemes)

# Define server logic required to draw a histogram
server <- function(input,output,session) { 
  base_api<-"https://apiv3.iucnredlist.org/api/v3/"
  token<-"?token=1ac5642498c8cf69b5d5367a6c17711bbdfe48e3beb4ebaa038fd825679eb294"
  
  output$api_data<-renderDataTable({
    if(input$Category=="All"){
    #从api连接query物种数据
    base_api<-"https://apiv3.iucnredlist.org/api/v3/"
    token<-"?token=1ac5642498c8cf69b5d5367a6c17711bbdfe48e3beb4ebaa038fd825679eb294"
    
    #Global assessments根据页数索引
    key_words<-"species/page/"
    page_number<-input$Page
    
    my_URL<-paste0(base_api,key_words,page_number,token)
    
    raw_Data<-GET(url=my_URL)
    
    #接收JSON数据
    data_Input<-content(raw_Data,as="text")
    
    #进行数据格式转换，转化成为R list
    Global_assessments<-fromJSON(data_Input)
    
    #转化成为R data.frame,生成新的global species列表
    Global_assessments_df<-as.data.frame(do.call(rbind,Global_assessments$result))
    datatable(Global_assessments_df,options = list(scrollX=TRUE, scrollCollapse=TRUE))
    }else{
      #从api连接query物种数据
      base_api<-"https://apiv3.iucnredlist.org/api/v3/"
      token<-"?token=1ac5642498c8cf69b5d5367a6c17711bbdfe48e3beb4ebaa038fd825679eb294"
      
      #根据RL Category进行索引
      key_words<-"species/category/"
      RL_category<-input$Category
      my_URL<-paste0(base_api,key_words,RL_category,token)
      
      raw_Data<-GET(url=my_URL)
      
      #接收JSON数据
      data_Input<-content(raw_Data,as="text")
      
      #进行数据格式转换，转化成为R list
      Category_assessments<-fromJSON(data_Input)
      #转化成为R data.frame,生成新的categoryspecies列表
      Category_assessments<-as.data.frame(do.call(rbind, Category_assessments$result))
    }
  })#end of datatable
  
  # Function： FileInput
  inputFile <- reactive({
    inFile <- input$user_data
    if(is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  output$User_dataInput<-renderDataTable({
    datatable(inputFile(),options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  output$summary_user<-renderPrint({
    dataset<-inputFile()
    summary(dataset)
  })
  
  # 4个Box:
  output$Species_Assessed <-renderValueBox({
    valueBox(value="147,517",
             subtitle="more than 147,500 species have been assessed for IUCN Red List",
             icon("angle-double-right"),color="red")
  })
  
  output$Total<-renderInfoBox({
    species_count<-"speciescount"
    my_URL<-paste0(base_api,species_count,token)
    
    raw_Data<-GET(url=my_URL)
    
    #接收JSON数据
    data_Input<-content(raw_Data,as="text")
    #进行数据格式转换，转化成为R list
    Category_assessments<-fromJSON(data_Input)
    
    #赋值给infobox
    infoBox(title="Total:",value=Category_assessments$count,fill=T,color="yellow")
  })
  
  output$CR<-renderInfoBox({
    species_count<-"species/category/CR"
    my_URL<-paste0(base_api,species_count,token)
    
    raw_Data<-GET(url=my_URL)
    
    #接收JSON数据
    data_Input<-content(raw_Data,as="text")
    #进行数据格式转换，转化成为R list
    Category_assessments<-fromJSON(data_Input)
    infoBox(title="CR",value=Category_assessments$count,fill=T,color="purple")
  })
  
  output$EN <-renderInfoBox({
    species_count<-"species/category/EN"
    my_URL<-paste0(base_api,species_count,token)
    
    raw_Data<-GET(url=my_URL)
    
    #接收JSON数据
    data_Input<-content(raw_Data,as="text")
    #进行数据格式转换，转化成为R list
    Category_assessments<-fromJSON(data_Input)
    infoBox(title="EN:",value=Category_assessments$count,fill=T,color="light-blue")
  })
  
  output$VU <-renderInfoBox({
    species_count<-"species/category/VU"
    my_URL<-paste0(base_api,species_count,token)
    
    raw_Data<-GET(url=my_URL)
    
    #接收JSON数据
    data_Input<-content(raw_Data,as="text")
    #进行数据格式转换，转化成为R list
    Category_assessments<-fromJSON(data_Input)
    infoBox(title="VU:",value=Category_assessments$count,fill=T,color="olive")
  })
  
  
  #RedList Index地图
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
      geom_polygon(aes(fill=RL_Index,map_id=region))+
      scale_alpha_manual(values=c(1, .5)) +
      scale_fill_viridis_b(option="inferno",direction=1,limit=c(0,1.5),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
      theme_minimal() +
      labs(fill="Redlist Index",title = "Red List Index 1993-2022") +
      theme(legend.position = "bottom")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6))
    
    ggplotly(map_RL) 
  })
  # Species Distribution map
  #物种分布地图
  datasetInput_distribution<-reactive({
    switch(input$Species,
           "Mammals"=mammal_spatial,
           "Amphibians"=amphibians_spatial,
           "Sharks & Rays"=sharksrays_spatial,
           "Reptiles"=reptiles_spatial,
           "Marine Fish"=MarineFish_spatial,
           "Birds"=NULL,)
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
        values = c(EX = "#462C7A",
                   EW = "#d1615d",
                   CR = "#f1a2a9",
                   EN = "#e7ca60",
                   VU = "#85b6b2",
                   NT.or.LR.nt = "#2C718E",
                   LC.or.LR.lc ="#6a9f58",
                   DD = "#b8b0ac",
                   Total = "#a87c9f")
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
        values = c(EX = "#462C7A",
                   EW = "#d1615d",
                   CR = "#f1a2a9",
                   EN = "#e7ca60",
                   VU = "#85b6b2",
                   NT.or.LR.nt = "#2C718E",
                   LC.or.LR.lc ="#6a9f58",
                   DD = "#b8b0ac",
                   Total = "#a87c9f")
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
  
  datasetInput_region<-reactive({
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
    data_new_region<-head(datasetInput_region(), n = input$obs)
    regionData_new<-melt(data=data_new_region,
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
                   VU = "#85b6b2",
                   NT.or.LR.nt = "#2C718E",
                   LC.or.LR.lc ="#6a9f58",
                   DD = "#b8b0ac",
                   Total = "#a87c9f")
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
        values = c(DD = "#b8b0ac",
                   Total = "#a87c9f")
      )+
      coord_flip()+
      labs(x = "Countries' Name",fill = "Uncertainty") +
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=3),
            axis.text.y = element_text(size=3),
            axis.title = element_text(size = 10)) +
      theme(axis.text.x=element_text(size=rel(0.6), angle=40))
    ggplotly(map3)
  })
  
  #global/regional数据查看
  output$Regional_data<-renderDataTable({
    
    datatable(datasetInput_region(),options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$Regional_RLdata<-renderDataTable({
    
    datatable(datasetInput_category(),options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$summary_regional<-renderPrint({
    dataset<-datasetInput()
    summary(dataset)
  })
  
  #Usecase
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
        values = c(EX = "#462C7A",
                   EW = "#d1615d",
                   CR = "#f1a2a9",
                   EN = "#e7ca60",
                   VU = "#85b6b2",
                   NT.or.LR.nt = "#2C718E",
                   LC.or.LR.lc ="#6a9f58",
                   DD = "#b8b0ac",
                   Total = "#a87c9f")
      )+
      labs(x = "Name", y = "Amount") +
      coord_flip() +
      theme_minimal() +
      ggtitle("7 Category in each taxonomic group")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=4),
            axis.text.y = element_text(size=6),
            plot.title = element_text(size = 10))+
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
        values = c(DD = "#b8b0ac",
                   Total = "#a87c9f")
      )+ 
      labs(x="Name",y = "Percentage",
           fill = "Data Deficiency") +
      coord_flip() +
      theme_minimal()+
      ggtitle("Data Deficiency in RL_Database")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=4),
            axis.text.y = element_text(size=6),
            plot.title = element_text(size = 10))+
      theme(axis.text.x=element_text(size=rel(0.6), angle=40))    
    ggplotly(DD_plot)
  })
  
  #03 Pie
  output$pieChart<-renderPlot({
    
    if(input$radio=="01. Mammalia"){
      total_inDB=6577
    }else if(input$radio=="02. Amphibian"){
      total_inDB=8463
    }else if(input$radio=="04. Insecta"){
      total_inDB=11690
    }else{
      total_inDB=1053578
    }
    
    data_input<- datasetInput()
    total_data<-tail(data_input,1)
    
    total<-gsub(",","",total_data$Total) %>% as.numeric()
    
    NE<-total_inDB-total
    
    total_dataNew <- mutate(total_data,NE,total_inDB)
    
    
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
        values = c(EX = "#462C7A",
                   EW = "#d1615d",
                   CR = "#f1a2a9",
                   EN = "#e7ca60",
                   VU = "#85b6b2",
                   NT.or.LR.nt = "#2C718E",
                   LC.or.LR.lc ="#6a9f58",
                   DD = "#b8b0ac",
                   NE="#000000")
      )+
      theme_void()+
      ggtitle("Not Evaluated compare with data in DB")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            plot.title = element_text(size = 10))+
      geom_label_repel(data = total.data,
                       aes(label = str_c(Amount,"\n",scales::percent(percentage))),
                       size = 4, show.legend = FALSE,segment.size = 0.5,position = position_stack(vjust = 0.5),vjust="center",hjust="center",color="white") +
      guides(fill = guide_legend(title = "Red List Category"))+
      xlim(0.5, 2.5)
    
    print(pieChart)
  })
  
  #04Errorbar
  output$Errorbar<-renderPlot({
    #数据输入
    if(input$radio=="01. Mammalia"){
      total_inDB=6577
    }else if(input$radio=="02. Amphibian"){
      total_inDB=8463
    }else if(input$radio=="04. Insecta"){
      total_inDB=11690
    }else{
      total_inDB=1053578
    }
    
    data_input<- datasetInput()
    data_input$Total<-gsub(",","",data_input$Total) %>% as.numeric()
    data_input$DD<-gsub(",","",data_input$DD) %>% as.numeric()
    Total_dd<-(data_input$Total-data_input$DD)
    
    #计算NE的数量
    total_tail<-tail(data_input,1)
    total<-gsub(",","",total_tail$Total) %>% as.numeric()
    
    NE<-total_inDB-total
    total_dataNew <- mutate(data_input,Total_dd,NE,total_inDB)
    
    #求列的长度
    length_data<-length(total_dataNew$NE)%>% as.numeric()-1
    length_data
    
    total_dataNew[c(1:length_data),"NE"]<-0
    total_dataNew[c(1:length_data),"total_inDB"]<-0
    
    #长数据转换为宽数据
    uncertainty<-reshape2::melt(data=total_dataNew,
                                id.vars=c("Name","Total","total_inDB"),
                                measure.vars = c("DD","Total_dd","NE"),
                                variable.name = "Category",
                                value.name = "Amount")
    uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()
    
    
    uncertain_summary<-aggregate(Amount~Name,
                                 data = uncertainty,
                                 function(x)c(mean=mean(x),
                                              sd=sd(x),
                                              se=sd(x)/sqrt(length(x))
                                              
                                 ))
    uncertain_summary<-data.frame(Name=uncertain_summary[,1],uncertain_summary$Amount)
    
    # 4.合并数据
    mapdata_new<-left_join(total_dataNew,uncertain_summary,by="Name")
    
    
    #5.再次长数据转化为宽数据
    uncertainty<-reshape2::melt(data=mapdata_new,
                                id.vars=c("Name","mean","sd","se","total_inDB","Total"),
                                measure.vars = c("Total_dd","DD","NE"),
                                variable.name = "Category",
                                value.name = "Amount")
    uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()
    uncertainty$Category<-factor(uncertainty$Category,ordered = T,levels = c("NE","DD","Total_dd"))
    
    
    #条形图+误差线
    errorbar_data<- data.table(uncertainty)[, list(Amount = sum(Total), se_m = mean(se), Or = "Total_dd"), by = c("Name", "Category")] 
    errorbar_data
    
    barplot_error<-ggplot(uncertainty) +
      aes(x = Name, y = Amount, fill = Category) +
      geom_col(width=0.6,position=position_stack(vjust=1)) +
      labs(
        subtitle = "Uncertainty in Biodiversity",
        fill = "Uncertainty")+
      theme(legend.title = element_text(size = 6), 
            legend.text = element_text(size = 5),
            axis.text.x = element_text(size=4),
            axis.text.y = element_text(size=6))+
      coord_flip()+
      geom_errorbar(data=errorbar_data,
                    inherit.aes = FALSE,
                    width=0.4,
                    aes(x=Name,ymax = Amount+se_m, ymin=Amount-se_m), 
                    position=position_dodge(0.9), colour="red")+
      scale_fill_manual(
        values = c(NE = "#000000",
                   DD = "#b8b0ac",
                   Total_dd = "#a87c9f")
      )  +
      theme_minimal()
    
    print(barplot_error)
  })
  
  
  
  }#end of server