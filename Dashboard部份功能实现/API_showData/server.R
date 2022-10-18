library(shiny)
library(httr)
library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(reshape2)
library(DT)
library(shinyWidgets)
library(shinydashboard)
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

  
  
  }#end of server