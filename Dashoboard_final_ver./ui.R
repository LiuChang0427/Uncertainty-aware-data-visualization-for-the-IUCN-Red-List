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
library(rworldmap)
library(ggrepel)
library(rworldmap)
library(maptools)
library(scales)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Uncertainty-aware DataVis for IUCN Red List",titleWidth = 400,
                  tags$li(class="dropdown",tags$a(href="https://www.youtube.com/watch?v=VukyqMajAOU",icon("youtube"),
                                                  "Short Introduction",target="_blank"))),
  
  dashboardSidebar(
    sidebarMenu(
      convertMenuItem(
        menuItem("IUCN RL Data",tabName="apiData",icon=icon("house"),
               tags$h5("List of Page"),
               selectInput(inputId = 'Page', label = 'Number of Page', choices =c(0:15)),
               prettyRadioButtons(inputId = "Category",label = "Choose of Category:", 
                 choices = c("All","EX", "EW", "CR", "EN", "VU", "NT","LC","DD"),
                 inline = TRUE,
                 icon = icon("user"),
                 selected = "All",
                 animation = "tada")),#end of menuItem1
        tabName = "apiData"),#end of convertMenuItem1
      
      convertMenuItem(
        menuItem("Species Distribution",tabName = "Species_Distribution",icon=icon("globe"),
        #需要替换的按钮
        checkboxGroupInput(
          inputId = "Species_regional_category",
          label = "Please choose a Red List Category:", 
          choices = c("Extinct (EX)"="EX", 
                      "Extinct in the Wild (EW)"="EW", 
                      "Critically Endangered (CR)"="CR",
                      "Endangered (EN)"="EN",
                      "Vulnerable (VU)"="VU",
                      "Near Threatened (NT)"="NT.or.LR.nt",
                      "Least Concern (LC)"="LC.or.LR.lc",
                      "Data Deficient (DD)"="DD",
                      "Select All"),
          selected = "DD"),
        
        radioButtons(inputId = "Selection",label ="Countries/Region",choices = c("Global","Region"),selected ="Global" ),
        #下拉菜单，选择各个地区数据
        selectInput(inputId ="Countries_Region",
                    label="Choose Region",
                    choices=c("00. Show All",
                              "01. Antarctic",
                              "02. Caribbean Islands",
                              "03. East Asia",
                              "04. Europe",
                              "05. Mesoamerica",
                              "06. North Africa",
                              "07. North America",
                              "08. North Asia",
                              "09. Oceania",
                              "10. South America",
                              "11. South And Southeast Asia",
                              "12. Saharan Africa",
                              "13. West And Central Asia"),selected = "00. Show All"),
        
        #查看前多少行数据
        numericInput(inputId = "obs",
                     label = "Number of data to view:",
                     value = 10)),
      tabName = "Species_Distribution"),#end of convertMenuItem2
      
      convertMenuItem(menuItem("User_Input",tabName = "UserDataInput",icon=icon("database"),
              fileInput("user_data",'Upload Your Data File',accept = c('text/csv','text'))
              ),#end of menuItem2 
              tabName = "UserDataInput"),#end of convertMenuItem3
      
      convertMenuItem(
        menuItem("Use cases",tabName = "Usecase",icon=icon("tasks"),
                 prettyRadioButtons(
                   inputId = "radio",
                   label = "Choose:", 
                   choices = c("01. Mammalia", 
                               "02. Amphibian", 
                               "03. Reptilian",
                               "04. Insecta")
                 )
        ),# end of radioButton $ menuItem
      tabName = "Usecases"),
      
      convertMenuItem(
      menuItem("Bibliography", tabName = "Bibliography",icon = icon("book"),
               tags$li(class="dropdown",tags$a(href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=iucn+red+list&oq=",icon("bookmark"),
                                               "Google Scholar",target="_blank"))),
      tabName = "Bibliography")
    )#end of sidebarMenu
    
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName="apiData",
            fluidRow(valueBoxOutput("Species_Assessed",width=12)
            ),# end of fluidRow1
            fluidRow(
              box(plotlyOutput("map_RL", height = 450),width=10),#box1
              box(pickerInput(
                inputId = "Select_year",
                label = "Year:", 
                choices = c(1993,1994,1995,1996,1997,1998,1999,2000,
                            2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
                            2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,
                            2021,2022)
              ),width=2)#end of box2
              ),#end of fluidRow2
            fluidRow(dataTableOutput("api_data"))# end of fluidRow3
    ),
    
    tabItem(tabName="UserDataInput",
            tabBox(id="UserDataInput",width = 12,
                   tabPanel(title = "DataTable",dataTableOutput("User_dataInput")),
                   tabPanel(title = "Summary",verbatimTextOutput("summary_user")))
            ),#end of tabItem2
    
    tabItem(tabName="Usecase",
            fluidRow(
              box(plotlyOutput("sevenCatgories", height = 350),width=6),#01
              box(plotlyOutput("DDvsTotal", height = 350),width=6),#02
              box(plotOutput("pieChart", height = 350),width=6),#03
              box(plotOutput("Errorbar", height = 350),width=6)#04
            )),#end of tabItem3
    
    tabItem(tabName="Species_Distribution",
            fluidRow(
              box(plotlyOutput("distribution_Plot", height = 550),width=14)
            ),#end of tabItem & fluidRow01(distribution map)
    
            fluidRow(
              box(plotlyOutput("region_Plot", height = 350),width=14),
              box(plotOutput("category_Species_Plot", height = 290),width=6),
              box(plotlyOutput("uncertainty_plot", height = 290),width=6)
            ),
            
            fluidRow(
              tabBox(id="RegionalData",height = 200,width=14,
                     tabPanel(title = "DataTable",dataTableOutput("Regional_data")),
                     tabPanel(title = "RL_DataTable",dataTableOutput("Regional_RLdata")),
                     tabPanel(title = "Summary",verbatimTextOutput("summary_regional")))
            )# end of data table
          )#end of tabItem & fluidRow01(regional map)
    
  )
  )#end of tabItems
)#end of ui


