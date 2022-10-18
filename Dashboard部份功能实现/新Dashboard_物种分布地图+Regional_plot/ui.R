library(shiny)
library(shinydashboard)
library(shinythemes)
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

ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "Uncertainty-aware DataVis for IUCN Red List",titleWidth = 400,
                    tags$li(class="dropdown",tags$a(href="https://www.youtube.com/watch?v=VukyqMajAOU",icon("youtube"),
                                                    "Short Introduction",target="_blank"))),
    
  
    dashboardSidebar(
      selectInput(inputId ="Species",
                  label ="The select of data:",
                  choices=c("Mammals",
                            "Amphibians",
                            "Birds",
                            "Sharks & Rays",
                            "Marine Fish",
                            "Reptiles")),
      
        radioButtons(inputId = "Selection",label ="Countries/Region",choices = c("Global","Region"),selected ="Global" ),
        #下拉菜单，选择各个地区数据
        selectInput(inputId ="Countries_Region",
                    label="Choose of Region",
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
                     value = 10)
      
    ),#end of siderbar
  
  
    dashboardBody(
    tabItem(tabName="Infobox",
            fluidRow(valueBoxOutput("Species_Assessed",width=6),
                     infoBoxOutput("Goal",width=3),
                     infoBoxOutput("Species_Remaining",width=3),
                     infoBoxOutput("t3info",width=3),
                     infoBoxOutput("t4info",width=3),
                     )
    ),
    tabItem(tabName="Species_distribution_map",
            fluidRow(
            box(plotlyOutput("distribution_Plot", height = 450),width=8),
            box(title = "Controls of map",
                sliderInput("slider", label="Number of observations:", 0, 500, 2),width=4),
            box(plotOutput("smallmultiples", height = 260),width=4)
      )),#end of tabItem & fluidRow01(distribution map)
    
    tabItem(tabName="Regional_map",
            fluidRow(
              box(plotlyOutput("region_Plot", height = 350),width=14),
              box(plotOutput("category_Species_Plot", height = 260),width=6),
              box(plotlyOutput("uncertainty_plot", height = 260),width=6)
              
    ))#end of tabItem & fluidRow01(regional map)
  
)
)