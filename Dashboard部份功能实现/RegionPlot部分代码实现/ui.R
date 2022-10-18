library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(reshape2)
library(plotly)
library(viridis)
library(tidyverse)
library(DT)

shinyUI(fluidPage(

    # Page title
    titlePanel("Region Plot of IUCN Red List"),
    sidebarLayout(
        sidebarPanel(
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
          
          
        ),

        
        mainPanel(
          tabsetPanel(type="tab",
                      tabPanel("Plot",
                               plotlyOutput("region_Plot"),
                               plotOutput("category_Species_Plot"),
                               plotlyOutput("uncertainty_plot")),
                      tabPanel("Data",tableOutput("view")),
                      tabPanel("Summary",
                               verbatimTextOutput("summary"))
            
          )
        )
        
    )
))
