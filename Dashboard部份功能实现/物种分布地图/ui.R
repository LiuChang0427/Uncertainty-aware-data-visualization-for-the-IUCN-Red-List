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
                            "Reptiles"))
      
    ),
  
  
    dashboardBody(
    tabItem(tabName="Infobox",
            fluidRow(valueBoxOutput("Species_Assessed",width=6),
                     infoBoxOutput("Goal",width=3),
                     infoBoxOutput("Species_Remaining",width=3),
                     infoBoxOutput("t4th",width=3),
                     )
    ),
    fluidRow(
      box(width ="80%" , height ="100%",plotlyOutput("distribution_Plot", height = 350)),
      
      box(width ="20%" , height ="100%",
        title = "Controls of map",
        sliderInput("slider", label="Number of observations:", 0, 500, 2)
      )
    )
  
)
)