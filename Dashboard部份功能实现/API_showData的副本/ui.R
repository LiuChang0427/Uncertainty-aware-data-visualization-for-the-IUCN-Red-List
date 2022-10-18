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
library(ggmap)
library(plotly)
library(ggplot2)

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
        menuItem("IUCN RL Data",tabName="apiData",icon=icon("dashboard"),
               tags$h5("List of Page"),
               selectInput(inputId = 'Page', label = 'Number of Page', choices =c(0:15)),
               prettyRadioButtons(inputId = "Category",label = "Choose of Category:", 
                 choices = c("All","EX", "EW", "CR", "EN", "VU", "NT","LC","DD"),
                 inline = TRUE,
                 icon = icon("user"),
                 selected = "All",
                 animation = "tada")),#end of menuItem1
        tabName = "apiData"),#end of convertMenuItem1
      
      convertMenuItem(menuItem("User_Input",tabName = "UserDataInput",icon=icon("database"),
              fileInput("user_data",'Upload Your Data File',accept = c('text/csv','text'))
              ),#end of menuItem2 
              tabName = "UserDataInput")#end of convertMenuItem2
      
    )#end of sidebarMenu
    
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName="apiData",
            fluidRow(valueBoxOutput("Species_Assessed",width=6),
                     infoBoxOutput("Total",width=3),
                     infoBoxOutput("CR",width=3),
                     infoBoxOutput("EN",width=3),
                     infoBoxOutput("VU",width=3)
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
    
    #tabItem(tabName="apiData",dataTableOutput("api_data")),
    tabItem(tabName="UserDataInput",
            tabBox(id="UserDataInput",width = 12,
                   tabPanel(title = "DataTable",dataTableOutput("User_dataInput")),
                   tabPanel(title = "Summary",verbatimTextOutput("summary_user")))
           # fluidRow(box(dataTableOutput("User_dataInput"),width=7),
            #         box(verbatimTextOutput("summary_user"),width=5))
            )
  )
  )#end of tabItems
)#end of ui


