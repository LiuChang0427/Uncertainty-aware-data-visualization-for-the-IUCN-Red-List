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

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Uncertainty-aware DataVis for IUCN Red List",titleWidth = 400,
                  tags$li(class="dropdown",tags$a(href="https://www.youtube.com/watch?v=VukyqMajAOU",icon("youtube"),
                                                  "Short Introduction",target="_blank"))),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("IUCN RL Data",tabName="apiData",icon=icon("dashboard"),
               tags$h5("List of Page"),
               selectInput(inputId = 'Page', label = 'Number of Page', choices =c(0:15)),
               prettyRadioButtons(inputId = "Category",label = "Choose of Category:", 
                 choices = c("All","EX", "EW", "CR", "EN", "VU", "NT","LC","DD"),
                 inline = TRUE,
                 icon = icon("user"),
                 selected = "All",
                 animation = "tada")
               ),#end of menuItem1
      menuItem("User_Input",tabName = "UserDataInput",icon=icon("database"),
              fileInput("user_data",'Upload Your Data File',accept = c('text/csv','text'))
               )#end of menuItem2
      
    )#end of sidebarMenu
    
  ),
  
  dashboardBody(
    tabItem(tabName="Infobox",
            fluidRow(valueBoxOutput("Species_Assessed",width=6),
                     infoBoxOutput("Total",width=3),
                     infoBoxOutput("CR",width=3),
                     infoBoxOutput("EN",width=3),
                     infoBoxOutput("VU",width=3)
            ),# end of fluidRow1
            fluidRow(dataTableOutput("api_data"))# end of fluidRow2
    ),
    
    #tabItem(tabName="apiData",dataTableOutput("api_data")),
    tabItem(tabName="UserDataInput",
            tabBox(id="UserDataInput",width = 12,
                   tabPanel(title = "userDataTable",dataTableOutput("User_dataInput")),
                   tabPanel(title = "userSummary",verbatimTextOutput("summary_user")))
           # fluidRow(box(dataTableOutput("User_dataInput"),width=7),
            #         box(verbatimTextOutput("summary_user"),width=5))
            )
  )
)#end of ui


