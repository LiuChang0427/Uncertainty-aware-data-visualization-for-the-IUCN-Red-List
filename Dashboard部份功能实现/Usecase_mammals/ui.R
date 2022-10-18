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


ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Usecase test01"),# end of Heard
  dashboardSidebar(
    sidebarMenu(
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
      menuItem("Bibliography", tabName = "Bibliography", icon = icon("book"))
     
      
    )#end of sidebarMenu
    
  ),# end of Sidebar
  dashboardBody(
    tabItem(tabName="Usecase",
    fluidRow(
      box(plotlyOutput("sevenCatgories", height = 350),width=6),#01
      box(plotlyOutput("DDvsTotal", height = 350),width=6),#02
      box(plotOutput("pieChart", height = 350),width=6),#03
      box(plotOutput("Errorbar", height = 350),width=6)#04
    )),
    tabItem(tabName = "Bibliography Details",dataTableOutput("Scholar"))
  )## end of Body
)