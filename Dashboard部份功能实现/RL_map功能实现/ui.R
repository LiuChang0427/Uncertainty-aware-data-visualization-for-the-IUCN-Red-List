library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(ggmap)
library(dplyr)
library(leaflet)
library(DataEditR)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
              fluidRow(
                box(plotlyOutput("map_RL", height = 400),width=10),
                box(pickerInput(
                  inputId = "Select_year",
                  label = "Year:", 
                  choices = c(1993,1994,1995,1996,1997,1998,1999,2000,
                              2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
                              2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,
                              2021,2022)
                  ),width=2
               )#end of box 2
              )#end of fluidrow
      
)#end of dashboardbody
)#end of dashboardPage
