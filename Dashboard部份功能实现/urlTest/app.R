library(shiny)
library(shinydashboard)


ui <- dashboardPage(skin = "purple",
          dashboardHeader(title = "Uncertainty-aware DataVis for IUCN Red List",titleWidth = 400,
                          tags$li(class="dropdown",tags$a(href="https://www.youtube.com/watch?v=VukyqMajAOU",icon("youtube"),
                                                                    "Short Introduction",target="_blank"))), 
                
          dashboardSidebar(
                  sidebarMenu(
                      menuItem("Bibliography", 
                               tabName = "Bibliography",icon = icon("book"),
                               tags$li(class="dropdown",tags$a(href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=iucn+red+list&oq=",icon("bookmark"),
                                                               "Google Scholar",target="_blank"))
                               )
                      )
           ),#end of sidebarMenu

                  
          dashboardBody(
            tabItem(tabName="Bibliography",
            fluidRow(
                    htmlOutput("frame"))
            )
            )
)


server <- function(input, output) {  
  output$frame <- renderUI({
    key_word <- "scholar?hl=zh-CN&as_sdt=0%2C5&q=iucn+red+list&oq=IUCN"
    test <<- paste0("https://scholar.google.com/",key_word)
    my_test <- tags$iframe(src=test, height=600, width=535)
    print(my_test)
    my_test
  })
}

# Run the application 
shinyApp(ui, server )
