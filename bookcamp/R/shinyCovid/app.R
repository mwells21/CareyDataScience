#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(shinydashboard)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Covid-19"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Twitter", tabName = "twitter", icon = icon("th"))   
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(width = 8,
                               box(
                                   title = "Controls",
                                   plotOutput()
                                   )
                               )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "twitter",
                    h2("Tweets")
            )
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
