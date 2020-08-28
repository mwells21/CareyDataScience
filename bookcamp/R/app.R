library(shiny)
library(shinydashboard) 
library(tidyverse)
library(ggthemes)
library(plotly)
allStatesNames=""
######### Paste the code regarding collecting data and cleaning data below






ui <- dashboardPage( 
    dashboardHeader(title = "COVID-19"), 
    dashboardSidebar(
        selectInput("states", "Pick a state:",choices = allStatesNames,
                    selected = c("Alabama", "California"),
                    multiple = TRUE)
    ), 
    dashboardBody( 
            ############### You can change the value of texts inside h1, h3, p below
            h1("Title"),
            h3("subtitle"),
            p("Text"),
            column(6, plotlyOutput("plot1")),
            column(6, plotlyOutput("plot2")),
            p("Text"),
            column(6, plotlyOutput("plot3")),
            column(6, plotlyOutput("plot4")),
            p("Text"),
            p("Data source: JHU CSSE")
    ) 
)


server <- function(input, output, session) {
    updateSelectInput(session, "states",choices = allStatesNames,
                      selected = c("Alabama","California"))
    
    output$plot1 <- renderPlotly({
        stateName = input$states
        #################### Paste the code for total death visualization below

        
        
        
        
        
    })
    
    output$plot2 <- renderPlotly({
        stateName = input$states
        ####################Paste the code for total death per population visualization below

        
        
        
        
    })
    
    output$plot3 <- renderPlotly({
        stateName = input$states
        ####################Paste the code for new death visualization below

        
        
        
        
    })
    
    output$plot4 <- renderPlotly({
        stateName = input$states
        ####################Paste the code for new death per population visualization below

        
        
        
        
    })
    
}

shinyApp(ui = ui, server = server)
