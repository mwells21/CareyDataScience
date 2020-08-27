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
library(ggthemes)
library(plotly)
library(maps)
library(ggalt)

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
                        column(
                            width = 6,
                            offset = 2,
                            box(
                                title = "Total Deaths",
                                width = 12,
                                plotOutput(outputId = "home_map")
                                )
                            ),
                        column(
                            width = 2,
                            fluidRow(
                                valueBoxOutput(outputId = "home_valuebox_1",width = 12)
                                ),
                            fluidRow(
                                valueBoxOutput(outputId = "home_valuebox_2",width = 12)
                                ),
                            fluidRow(
                                valueBoxOutput(outputId = "home_valuebox_3",width = 12)
                            ),
                            fluidRow(
                                valueBoxOutput(outputId = "home_valuebox_4",width = 12)
                            )
                    )
                    ),
                    
                    fluidRow(
                        column(
                            width = 8,
                            offset = 2,
                            box(
                                title = "Daily Deaths",width = 12,
                                plotlyOutput(outputId = "plot1")
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
    
    #---- Home - USA Mao ----
    
    output$home_map = renderPlot({
        world <- map_data("world")
        world <- world[world$region != "Antarctica", ]
        
        # trim to just the usa
        usa = world[world$region=="USA"&world$long < 0 & world$long > -130,]
        
        # Color Scheme 
        palette = c("#8FBCBB","#88C0D0","#81A1C1","#5E81AC","#BF616A",
                    "#D08770","#EBCB8B","#A3BE8C","#B48EAD","#2E3440",
                    "#3B4252","#434C5E","#4C566A")
        
        
        csse_covid_19_daily_reports_us <- read_csv("data/csse_covid_19_daily_reports_us.csv")
        
        # MAP of US - Deaths By State
        ggplot() +
            geom_cartogram(
                data = usa, map = usa,
                aes(x = long, y = lat, map_id = region),
                color = palette[[13]], fill = palette[[10]], size = 0.125
            ) +
            labs(
                x = NULL, y = NULL
            ) +
            geom_point(
                data = csse_covid_19_daily_reports_us[(csse_covid_19_daily_reports_us$Long_ < 0 & csse_covid_19_daily_reports_us$Long_ > -130 & !is.na(csse_covid_19_daily_reports_us$Long_)),],
                aes(Long_, Lat, size = Deaths),
                fill = "red",
                shape = 21, 
                alpha = 2/3, 
                stroke = 0.25, 
                color = "#2b2b2b"
            ) +
            scale_size_area(name = "Deaths", max_size = 20, labels = scales::comma) +
            theme(panel.grid = element_blank())+
            theme(axis.title.x = element_blank(),
                  axis.text.x  = element_blank(),
                  axis.ticks.x = element_blank())+
            theme(axis.title.y = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())+
            theme(plot.background   = element_rect(fill = "white", color = "white")) +
            theme(panel.background  = element_rect(fill = "white", color = "white"))+
            theme(legend.background = element_rect(fill = "white", color = "white"))+
            theme(panel.background  = element_rect(fill = "white", color = "white"))+
            theme(legend.key        = element_rect(fill = "white", color = "white"))
        
    })
    
    
    #---- Home - Daily Deaths per state ---- 
    
    output$plot1 <- renderPlotly({
        dailyChange <- read_csv("data/dailyChange.csv")
        
        state_colors = c("red4","firebrick1","darkorange4","chocolate1","orange","darkgoldenrod","gold","yellow","lightgoldenrod",rep("gray",49))
        names(state_colors) = dailyChange$state[dailyChange$date == "2020-08-19"][rev(order(dailyChange$deaths[dailyChange$date == "2020-08-19"]))]
        
        
        ggplotly(ggplot(dailyChange, aes(x = date, y = daily_change)) + 
            geom_line(aes(color = state), size = 1) +
            scale_color_manual(values = state_colors) +
            theme(legend.title = element_text(colour = "white"))+
            theme_gdocs()+
            labs(
                x = NULL, y = NULL
            )) 
        
        
    })
    
    
    # ---- Home - Value Boxes -----
    
    
    output$home_valuebox_1 = renderValueBox({
        input <- read_csv("data/csse_covid_19_daily_reports_us.csv")
        valueBox(value = sum(input$Deaths),
                 color = "red",
                 "Total Deaths",
                 icon = icon("biohazard")
        )
    })
    
    output$home_valuebox_2 = renderValueBox({
        input <- read_csv("data/csse_covid_19_daily_reports_us.csv")
        valueBox(
            value = sum(input$Active),
            color = "orange",
            "Active Cases",
            icon = icon("lungs-virus")
        )
    })
    
    output$home_valuebox_3 = renderValueBox({
        input <- read_csv("data/csse_covid_19_daily_reports_us.csv")
        valueBox(value = sum(na.omit(input$Recovered)),
                 color = "maroon",
                 "Recovered",
                 icon = icon("heart")
        )
    })
    
    output$home_valuebox_4 = renderValueBox({
        input <- read_csv("data/csse_covid_19_daily_reports_us.csv")
        valueBox(
            value = sprintf(mean(na.omit(input$Mortality_Rate)), fmt = '%#.3f'),
            color = "yellow",
            "MortalityRate",
            icon = icon("project-diagram")
        )
    })
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
