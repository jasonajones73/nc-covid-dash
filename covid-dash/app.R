# Load primary packages
library(shiny)
library(tidyverse)
library(lubridate)
library(echarts4r)
library(leaflet)
library(htmltools)
library(bsplus)
library(countup)

# Source necessary modules
source("modules/getData.R")
source("modules/echartsModule.R")
source("modules/echartsBarModule.R")
source("modules/leafletModule.R")

# Define UI
ui <- navbarPage(
    title = "NC COVID Dashboard",
    collapsible = TRUE,
    tabPanel("Dashboard",
             fluidPage(
                 tags$link(href="https://fonts.googleapis.com/css2?family=Questrial&display=swap", rel="stylesheet"),
                 tags$style(HTML("* {font-size: 100%; font-family: Questrial, sans-serif;}")),
                 tags$style(".panel-title {font-size: 20px;} .control-label {font-size: 20px}"),
                 fluidRow(
                     column(width = 5,
                            fluidRow(uiOutput("state_total")),
                            fluidRow(column(width = 12,
                                            selectInput("countySelection",
                                                        label = "Please select a county:",
                                                        choices = unique(combined$Admin2),
                                                        selected = "Gaston",
                                                        selectize = TRUE, width = "100%"))),
                            fluidRow(uiOutput("county_total"))),
                     column(width = 7,
                            bs_accordion(id = "main_charts") %>%
                                bs_append(title = "Weekly Confirmed Cases",
                                          content = echartUI("chart4")) %>%
                                bs_append(title = "Daily Confirmed Cases",
                                          content = echartUI("chart1")) %>%
                                bs_append(title = "Cumulative Confirmed Cases",
                                          content = echartUI("chart3")) %>%
                                bs_append(title = "Daily Deaths",
                                          content = echartUI("chart2")) %>%
                                bs_append(title = "Confirmed Cases by Day of Week",
                                          content = echartUI("chart5")) %>%
                                bs_append(title = "County Map of Cases per 10,000 Residents",
                                          content = leafletMapUI("map")),
                            p(sprintf('Data updated from the Johns Hopkins GitHub repository automatically and is current as of %s', format(unique(confirmed_map$date), '%b %d, %Y')))
                            )
                     )
                 )
             ),
    tabPanel("About",
             fluidPage(h1("Data Source"),
                       p("All of the data used for the creation of this Shiny web application can be found within the",
                         tags$a("COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.",
                                href = "https://github.com/CSSEGISandData/COVID-19")
                         )
                       )
             )
    )

# Define server
server <- function(input, output) {
    
    # Filtering data for county selection
    confirmed_filt <- reactive({
        confirmed %>%
            filter(Admin2 == input$countySelection)
    })
    
    deaths_filt <- reactive({
        deaths %>%
            filter(Admin2 == input$countySelection)
    })
    
    by_week_filt <- reactive({
        by_week %>%
            filter(Admin2 == input$countySelection)
    })
    
    by_dow_filt <- reactive({
        by_dow %>%
            filter(Admin2 == input$countySelection)
    })
    
    info_box <- reactive({
        confirmed_map %>%
            filter(Admin2 == input$countySelection)
    })
    
    # This is a series of module calls to produce the necessary charts from the 
    # chart module. Every chart is being produced from the same module
    callModule(echartServer, "chart1", df = confirmed_filt,
               column_name = "daily_cases", chart_name = "Daily Confirmed Cases")
    
    callModule(echartServer, "chart2", df = deaths_filt,
               column_name = "daily_deaths", chart_name = "Daily Deaths")
    
    callModule(echartServer, "chart3", df = confirmed_filt,
               column_name = "cases", chart_name = "Cumulative Cases")
    
    callModule(echartServer, "chart4", df = by_week_filt,
               column_name = "week_total", chart_name = "Weekly Confirmed Cases")
    
    callModule(echartBarServer, "chart5", df = by_dow_filt,
               column_name = "day_total", chart_name = "Confirmed Cases by Day of Week")
    
    # Call leaflet module and pass data objects to module
    map_sel <- callModule(leafletMapServer, "map", map_dat = confirmed_map)
    
    # Rendering odometer counts
    output$st_total_count <- renderCountup({
        countup(sum(confirmed_map$cases))
    })
    
    output$st_prior_count <- renderCountup({
        countup(sum(confirmed_map$daily_cases))
    })
    
    output$ct_total_count <- renderCountup({
        countup(sum(info_box()$cases))
    })
    
    output$ct_prior_count <- renderCountup({
        countup(sum(info_box()$daily_cases))
    })
    
    output$state_total <- renderUI({
        tagList(
            column(width = 6,
                   tags$style(".well {background-color: #0077b5;}"),
                   wellPanel(
                       h1(countupOutput("st_total_count"), style = "text-align: center; color: white; margin: 0 0 0px;"),
                       h3("NC Cases", style = "text-align: center; color: white; margin: 0 0 0px")
                       )),
            column(width = 6,
                   tags$style(".well {background-color: #0077b5;}"),
                   wellPanel(
                       h1(countupOutput("st_prior_count"), style = "text-align: center; color: white; margin: 0 0 0px"),
                       h3("NC Prior Day", style = "text-align: center; color: white; margin: 0 0 0px")
                   ))
        )
    })
    
    output$county_total <- renderUI({
        tagList(
            column(width = 6,
                   tags$style(".well {background-color: #0077b5;}"),
                   wellPanel(
                       h1(countupOutput("ct_total_count"), style = "text-align: center; color: white; margin: 0 0 0px"),
                       h3(sprintf("%s Cases", input$countySelection), style = "text-align: center; color: white; margin: 0 0 0px"),
                       
                   )),
            column(width = 6,
                   tags$style(".well {background-color: #0077b5;}"),
                   wellPanel(
                       h1(countupOutput("ct_prior_count"), style = "text-align: center; color: white; margin: 0 0 0px"),
                       h3(sprintf("%s Prior Day", input$countySelection), style = "text-align: center; color: white; margin: 0 0 0px")
                   ))
        )
    })
    
}

# Run the app
shinyApp(ui = ui, server = server)
