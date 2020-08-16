# Load primary packages
library(shiny)

# Source necessary modules
source("modules/getData.R")
source("modules/echartsModule.R")
source("modules/echartsBarModule.R")
source("modules/leafletModule.R")

# Define UI
ui <- navbarPage(
    title = "NC COVID Dashboard",
    collapsible = TRUE,
    tabPanel("Data Visualization",
             fluidPage(
                 tags$link(href="https://fonts.googleapis.com/css2?family=Questrial&display=swap", rel="stylesheet"),
                 tags$style(HTML("* {font-size: 100%; font-family: Questrial, sans-serif;}")),
                 fluidRow(selectInput("countySelection",
                                      label = "Please select a county:",
                                      choices = unique(combined$Admin2),
                                      selected = "Gaston",
                                      selectize = TRUE)),
                 fluidRow(tabsetPanel(type = "pill",
                                      tabPanel("Daily Confirmed Cases",
                                               echartUI("chart1")),
                                      tabPanel("Weekly Confirmed Cases",
                                               echartUI("chart4")),
                                      tabPanel("Cumulative Confirmed Cases",
                                               echartUI("chart3")),
                                      tabPanel("Daily Deaths",
                                               echartUI("chart2")),
                                      tabPanel("Confirmed Cases by Day of Week",
                                               echartUI("chart5")),
                                      tabPanel("Map of Total Cases",
                                               leafletMapUI("map"))
                                      ))
                 )
             )
    )

# Define server
server <- function(input, output) {
    
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
    
    # Call leaflet module and pass both reactive data objects to module
    map_sel <- callModule(leafletMapServer, "map", map_dat = confirmed_map)
    
}

# Run the app
shinyApp(ui = ui, server = server)
