library(shinydashboard)
library(leaflet)
library(scales)
library(plotly)

setwd("~/Desktop/final")

sourceData = read.csv("tmp_table_RAQI(1996-05-01To2016-05-01)14.csv")
sourceDatePm25 <- read.csv("site_pm2.5_daily4.csv")
sourceDataPM10 <- read.csv("site_PM10_yearly3.csv")
sourceDataOZONE <- read.csv("site_OZONE_yearly.csv")
sourceDataNO <- read.csv("site_NO_yearly.csv")

header <- dashboardHeader(
  title = "Air Quality of Sydney"
)

sites_name <- colnames(sourceData)[7:22]
all_names <- colnames(sourceData)[4:22]
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",

    menuItem("Instruction", tabName = "Instruction", icon = icon("exclamation-sign", lib = "glyphicon")),
              
    menuItem("Home", tabName = "Home", icon = icon("dashboard")),
    
    menuItem("Summary", icon = icon("th"), tabName = "Summary", badgeColor = "green"),
    
    
    
    # conditional panel for home tab
    conditionalPanel("input.sidebarmenu === 'Home'",
                              selectInput("Compare_Site", label = h4("Select site"),
                                          choices = sites_name,
                                          selected = NULL),
                              hr(),
                              # fluidRow( verbatimTextOutput("firstClick_value")),
                     
                     
                            actionButton("add", label = "Add"), 
                            actionButton("clear", label = "Clear")
                     
                    
    ),
    
    #conditional panel for summary tab
    conditionalPanel("input.sidebarmenu === 'Summary'",
                     
                     
                     radioButtons(
                       inputId="radioSite",
                       label="Site Selection Type:",
                       choices=list(
                         "All"
                         # "Manual Select"
                       ),
                       selected="All"),
                     
                     # conditionalPanel(
                     #   condition = "input.radioSite != 'All'",
                     #   checkboxGroupInput("checkGroup", label = h4("Compare Sites"),
                     #                      choices = sites_name,
                     #                      selected = sites_name[2]),
                     #   fluidRow(verbatimTextOutput("sites_value"))
                     # ),
                     
                     hr(),
                     sliderInput("tab2_AQI_year_slider", "AQI Date Range", min(as.Date(sourceData$Date[1])), max(as.Date(sourceData$Date[nrow(sourceData)])),
                                 value = range(as.Date(sourceData$Date[1]), step = 1)
                     ),
                     
                     hr(),
                     sliderInput("tab2_PM25_year_slider", "PM2.5 Date Range", min(as.Date(sourceDatePm25$Date[1])), max(as.Date(sourceDatePm25$Date[nrow(sourceDatePm25)])),
                                 value = range(as.Date(sourceDatePm25$Date[5]), step = 1)
                     ),
                     
                     hr(),
                     sliderInput("tab2_PM10_year_slider", label = "PM10 Year Range", min = as.numeric(sourceDataPM10$Date[1]), max = as.numeric(sourceDataPM10$Date[nrow(sourceDataPM10)]),
                                 value = c(as.numeric(sourceDataPM10$Date[2]), as.numeric(sourceDataPM10$Date[5]))
                     ),
                     
                     hr(),
                     sliderInput("tab2_Ozone_year_slider", label = "Ozone Year Range", min = as.numeric(sourceDataOZONE$Date[1]), max = as.numeric(sourceDataOZONE$Date[nrow(sourceDataOZONE)]),
                                 value = c(as.numeric(sourceDataPM10$Date[2]), as.numeric(sourceDataPM10$Date[4]))
                     ),
                     
                     hr(),
                     sliderInput("tab2_NO_year_slider", label = "NO Year Range", min = as.numeric(sourceDataNO$Date[1]), max = as.numeric(sourceDataNO$Date[nrow(sourceDataNO)]),
                                 value = c(as.numeric(sourceDataNO$Date[2]), as.numeric(sourceDataNO$Date[3]))
                     )
                     
                     
    )
  )
)


body <- dashboardBody(
  
  # instruction tab content
  tabItems(tabItem(tabName = "Instruction", 
          fluidRow(
              box(width = NULL, title = "Instruction", status = "warning", solidHeader = TRUE,
              verbatimTextOutput("instructions")
              )
          )
            
    ),
    
    # Home tab content
    tabItem(tabName = "Home",
            
        fluidRow(
    
          fluidRow(
            # A static infoBox
            #infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
      
            infoBoxOutput("AQIAverageInfo", width = 3),
    
            infoBoxOutput("PM25AverageInfo", width = 2),
      
            infoBoxOutput("PM10AverageInfo", width = 2),
      
            infoBoxOutput("NOAverageInfo", width = 2),
      
            infoBoxOutput("OZONEAverageInfo", width = 3)
      
          )
    
        ),
  fluidRow(
    column(width = 5,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 370)
           )
    ),
    box(width = 7, title = "AQI of the Site", status = "primary", solidHeader = TRUE,
        plotlyOutput("linechart")

        )
  ),
  hr(),
  fluidRow(
    box(width = 7, title = "PM2.5 of the Site", status = "primary", solidHeader = TRUE,
        plotlyOutput("sitePM25ScatterChart"),
        dateRangeInput("dateRange", label = h3("Date range"),start = as.Date("1990-01-01"), end = as.Date("2016-01-01"))
        # verbatimTextOutput("dateRangeText")
        ),
    
    box(width = 5, title = "PM10 of the Site", status = "primary", solidHeader = TRUE,
        plotlyOutput("sitePM10LineChart")
    )
    
  ),
  
  hr(),
  fluidRow(
    box(width = 6, title = "NO of the Site", status = "primary", solidHeader = TRUE,
        plotlyOutput("siteNOFilledChart")

    ),
    
    box(width = 6, title = "OZONE of the Site", status = "primary", solidHeader = TRUE,
        plotlyOutput("siteOZONELineChart")
    )
    
  )

  
    ),
  
  
  
  # Summary tab content
  tabItem(tabName = "Summary",
          h2("Summary"),
          fluidRow( verbatimTextOutput("averageAQIList")),
          
          fluidRow(
            box(width = 6, title = "Average AQI", status = "primary", solidHeader = TRUE,
                plotlyOutput("averageAQIBarChart")
                ),
            box(width = 6, title = "Average PM2.5", status = "primary", solidHeader = TRUE,
                plotlyOutput("averagePM25BarChart")
                )
            
            ),
          
          fluidRow(
            box(width = 4, title = "Average PM10", status = "primary", solidHeader = TRUE,
                plotlyOutput("averagePM10BarChart")
            ),
            box(width = 4, title = "Average Ozone", status = "primary", solidHeader = TRUE,
                plotlyOutput("averageOzoneBarChart")
            ),
            box(width = 4, title = "Average NO", status = "primary", solidHeader = TRUE,
                plotlyOutput("averageNOBarChart")
            )
            
          ),
          
          
          fluidRow(
            box(width = 6, title = "AQI of Sydney Surburb in 20 years", status = "primary", solidHeader = TRUE,
                plotlyOutput("scatterChart"),
                sliderInput("yearSlide", "Year Range", min(as.Date(sourceData$Date[1])), max(as.Date(sourceData$Date[10961])),
                            value = range(as.Date(sourceData$Date[5]), step = 1)
                )
                
            ),
            box(width = 6,title = "Average AQI of Sydney Surburb in 20 years", status = "primary", solidHeader = TRUE,
                   plotlyOutput("bar")
                  )
            
               )
          
          
     )
  )
  
)



dashboardPage(
  header,
  sidebar,
  body
)