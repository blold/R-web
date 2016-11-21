library(leaflet)
library(dplyr)
library(plotly)
library(Cairo)
library(maptools)
library(scales)
library(curl) # make the jsonlite suggested dependency explicit

setwd("~/Desktop/final")

# Load data
sourceData = read.csv("tmp_table_RAQI(1996-05-01To2016-05-01)14.csv")
sourceLocation = read.csv("Station(LngLag)2.csv")
sourceDatePm25 <- read.csv("site_pm2.5_daily4.csv")
sourceDataPM10 <- read.csv("site_PM10_yearly3.csv")
sourceDataNO <- read.csv("site_NO_yearly.csv")
sourceDataOZONE <- read.csv("site_OZONE_yearly.csv")
#ins <- read.csv("instruction2.csv")
ins <- c("The dataset comes from government agency that protects and conserves the natural environment - See more at: http://www.environment.nsw.gov.au/. This project is to discover the air quality change in Sydney from 1986 to 2016, including PM10, PM2.5, AQI, Ozone, NO. User can get into different page by clicking different tab in the right slide panel. In the home tab, user should click one site marker in the map to initial the plot. After clicking,all relating plot will be presented in matching chart. At the same time, user can select one comparing site from the dropdown list in the right slide panel in home tab. In the summary tab, user can select see the average air quality index in the different plot. By dragging the date range slideinput in the right panel, user can check the data with matching date range.")


function(input, output, session) {
  
  #instruction
  output$instructions <- renderPrint({ins})

  # Tap 1(Sites Interaction)
  output$map <- renderLeaflet({
    map <- leaflet() %>% 
      addTiles() %>%
      setView(lng = 151.1802065, lat = -33.8709452, zoom = 12) %>%
      addMarkers(
        data = sourceLocation,
        popup = paste('name: ', sourceLocation$name), layerId = rownames(sourceLocation),
        clusterOptions = markerClusterOptions(), clusterId = 'cluster1' )
    map
  })

  
  # observe click event
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    print(as.numeric(click$id))
    clickID <- as.numeric(click$id)
    rowID <- clickID
    print(rowID)
    siteName <- sourceLocation[rowID, 3]
    output$firstClick_value <- renderPrint({siteName})
    
    
    # Tab1 AQI line chart
    print(siteName)
    targetColumnID <- which(names(sourceData)== siteName)
    output$linechart <- renderPlotly({
      plot_ly(sourceData, x = Date, y = sourceData[,targetColumnID], mode = "lines + markers", name = siteName) %>% 
        layout(
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 3, 
                  label = "3 mo", 
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6, 
                  label = "6 mo", 
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1, 
                  label = "1 yr", 
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1, 
                  label = "YTD", 
                  step = "year",
                  stepmode = "todate"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = "AQI Index"))
      
    })
    
    
    #Info box - AQI average
    AQI_Average <- round(mean(sourceData[,targetColumnID], na.rm=TRUE, na.action=NULL))
    output$AQIAverageInfo <- renderInfoBox({
      infoBox(
        "AQI Average", AQI_Average, icon = icon("stats", lib = "glyphicon"),
        color = "blue"
      )
    })
    
    
    
    # sitePM25ScatterChart
    startDate <- input$dateRange[1]
    endDate <- input$dateRange[2]
    subsetSourceDatePm25 <- subset(sourceDatePm25, as.Date(Date) > as.Date(startDate) & as.Date(Date) < as.Date(endDate))
    targetColumnID2 <- which(names(subsetSourceDatePm25)== siteName)
    targetYvalur <- subsetSourceDatePm25[,targetColumnID2]
    output$sitePM25ScatterChart <- renderPlotly({
      plot_ly(subsetSourceDatePm25, x = Date, y = targetYvalur,
              mode = "markers") %>% 
        layout(
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 6, 
                  label = "6 mo", 
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1, 
                  label = "1 yr", 
                  step = "year",
                  stepmode = "backward"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = "PM 2.5 (µg/m3)"))

    })
    
    #Info box - PM25AverageInfo
    pm25_Average <- round(mean(subsetSourceDatePm25[,targetColumnID2], na.rm=TRUE, na.action=NULL))
      output$PM25AverageInfo <- renderInfoBox({
        infoBox(
          "Average Pm2.5", paste0(pm25_Average, "µg/m3"), icon = icon("stats", lib = "glyphicon"),
          color = "yellow"
        )
      })
    
    # sitePM10LineChart
    targetColumnID3 <- which(names(sourceDataPM10)== siteName)
    targetYvalurPM10 <- sourceDataPM10[,targetColumnID3]
    output$sitePM10LineChart <- renderPlotly({
      plot_ly(sourceDataPM10, x = sourceDataPM10$Date,y = targetYvalurPM10, name = targetYvalurPM10) %>% 
        layout(xaxis = list(title = "Date"),yaxis = list(title = "PM 10 (µg/m3)"))
    })
    
    
    #Info box - PM10 average
    pm10_Average <- round(mean(sourceDataPM10[,targetColumnID3], na.rm=TRUE, na.action=NULL))
    output$PM10AverageInfo <- renderInfoBox({
      infoBox(
        "Average Pm10", paste0(pm10_Average, "µg/m3"), icon = icon("stats", lib = "glyphicon"),
        color = "aqua"
      )
    })
    
    #siteNOFilledChart
    targetColumnID4 <- which(names(sourceDataNO)== siteName)
    targetYvalurNO <- sourceDataNO[,targetColumnID4]
    output$siteNOFilledChart <- renderPlotly({
      
      plot_ly(sourceDataNO, x = Date,y = targetYvalurNO, name = siteName, fill = "tozeroy") %>% 
        layout(xaxis = list(title = "Date"),yaxis = list(title = "NO (pphm)"))
      
    })
    
    #Info box - NO average
    NO_Average <- round(mean(sourceDataNO[,targetColumnID4], na.rm=TRUE, na.action=NULL))
    output$NOAverageInfo <- renderInfoBox({
      infoBox(
        "Average NO", paste0(NO_Average, "pphm"), icon = icon("stats", lib = "glyphicon"),
        color = "olive"
      )
    })
    
    #siteOZONELineChart
    targetColumnID5 <- which(names(sourceDataOZONE)== siteName)
    targetYvalurOZONE <- sourceDataOZONE[,targetColumnID4]
    output$siteOZONELineChart <- renderPlotly({
      
      plot_ly(sourceDataOZONE, x = Date,y = targetYvalurOZONE, name = siteName, fill = "tozeroy") %>% 
        layout(xaxis = list(title = "Date"),yaxis = list(title = "OZNE (pphm)"))
      
    })
    
    #Info box - OZONEAverageInfo
    OZONE_Average <- round(mean(sourceDataOZONE[,targetColumnID5], na.rm=TRUE, na.action=NULL))
    output$OZONEAverageInfo <- renderInfoBox({
      infoBox(
        "Average OZONE", paste0(OZONE_Average, "pphm"), icon = icon("stats", lib = "glyphicon"),
        color = "purple"
      )
    })

    # observe add function
    observeEvent(input$add, {
      compareSiteName <- input$Compare_Site
      compareSitePM10ColumnId <- which(names(sourceDataPM10)== compareSiteName)
      compareSitePM10Data <- sourceDataPM10[,compareSitePM10ColumnId]
      
      output$sitePM10LineChart <- renderPlotly({
        plot_ly(sourceDataPM10, x = sourceDataPM10$Date,y = targetYvalurPM10, name = siteName) %>% 
          add_trace(x = sourceDataPM10$Date, y = compareSitePM10Data, name = compareSiteName) %>%
          layout(xaxis = list(title = "Date"),yaxis = list(title = "PM 10 (µg/m3)"))
      })
        
        #compareSiteAQIName <- input$Compare_Site
        compareSiteAQIColumnId <- which(names(sourceData)== compareSiteName)
        compareSiteAQIData <- sourceData[,compareSiteAQIColumnId]
        output$linechart <- renderPlotly({
          plot_ly(sourceData, x = Date, y = sourceData[,targetColumnID], mode = "lines + markers", name = siteName) %>%
            add_trace(x = Date, y = compareSiteAQIData, mode = "lines + markers", name = compareSiteName) %>%
            layout(
              xaxis = list(
                rangeselector = list(
                  buttons = list(
                    list(
                      count = 3,
                      label = "3 mo",
                      step = "month",
                      stepmode = "backward"),
                    list(
                      count = 6,
                      label = "6 mo",
                      step = "month",
                      stepmode = "backward"),
                    list(
                      count = 1,
                      label = "1 yr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 1,
                      label = "YTD",
                      step = "year",
                      stepmode = "todate"),
                    list(step = "all"))),

                rangeslider = list(type = "date")),

              yaxis = list(title = "AQI Index"))

          })
        
        #compare_sitePM25ScatterChart
        compareSitePM25ColumnId <- which(names(subsetSourceDatePm25)== compareSiteName)
        compareSitePM25Data <- subsetSourceDatePm25[,compareSitePM25ColumnId]
        output$sitePM25ScatterChart <- renderPlotly({
          plot_ly(subsetSourceDatePm25, x = Date, y = targetYvalur,mode = "markers", name = siteName) %>% 
            add_trace(x = Date ,y = compareSitePM25Data, mode = "markers", marker=list(color="olive" , size=8 , opacity=0.3), name = compareSiteName) %>% 
            layout(
              xaxis = list(
                rangeselector = list(
                  buttons = list(
                    list(
                      count = 6, 
                      label = "6 mo", 
                      step = "month",
                      stepmode = "backward"),
                    list(
                      count = 1, 
                      label = "1 yr", 
                      step = "year",
                      stepmode = "backward"),
                    list(step = "all"))),
                
                rangeslider = list(type = "date")),
              
              yaxis = list(title = "PM 2.5 (µg/m3)"))
        })
        
        
        #compare_siteNOFilledChart
        compareSiteNOColumnId <- which(names(sourceDataNO)== compareSiteName)
        compareSiteNOData <- sourceDataNO[,compareSiteNOColumnId]
        output$siteNOFilledChart <- renderPlotly({
          
          plot_ly(sourceDataNO, x = Date,y = targetYvalurNO, name = siteName, fill = "tozeroy") %>% 
            add_trace(x = Date, y = compareSiteNOData, fill = "tonexty", name = compareSiteName) %>%
            layout(xaxis = list(title = "Date"),yaxis = list(title = "NO (pphm)"))
          
        })
        
        #compare_siteOZONELineChart
        compareSiteOZONEColumnId <- which(names(sourceDataOZONE)== compareSiteName)
        compareSiteOZONEData <- sourceDataOZONE[,compareSiteOZONEColumnId]
        output$siteOZONELineChart <- renderPlotly({
          plot_ly(sourceDataOZONE, x = Date,y = targetYvalurOZONE, name = siteName, fill = "tozeroy") %>% 
            add_trace(x = Date, y = compareSiteOZONEData, fill = "tonexty", name = compareSiteName) %>%
            layout(xaxis = list(title = "Date"),yaxis = list(title = "OZNE (pphm)"))
          
        })

    })
    
    
      
   
    #observe clear compare
    observeEvent(input$clear, {
      
      # recover sitePM10LineChart
      output$sitePM10LineChart <- renderPlotly({
        plot_ly(sourceDataPM10, x = sourceDataPM10$Date,y = targetYvalurPM10, name = "targetYvalurPM10") %>% 
          layout(xaxis = list(title = "Date"),yaxis = list(title = "PM 10 (µg/m3)"))
          })
      
      # recover AQI linechart
      output$linechart <- renderPlotly({
        
        plot_ly(sourceData, x = Date, y = sourceData[,targetColumnID], mode = "lines + markers", name = paste('AQI of ', siteName)) %>% 
          layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 3, 
                    label = "3 mo", 
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 6, 
                    label = "6 mo", 
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1, 
                    label = "1 yr", 
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 1, 
                    label = "YTD", 
                    step = "year",
                    stepmode = "todate"),
                  list(step = "all"))),
              
              rangeslider = list(type = "date")),
            
            yaxis = list(title = "AQI Index"))
        
      })
      
      # recover sitePM25ScatterChart
      output$sitePM25ScatterChart <- renderPlotly({
        plot_ly(subsetSourceDatePm25, x = Date, y = targetYvalur,
                mode = "markers") %>% 
          layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 6, 
                    label = "6 mo", 
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1, 
                    label = "1 yr", 
                    step = "year",
                    stepmode = "backward"),
                  list(step = "all"))),
              
              rangeslider = list(type = "date")),
            
            yaxis = list(title = "PM 2.5 (µg/m3)"))
        })
      
      #recover siteNOFilledChart
      output$siteNOFilledChart <- renderPlotly({
        
        plot_ly(sourceDataNO, x = Date,y = targetYvalurNO, name = siteName, fill = "tozeroy") %>% 
          layout(xaxis = list(title = "Date"),yaxis = list(title = "NO (pphm)"))
        
      })
      
      #recover siteOZONELineChart
      output$siteOZONELineChart <- renderPlotly({
        
        plot_ly(sourceDataOZONE, x = Date,y = targetYvalurOZONE, name = siteName, fill = "tozeroy") %>% 
          layout(xaxis = list(title = "Date"),yaxis = list(title = "OZNE (pphm)"))
        
      })

    })
    
})
  
  
  # summary tab
  output$selected_value <- renderPrint({ input$radio })
  
  observe({

    if (input$radioSite == "All"){
      
        #Year Range Selection
        tab2_AQI_startDate <- input$tab2_AQI_year_slider[1]
        tab2_AQI_endDate <- input$tab2_AQI_year_slider[2]
        tab2_PM25_startDate <- input$tab2_PM25_year_slider[1]
        tab2_PM25_endDate <- input$tab2_PM25_year_slider[2]
        tab2_PM10_startDate <- input$tab2_PM10_year_slider[1]
        tab2_PM10_endDate <- input$tab2_PM10_year_slider[2]
        tab2_Ozone_startDate <- input$tab2_Ozone_year_slider[1]
        tab2_Ozone_endDate <- input$tab2_Ozone_year_slider[2]
        tab2_NO_startDate <- input$tab2_NO_year_slider[1]
        tab2_NO_endDate <- input$tab2_NO_year_slider[2]
        
        
        #AQI average Index
        tab2_subsetAQIDataByDate <- subset(sourceData, as.Date(Date) > as.Date(tab2_AQI_startDate) & as.Date(Date) < as.Date(tab2_AQI_endDate))
        AQI_Average_List2 <- round(mean(tab2_subsetAQIDataByDate[,7], na.rm=TRUE, na.action=NULL))
        for (j in 8:22){
          AQI_Average_Temp2 <- round(mean(tab2_subsetAQIDataByDate[,j], na.rm=TRUE, na.action=NULL))
          AQI_Average_List2 <- append(AQI_Average_List2, AQI_Average_Temp2)
        }

        #print(AQI_Average_List2)
        output$averageAQIBarChart <- renderPlotly({
          plot_ly(x = colnames(sourceData[7:22]), y = AQI_Average_List2[1:16],type = "bar")%>%
            layout(xaxis = list(title = " "),yaxis = list(title = "AQI (index)"))
        })
        
        
        #PM2.5 average
        tab_subsetPM25DataByDate <- subset(sourceDatePm25, as.Date(Date) > as.Date(tab2_PM25_startDate) & as.Date(Date) < as.Date(tab2_PM25_endDate))
        #print(tab_subsetPM25DataByDate$Date)
        PM25_Average_List2 <- round(mean(tab_subsetPM25DataByDate[,2],na.rm=TRUE, na.action=NULL), 2)
        print(PM25_Average_List2)
        for (j in 3:17){
          PM25_Average_Temp2 <- round(mean(tab_subsetPM25DataByDate[,j],na.rm=TRUE, na.action=NULL), 2)
          PM25_Average_List2 <- append(PM25_Average_List2, PM25_Average_Temp2)
        }
        
        print(PM25_Average_List2)
        output$averagePM25BarChart <- renderPlotly({
          plot_ly(x = colnames(sourceDatePm25[2:17]), y = PM25_Average_List2[1:16],type = "bar")%>%
            layout(xaxis = list(title = " "),yaxis = list(title = "PM 2.5 (µg/m3)"))
        })
        
        
        #PM10 average
        tab_subsetPM10DataByDate <- subset(sourceDataPM10, as.numeric(Date) > as.numeric(tab2_PM10_startDate) & as.numeric(Date) < as.numeric(tab2_PM10_endDate))
        PM10_Average_List <- round(mean(tab_subsetPM10DataByDate[,2],na.rm=TRUE, na.action=NULL), 1)
        for (j in 3:17){
          PM10_Average_Temp <- round(mean(tab_subsetPM10DataByDate[,j],na.rm=TRUE, na.action=NULL), 1)
          PM10_Average_List <- append(PM10_Average_List, PM10_Average_Temp)
        }
        
        output$averagePM10BarChart <- renderPlotly({
          plot_ly(x = colnames(sourceDataPM10[2:17]), y = PM10_Average_List[1:16],type = "bar")%>%
            layout(xaxis = list(title = " "),yaxis = list(title = "PM 10 (µg/m3)"))
        })
        
        
        #Ozone average
        tab_subsetOzoneDataByDate <- subset(sourceDataOZONE, as.numeric(Date) > as.numeric(tab2_Ozone_startDate) & as.numeric(Date) < as.numeric(tab2_Ozone_endDate))
        Ozone_Average_List <- round(mean(tab_subsetOzoneDataByDate[,2],na.rm=TRUE, na.action=NULL), 1)
        for (j in 3:17){
          Ozone_Average_Temp <- round(mean(tab_subsetOzoneDataByDate[,j],na.rm=TRUE, na.action=NULL), 1)
          Ozone_Average_List <- append(Ozone_Average_List, Ozone_Average_Temp)
        }
        
        output$averageOzoneBarChart <- renderPlotly({
          plot_ly(x = colnames(sourceDataOZONE[2:17]), y = Ozone_Average_List[1:16],type = "bar")%>%
            layout(xaxis = list(title = " "),yaxis = list(title = "Ozone (pphm)"))
        })
        
        #NO average
        tab_subsetNODataByDate <- subset(sourceDataNO, as.numeric(Date) > as.numeric(tab2_NO_startDate) & as.numeric(Date) < as.numeric(tab2_NO_endDate))
        NO_Average_List <- round(mean(tab_subsetNODataByDate[,2],na.rm=TRUE, na.action=NULL), 1)
        for (j in 3:17){
          NO_Average_Temp <- round(mean(tab_subsetNODataByDate[,j],na.rm=TRUE, na.action=NULL), 1)
          NO_Average_List <- append(NO_Average_List, NO_Average_Temp)
        }
        
        output$averageNOBarChart <- renderPlotly({
          plot_ly(x = colnames(sourceDataNO[2:17]), y = NO_Average_List[1:16],type = "bar")%>%
            layout(xaxis = list(title = " "),yaxis = list(title = "NO (pphm)"))
        })
        
        
    }


  })
  
  # output the bar chart with average surburb
  output$bar <- renderPlotly({
    
    averageCentral <- aggregate(list(average = sourceData$Sydney.central.east.RAQI), by=list(group = sourceData$group), FUN=mean, na.rm=TRUE, na.action=NULL)
    averageNorthWest <- aggregate(list(average = sourceData$Sydney.north.west.RAQI), by=list(group = sourceData$group), FUN=mean, na.rm=TRUE, na.action=NULL)
    averageSouthWest <- aggregate(list(average = sourceData$Sydney.south.west.RAQI), by=list(group = sourceData$group), FUN=mean, na.rm=TRUE, na.action=NULL)
    
    plot_ly(averageCentral, x = "1986 ~ 1996", y = averageCentral[1,2], type = "bar",mode = "markers",name = "Central North", marker=list(color="purple"), showlegend = TRUE) %>%
      add_trace(x = "1986 ~ 1996", y = 0,type = "bar",mode = "markers", name = "North West",marker=list(color="blue"), showlegend = TRUE) %>%
      add_trace(x = "1986 ~ 1996", y = averageSouthWest[1,2],type = "bar",mode = "markers", name = "South West",marker=list(color="red"), showlegend = TRUE) %>%
      add_trace(x = "1997 ~ 2016", y = averageCentral[2,2],type = "bar",mode = "markers",name = "Central North", marker=list(color="purple"), showlegend = FALSE) %>%
      add_trace(x = "1997 ~ 2016", y = averageNorthWest[2,2],type = "bar",mode = "markers", name = "North West",marker=list(color="blue"), showlegend = FALSE) %>%
      add_trace(x = "1997 ~ 2016", y = averageSouthWest[2,2],type = "bar",mode = "markers", name = "South West",marker=list(color="red"), showlegend = FALSE) %>%
      layout(xaxis = list(title = "Year Range"),yaxis = list(title = "AQI (index)"))
  })
  
  
  # output the scatter chart with surburb
  output$scatterChart <- renderPlotly({
    dataSlide <- subset(sourceData, as.Date(Date) > as.Date(input$yearSlide[1]) & as.Date(Date) < as.Date(input$yearSlide[2]))
    plot_ly(data = dataSlide, x = dataSlide$Date, y = Sydney.central.east.RAQI, mode = "markers", name = "Sydney.central.east", opacity =0.2, showlegend = TRUE ) %>%
      add_trace(x = dataSlide$Date, y = sourceData$Sydney.north.west.RAQI, mode = "markers",name = "Sydney.north.west", marker=list(color="purple" , size=8 , opacity=0.5), showlegend = TRUE) %>%
      add_trace(x = dataSlide$Date, y = sourceData$Sydney.south.west.RAQI, mode = "markers",name = "Sydney.south.west.RAQI", marker=list(color="red" , size=6 , opacity=0.3), showlegend = TRUE) %>% 
      layout(xaxis = list(title = "Date"),yaxis = list(title = "AQI (index)"))
  })


}