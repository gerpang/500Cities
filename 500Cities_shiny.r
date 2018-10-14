#load packages
library(shiny)
library(leaflet)
library(RColorBrewer) #creates nice looking color palettes especially for thematic maps 
library(DT)
library(rgdal)
library(gpclib)
library(maptools)
library(R6)
library(raster)
library(broom)
library(scales)
library(reshape2)
library(tidyverse)
library(data.table)
library(highcharter)

Data <- fread('https://chronicdata.cdc.gov/api/views/6vp6-wxuq/rows.csv?accessType=DOWNLOAD')
#fread from data.table has faster read speed than read.csv

#launch shinyApp(ui,server)
shinyApp(options=list(height=800), #set height of app within rmd 

#Define user interface element
  ui = fluidPage( 
    fluidRow(
      column(5,
             #user input 
             selectInput(inputId = 'categoryId', 
                         label = 'Select Category',
                         choices = unique(Data$CategoryID)
                         ),
             uiOutput('measures')
             ),
      column(3, 
             uiOutput('slider'),
             selectInput(inputId = 'age',
                         label = 'Type',
                         choices=unique(Data$DataValueTypeID)))
    ),
    fluidRow(column(8,leafletOutput('mymap')),
             column(4,dataTableOutput('table')))
  ),
#Define functionality   
  server = function(input,output,session){
    #use reactive functions for interactive input 
    df1 <- reactive({
      df <- Data
      #select variables of interest 
      df <- subset(df, select = c('CityName','StateAbbr', 'GeoLocation', 'Year',
                                  'Measure', 'Data_Value', 'PopulationCount', 'GeographicLevel',
                                  'Short_Question_Text', 'CategoryID', 'DataValueTypeID'))
      #remove missing
      df <- df[!is.na(df$Data_Value),]

      #subset data by the user selected input
      df <- subset(df, DataValueTypeID == input$age)
      df <- subset(df, CategoryID == input$categoryId)
      })
    
    #generate UI elements dynamically: shows the measures the user selects
    #(placed within server because it relies on user input)
    output$measures <- renderUI({
      selectInput(inputId = 'measures',
                  label = 'Select Measure',
                  choices= unique(df1()$Measure))
    })
    
    #filter data again based on user selections
    ##first subset was to reduce categories for user selection
    df2 <- reactive({
      x <- df1()
      x <- subset(x, Measure == as.character(input$measures))
    })
    
    #slider to filter map markers
    output$slider <- renderUI({
      sliderInput(inputId = 'slider',
                  label = 'Filter Map', 
                  min = min(df2()$Data_Value),
                  max = max(df2()$Data_Value),
                  value = c(min(df2()$Data_Value),max(df2()$Data_Value)))
    })
    
    #preparing data for building map 
    output$mymap <- renderLeaflet({
      df <- df2()
      #filter based on slider input 
      df <- subset(df, Data_Value > input$slider[1] & Data_Value < input$slider[2])
      
      #Apply colors to values from data set
      Colors <- brewer.pal(11,"RdBu")
      binpal <- colorBin(Colors,df$Data_Value, 6, pretty=FALSE)
      
      #separate geolocation column in latitude and longitude (for leaflet)
      lat = vector()
      long = vector()
      for (i in 1:nrow(df)){
        x <- unlist(strsplit(df$GeoLocation[i],",")) #splits strings by ,
        lat[i] <- substr(x[1],2,8) #selects 2nd to 8th character of 1st str
        long[i] <- substr(x[2],2,9) #selects 2nd to 9th character of 2nd str
      }
      
      df$lat = as.numeric(lat)
      df$long = as.numeric(long)
      
      #Build Map
      leaflet() %>%
        #Add state borders
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        
        #Add markers for each location 
        addCircleMarkers(lat = df$lat,
                         lng = df$long,
                         data = df,
                         label = paste(df$CityName,df$StateAbbr),
                         color = ~binpal(Data_Value),
                         radius = 10,
                         fillColor = ~binpal(Data_Value),
                         fill = TRUE,
                         opacity = 1
                         ) %>%
        addLegend(position = 'bottomleft', pal= binpal, values = df$Data_Value)
    })
    
    #create data table to show results in tabular format 
    output$table <- renderDataTable({
      df <- subset(df2(),select = c(CityName,StateAbbr, Data_Value))
      df <- df[order(df$Data_Value, decreasing = TRUE)]
      df <- setNames(df, c('City','State','Value'))
      datatable(df,options = list(pageLength=10))
    })
  }
)

