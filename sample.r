#Load packages
library(shiny)
library(leaflet)
library(RColorBrewer)
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

#Read in dataset. You can read in the dataset directly from the CDC website. I choose not to in this case because it is large and takes a long time to download.

#Data <- fread('500_Cities__Local_Data_for_Better_Health.csv')
Data <- fread('https://chronicdata.cdc.gov/api/views/6vp6-wxuq/rows.csv?accessType=DOWNLOAD')
#Each Shiny application consists of ui and server elements. The syntax to launch an application is shinyApp(ui, server)
shinyApp(options = list(height = 800), #height of the application within the Rmarkdown document
         
         #Define the user interface element
         ui = fluidPage(
           fluidRow(
             column(5
                    
                    #Create element to allow user input. The values from this input are accessed in the server function via input$categoryId
                    , selectInput('categoryId', 'Select Category'
                                  , choices = unique(Data$CategoryID)
                    )
                    #uiOutput allows you to render ui elements within the server function. This offers you more flexibility in defining the user-interface.
                    , uiOutput('measures'))
             
             , column(3, uiOutput('slider')
                      , selectInput('age', 'Type', choices = unique(Data$DataValueTypeID))
             )
           )
           ,fluidRow(column(8, leafletOutput('mymap'))
                     , column(4, dataTableOutput('table'))
           )
         )
         
         #Define functionality
         ,server = function(input, output, session){
           
           
           #Read the data into a reactive function. If data takes user input (reactive values), then it must be contained within a reactive function. I use this reactive function to filter the data set. A reactive function always returns the final line of code.
           df1 <- reactive ({
             df <- Data
             df <- subset(df, select = c('CityName','StateAbbr', 'GeoLocation', 'Year'
                                         , 'Measure', 'Data_Value', 'PopulationCount', 'GeographicLevel'
                                         , 'Short_Question_Text', 'CategoryID', 'DataValueTypeID'))
             
             #Removes NA values
             df <- df[!is.na(df$Data_Value),]
             df <- subset(df, DataValueTypeID == input$age)
             df <- subset(df, CategoryID == input$categoryId)
             #df <- subset(df, GeographicLevel == as.character(input$geoLevel))
           })
           
           #Here I user renderUI to dynamically generate ui elements. I use here because I only want to show the measures within the category the user selects. Because this takes user input, the ui must be generated within the server function.
           output$measures <- renderUI({
             selectInput('measures', 'Select Measure', choices = unique(df1()$Measure))
           })
           
           #Here I'm filtering the data again based on the measure chosen by the user. I couldn't do this in df1 because I first filtered the data based on category to reduce the options within the measures selectInput.
           df2 <- reactive ({
             x <- df1()
             x <- subset(x, Measure == as.character(input$measures))
           })
           
           #Create a slider to filter the map markers. 
           output$slider <- renderUI ({
             sliderInput('slider', 'Filter Map', min = min(df2()$Data_Value) 
                         , max = max(df2()$Data_Value)
                         , value = c(min(df2()$Data_Value), max(df2()$Data_Value)))
           })
           
           #Build the leaflet map
           output$mymap <- renderLeaflet({
             df <- df2()
             
             #Filter the data set based on values from the slider input
             df <- subset(df, Data_Value > input$slider[1] & Data_Value < input$slider[2])
             
             #Define color pallete
             Colors <- brewer.pal(11,"RdBu")
             
             #Apply pallete to values from data set
             binpal <- colorBin(Colors, df$Data_Value, 6, pretty = FALSE)
             
             #Separate GeoLocation column into latitude and longitude columns. Required for leaflet
             lat = vector()
             lng = vector()
             for (i in 1: nrow(df)){
               x<- unlist(strsplit(df$GeoLocation[i], ",")) #Splits the string at the comma
               lat[i] <- substr(x[1],2,8) #Selects characters 2 thru 8 of the string for latitude
               lng[i] <- substr(x[2],2,9) #Selects characters 2 thru 9 of the string for longitude
               
             }
             #Convert to numeric
             df$lat <- as.numeric(lat)
             df$lng <- as.numeric(lng)
             
             #Build leaflet map
             leaflet() %>%
               
               #Adds state borders to the map
               addTiles(
                 urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
               ) %>%
               
               #Add the markers for each location
               addCircleMarkers(lat = df$lat
                                , lng = df$lng
                                , data = df
                                , label = paste(df$CityName, df$StateAbbr)
                                , color = ~binpal(Data_Value)
                                , radius = 10
                                , fillColor = ~binpal(Data_Value)
                                , fill = TRUE
                                , opacity = 1
                                
               ) %>%
               addLegend(position = 'bottomleft', pal = binpal, values = df$Data_Value
               )
             
           })
           
           #Create data table to show values in tabular format
           output$table <- renderDataTable ({
             df <- subset(df2(), select = c(CityName, StateAbbr, Data_Value))
             df <- df[order(df$Data_Value, decreasing = TRUE),]
             df <- setNames(df, c('City', 'State', 'Value'))
             datatable(df, options = list(pageLength = 10))
             
           })
           
         }
)