library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)

ispark <- read.csv("D:/ispark.csv")
istasyon <- read.csv("D:/istasyon.csv")
ispark$CountyName <- as.factor(ispark$CountyName)
istasyon$County_Name <- as.factor(istasyon$County_Name)
istasyon$FUEL_DISTRIBUTION_COMPANY_DESC <- as.factor(istasyon$FUEL_DISTRIBUTION_COMPANY_DESC)


ui_1 <- fluidPage(
  sidebarPanel(width = 4,
               h2("Istanbul Shiny App", align="center"),
               
               h6("Created by Kadir CELIK", align = "center"),
               
               radioButtons(inputId = "veri", strong("Choose Data"), 
                            choices = c("ispark","istasyon")),
               
               selectInput(inputId = "ilce", label = "County",
                           choices = unique(istasyon$County_Name),
                           selected = "Sisli"),
               
               sliderInput(inputId = "buyukluk", 
                           label = "Minimum Parking Capacity", 
                           value = 50, min = 0, max = 500),
               
               checkboxInput(inputId = "acik_parklar", 
                             label = "Show only open parks",
                             value = FALSE),
               
               checkboxGroupInput("istasyonlar", 
                                  strong("Choose a gas station brand"),
                                  choices = unique(istasyon$FUEL_DISTRIBUTION_COMPANY_DESC),
                                  selected = "Opet")),
  
  
  mainPanel(width = 8,
            leafletOutput(outputId = "map", height = 800)
            
  )
)

server_1 <- function(input, output){
  
  ispark_map <- reactive({
    ispark %>% dplyr::filter(CountyName == input$ilce, CAPACITY >= input$buyukluk) %>% leaflet() %>% 
      addProviderTiles(providers$Stadia.StamenTonerLite) %>%
      addMarkers(~LONGITUDE, ~LATITUDE, label = ~ParkType ,popup = paste0(
        "<b>Capacity: </b>",
        ispark$CAPACITY)) %>%
      setView(lng = median(ispark$LONGITUDE, na.rm = TRUE),
              lat = median(ispark$LATITUDE, na.rm = TRUE), zoom = 12)
  })
  
  
  istasyon_map <- reactive({
    istasyon %>% dplyr::filter(County_Name == input$ilce, FUEL_DISTRIBUTION_COMPANY_DESC==input$istasyonlar) %>% leaflet() %>% 
      addProviderTiles(providers$Stadia.StamenTonerLite) %>%
      addMarkers(~LONGITUDE, ~LATITUDE, label = ~BUSINESS_TYPE_DESC, popup = paste0(
        "<b>Station Brand: </b>",
        istasyon$FUEL_DISTRIBUTION_COMPANY_DESC)) %>%
      setView(lng = median(ispark$LONGITUDE, na.rm = TRUE),
              lat = median(ispark$LATITUDE, na.rm = TRUE), zoom = 12)
  })
  
  
  
  
  
  observeEvent(input$veri, {
    if (input$veri == "ispark") {
      output$map <- renderLeaflet(ispark_map())
    } else if (input$veri == "istasyon") {
      output$map <- renderLeaflet(istasyon_map())
    }
  })
  
  
  
  
}

shinyApp(ui = ui_1, server = server_1)