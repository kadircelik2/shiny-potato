library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(gt)
library(tidyverse)
library(glue)



ui <- dashboardPage(
    dashboardHeader(title = "Counter Machine"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    menuItem("Blackjack", tabName = "bj",
                        menuSubItem("MySpecial", tabName = "bomba"),
                        menuSubItem("Hi-Lo", tabName = "hilo"), startExpanded = T)
                    )),
    dashboardBody(shinyDashboardThemes(
        theme = "grey_dark"),
        conditionalPanel(condition = "input.sidebarmenu == 'bomba'",
                         
                         actionButton(inputId = "ka", "A"),
                         actionButton(inputId = "k2", "2"),
                         actionButton(inputId = "k3", "3"),
                         actionButton(inputId = "k4", "4"),
                         actionButton(inputId = "k5", "5"),
                         actionButton(inputId = "k6", "6"),
                         actionButton(inputId = "k7", "7"),
                         actionButton(inputId = "k8", "8"),
                         actionButton(inputId = "k9", "9"),
                         actionButton(inputId = "k10+", "10-K"),
                         actionButton(inputId = "reset", "Reset"),
                         
                        # verbatimTextOutput(outputId = "outputs"),
                         
                         tableOutput(outputId = "table1"),
                         tableOutput(outputId = "table2"),
                         tableOutput(outputId = "table3"),
                        
                        img(src = "new.PNG")
                        
        )
    )
)

server <- function(input, output) {
    
    counter <- reactiveValues(A =32,
                              "2" = 32,
                              "3" = 32,
                              "4" = 32,
                              "5" = 32,
                              "6" = 32,
                              "7" = 32,
                              "8" = 32,
                              "9" = 32,
                              "10-K" = 128,
                              "toplam" = 416
                              )
    
    observeEvent(input$reset, {
        counter$A =32
        counter$"2" = 32
        counter$"3" = 32
        counter$"4" = 32
        counter$"5" = 32
        counter$"6" = 32
        counter$"7" = 32
        counter$"8" = 32
        counter$"9" = 32
        counter$"10-K" = 128
        counter$"toplam" = 416
        
    })
    
    observeEvent(input$ka, {
        counter$A <- counter$A - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k2, {
        counter$"2" <- counter$"2" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k3, {
        counter$"3" <- counter$"3" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k4, {
        counter$"4" <- counter$"4" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k5, {
        counter$"5" <- counter$"5" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k6, {
        counter$"6" <- counter$"6" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k7, {
        counter$"7" <- counter$"7" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k8, {
        counter$"8" <- counter$"8" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$k9, {
        counter$"9" <- counter$"9" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    observeEvent(input$"k10+", {
        counter$"10-K" <- counter$"10-K" - 1
        counter$"toplam" <- counter$"toplam" - 1
    })
    
    
    
  
    output$table1 <- renderTable({
      
      
      setNames(data.frame(counter$"toplam", 416-counter$"toplam"), c("Kalan Kart", "Cikan Kart"))
      
      
      
    })
  
    output$table2 <- renderTable({
        
      
        setNames(data.frame(((counter$"2"+counter$A)/counter$"toplam")*100, ((counter$"3"+counter$"2"+counter$A)/counter$"toplam")*100,
                            ((counter$"4"+counter$"3"+counter$"2"+counter$A)/counter$"toplam")*100,
                            ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$A)/counter$"toplam")*100,
                            ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$A)/counter$"toplam")*100,
                            ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$A)/counter$"toplam")*100,
                            ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$A)/counter$"toplam")*100,
                            ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$A)/counter$"toplam")*100,
                            ((counter$"10-K"+counter$A)/counter$"toplam")*100), 
                 c("A-2", "A-3", "A-4", "A-5", "A-6", "A-7", "A-8", "A-9", "10-J-Q-K-A"))
      
        
        
    })
    
    
    output$table3 <- renderTable({
      
      
      setNames(data.frame((counter$A/counter$"toplam")*100, (counter$"2"/counter$"toplam")*100, (counter$"3"/counter$"toplam")*100, (counter$"4"/counter$"toplam")*100, (counter$"5"/counter$"toplam")*100,
                          (counter$"6"/counter$"toplam")*100, (counter$"7"/counter$"toplam")*100, (counter$"8"/counter$"toplam")*100, (counter$"9"/counter$"toplam")*100, (counter$"10-K"/counter$"toplam")*100), 
               c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10-K"))
      
      
      
    })
    
    output$outputs <- renderText({
        sprintf("2    2-3   2-4   2-5   2-6   2-7   2-8   2-9   2-K     |    Kalan kart sayisi
                \n%.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%                 %.0f
                \n2-K   3-K   4-K   5-K   6-K   7-K   8-K   9-K  10-K    |    Cikan kart sayisi
                \n%.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%                 %.0f
                \nA    2    3    4    5    6    7    8    9    10-K
                \n%.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%   %.0f%%    %.0f%%",
                (counter$"2"/counter$"toplam")*100, ((counter$"3"+counter$"2")/counter$"toplam")*100, ((counter$"4"+counter$"3"+counter$"2")/counter$"toplam")*100,
                ((counter$"4"+counter$"3"+counter$"2"+counter$"5")/counter$"toplam")*100, ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6")/counter$"toplam")*100, ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7")/counter$"toplam")*100, ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$"8")/counter$"toplam")*100,
                ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9")/counter$"toplam")*100, ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100,
                counter$"toplam",
                ((counter$"4"+counter$"3"+counter$"2"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100, ((counter$"4"+counter$"3"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100,
                ((counter$"4"+counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100, ((counter$"5"+counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100,
                ((counter$"6"+counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100, ((counter$"7"+counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100, ((counter$"8"+counter$"9"+counter$"10-K")/counter$"toplam")*100, ((counter$"9"+counter$"10-K")/counter$"toplam")*100,
                (counter$"10-K"/counter$"toplam")*100, 416-counter$"toplam",
                (counter$A/counter$"toplam")*100, (counter$"2"/counter$"toplam")*100, (counter$"3"/counter$"toplam")*100, (counter$"4"/counter$"toplam")*100, (counter$"5"/counter$"toplam")*100,
                (counter$"6"/counter$"toplam")*100, (counter$"7"/counter$"toplam")*100, (counter$"8"/counter$"toplam")*100, (counter$"9"/counter$"toplam")*100, (counter$"10-K"/counter$"toplam")*100)
        
        
                
    })
    
    
    
    
    
    
    
}


shinyApp(ui = ui, server = server)
