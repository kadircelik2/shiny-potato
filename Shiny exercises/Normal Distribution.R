library(shiny)

ui_3 <- fluidPage(
  sidebarPanel(
    h2("Generate a Random Sample from Normal Distribution", align="center"),
    sliderInput(inputId = "Size", 
                label = "Choose Sample Size", 
                value = 100, min = 10, max = 1000),
    numericInput(inputId = "MeanValue", 
                 label = "Mean", value = 0),
    numericInput(inputId = "StdValue", 
                 label = "Standard Deviation", value = 1),
    actionButton(inputId = "NewData", label = "New Data"),
    textInput(inputId = "Title", 
              label = "Title", value = "Histogram"),
    selectInput(inputId = "Color", label = "Color",
                choices = c("Blue","Gold", 
                            "Dark Green", "Purple", 
                            "Red","Orange")),
    checkboxInput(inputId = "Summary", 
                  label = "Check if you want to see summary statistics",
                  value = FALSE)
  ),
  
  mainPanel(
    plotOutput(outputId = "Hist"),
    verbatimTextOutput(outputId = "SummaryStatistics")
  )
)

server_3 <- function(input, output) {
  
  data <- eventReactive(input$NewData,{
    rnorm(input$Size, input$MeanValue, input$StdValue)
  })
  
  output$Hist <- renderPlot({
    hist(data(),
         col = input$Color,
         main = input$Title,
         xlab = "Random Sample")
  })
  
  summary_condition <- reactive({ input$Summary })
  
  output$SummaryStatistics <- renderPrint({
    if(summary_condition()) {
      summary(data())
    }
  })
  
}

shinyApp(ui = ui_3, server = server_3)