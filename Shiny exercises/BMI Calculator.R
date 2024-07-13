library(shiny)

ui_2 <- fluidPage(
    
    titlePanel("BMI in shiny"),
    
    sidebarLayout(
        
        sidebarPanel(
            textInput(inputId = "Name", label = "Your Name"),
            textInput(inputId = "Surname", label = "Your Surname"),
            textInput(inputId = "Age", label = "Your age"),
            numericInput(inputId = "Height", 
                         label = "Please enter your height in cm",
                         value = 180),
            numericInput(inputId = "Weight",
                         label = "Please enter your weight in kg",
                         value = 70),
            actionButton(inputId = "Calculate", label = "Calculate")
        ),
        
        mainPanel(
            p(h3("Personel Information")),
            textOutput("NameSurname_out"),
            textOutput("Age"),
            br(),
            p(h3("BMI calculation")),
            textOutput("bmi_out"), 
        )
    )
)

server_2 <- function(input, output) {
    
    output$NameSurname_out <- renderText({
        input$Calculate
        isolate(paste("Name Surname:", input$Name, input$Surname))
    })
    
    output$Age <- renderText({
        input$Calculate
        isolate(paste("Age:", input$Age))
    })
    
    bmi <- reactive({
        input$Weight / (input$Height^2)*10000
    })
    
    output$bmi_out <- renderText({
        input$Calculate
        isolate(paste("BMI:", bmi()))
    })
}

shinyApp(ui_2, server_2)