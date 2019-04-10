# @title: Analysis of Logistic Map
# @author: Hayden Reece Hohns
# @date: 21/03/2019
# @brief: 


library(shiny)

# Define UI
ui <- fluidPage(
    h1("Analysis of the Logistic Map"),
    fluidRow(
        column(4, 
            sliderInput(inputId = "numIter", 
                        label = "Number of Iterations", 
                        value = 100, min = 1, max = 100),
            sliderInput(inputId = 'initCond', 
                        label = 'Initial Condition', 
                        value = 0.28, min = 0.0, max = 1.0), 
            sliderInput(inputId = "lambda", 
                        label = "Lambda",
                        value = 3.59, min = 0.0, max = 4.0, step = 0.01)
        ),
        column(8, 
               plotOutput("plot2"))
    ),
    plotOutput("plot1")
    
)

# Define server logic
server <- function(input, output) {
    
    # Define reactive values
    initData <- reactive({
        initData = array(input$initCond, c(input$numIter))
        for (t in 2:input$numIter) {
            initData[t] = input$lambda * initData[t - 1] * (1 - initData[t - 1])
        }
        return(initData)
    })
    logisticMap <- reactive({
        x = seq(0.0, 1.0, 0.01)
        logisticMap = array(0.0, c(100))
        for (i in 1:length(x)) {
            logisticMap[i] = input$lambda * x[i] * (1 - x[i])
        }
        return(logisticMap)
    })
    
    # Define plots
    output$plot1 <- renderPlot(plot(initData(), ylim = c(0, 1),
                                    type = 'l', lty = 1, 
                                    xlab = "Time", 
                                    ylab = "Orbit Value", 
                                    main = "Orbit of the Logistic Map with Initial Condition"))
    output$plot2 <- renderPlot(plot(logisticMap(), ylim = c(0, 1), 
                                    type = 'l', lty = 1, 
                                    xlab = 'x', ylab = 'T(x)', 
                                    main = "Logistic Map"))
}

# Run application 
shinyApp(ui = ui, server = server)