library(shiny)
library(shinyjs)
library(shinydashboard)



gcd <- function(bigger_number , smaller_number){
    if (bigger_number == smaller_number){
        return(bigger_number)
    }
    if (bigger_number < 0 | smaller_number < 0){
        bigger_number = abs(bigger_number)
        smaller_number = abs(smaller_number)
    }
    else if (bigger_number < smaller_number){
        big = smaller_number
        smaller_number = bigger_number
        bigger_number = big
    }
    work = 'Showing work:'
    while (smaller_number %% bigger_number != 0 ){
        divisor = floor( bigger_number / smaller_number)
        remainder = bigger_number -  smaller_number * divisor
        temp = (paste0(bigger_number, ' = ', smaller_number, ' * ', divisor, ' + ', remainder))
        bigger_number = smaller_number
        smaller_number = remainder
        work = paste(work, temp, sep="<br/>")
    }
    final_result = paste0('The final result is ', bigger_number, '.')
    work = paste(work, final_result, sep="<br/>")
    return( list( work, bigger_number ) )
}




# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    
    
    dashboardHeader(title = span("Euclidean Algorithm Calculator", 
                                 span( 
                                      style = "color: white; font-size: 25px; font-family: 'Optima'")),titleWidth = 400),
    dashboardSidebar(width = 400, disable = TRUE),
    dashboardBody(
        useShinyjs(),
        withMathJax(),
        fluidRow(
                 box( h5("Using the Euclidean Algorithm, we can find the greatest 
         common denominator between any 2 integers."),
                      
                      numericInput( inputId = "num1", label = "Select the first number",
                                    value = 1, min = 1, max = 500000),
                      numericInput( inputId = "num2", label = "Select the second number",
                                    value = 2, min = 1, max = 500000)
                      
        ),
        box( textOutput("output1"),
             hr(),
             htmlOutput("output2"))
        
        
    
        ) ),
    
    tags$head(tags$style(HTML("
    h5 {font-family: 'Optima'}
    * {font-family: 'Optima'}
                              ")))
    
    
    
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$output1 <- renderText({
        gcd(input$num1, input$num2)[[2]]
        
    })
    
    output$output2 <- renderUI({
        HTML( gcd(input$num1, input$num2)[[1]] ) 
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
