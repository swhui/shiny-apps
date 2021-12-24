library(stringr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(latex2exp)

fxn = function(fx, x, values){
    eval(parse(text = paste0( as.character(x), " <- c(" , paste(as.character(values), collapse = ", "), ")") ) )
    f_values = eval(parse(text = fx))
    return(f_values)
}



theme_gp <- function(){ 
    font <- "Optima" 
    
    theme_light() %+replace%   
        
        theme(
            
            #grid elements
            panel.grid.major = element_blank(),   
            panel.grid.minor = element_blank(),  
            axis.ticks = element_blank(),  
            
            
            #text elements
            plot.title = element_text(       
                family = font,          
                size = 22,          
                face = 'bold',  
                hjust = 0.5,           
                vjust = 1),           
            
            plot.subtitle = element_text(   
                family = font,      
                size = 14),        
            
            plot.caption = element_text(        
                family = font,         
                size = 9,           
                hjust = 1),       
            
            axis.title = element_text(    
                family = font,        
                size = 10),            
            
            axis.text = element_text(   
                family = font,      
                size = 9),           
            
            axis.text.x = element_text(          
                margin=margin(5, b = 10)),
            
            legend.position="top",
            legend.justification="right"
            
        )
}



# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    
    
    dashboardHeader(title = span("Plot a Function", 
                                 span(
                                     style = "color: white; font-size: 25px; font-family: 'Optima'")),titleWidth = 200),
    dashboardSidebar(width = 200, disable = TRUE),
    dashboardBody(
        useShinyjs(),
        withMathJax(),
        fluidRow(box( h4("Options"),
                      sliderInput("min_max", label = h5("Slider Range"), 
                                  min = -10000, max = 10000, value = c(0, 1)),
                      textInput(inputId = "variable", label = h5("Variable:"),
                                value = "x"),
                      textInput(inputId = "f_x", label = h5("Function:"),
                                value = "x")
        ),
             
        
        box( plotOutput("plt") )
        ) ),
    
    tags$head(tags$style(HTML("
    h4 {font-family: 'Optima'}
    h5 {font-family: 'Optima'}
    * {font-family: 'Optima'}
                              ")))
    
    
    
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plt <- renderPlot({ 
        num_values = min(20000, length(input$min_max[1]:input$min_max[2])*1000 )
        values = seq(input$min_max[1], input$min_max[2], length.out = num_values)
        y_axis_vals = fxn(input$f_x, input$variable, values)
        
        
        ggplot(data.frame()) + 
            geom_line(aes(x = values, y = y_axis_vals)) + 
            xlab(paste(as.character(input$variable))) + 
            ylab( TeX(input$f_x) ) + 
            ggtitle( TeX(paste0( "Applying the function: " ,input$f_x) ) )+
            theme_gp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
