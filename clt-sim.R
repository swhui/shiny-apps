library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(latex2exp)
library(Pareto)
library(ExtDist)


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


ui <- dashboardPage(
    
    
    
    dashboardHeader(title = span("The Magic of CLT", 
                                 span("dashboard", 
                                      style = "color: white; font-size: 25px; font-family: 'Optima'")),titleWidth = 200),
    dashboardSidebar(width = 200, disable = TRUE),
    dashboardBody(
        useShinyjs(),
        withMathJax(),
        fluidRow(box( h4("Options"),
             selectInput( inputId = "distr", label = "Distribution", 
                          choices = list("Normal" = "Normal", 
                                         "Exponential" = "Exponential", 
                                         "Gamma" = "Gamma",
                                         "Beta" = "Beta",
                                         "Pareto" = "Pareto",
                                         "Cauchy" = "Cauchy",
                                         "Laplace" = "Laplace",
                                         "Continuous Uniform" = "Continuous Uniform",
                                         "Binomial" = "Binomial",
                                         "Poisson" = "Poisson",
                                         "Geometric" = "Geometric",
                                         "Negative Binomial" = "Negative Binomial",
                                         "Discrete Uniform" = "Discrete Uniform"),
                          selected = "Normal"),
             
             sliderInput( inputId = "size", label = "Sample Size",
                          value = 50, min = 1, max = 10000,
                          step = 1),
             sliderInput( inputId = "num_sim", label = "Number of Simulations",
                          value = 100, min = 1, max = 10000,
                          step = 1),
             radioButtons( inputId = "sum_mean", label = "Sum/Mean",
                          choices = list("Sum" = "Sum",
                                         "Mean" = "Mean"),
                          selected = "Sum"),
             numericInput( inputId = "seed", label = "Choose the seed to set",
                          value = 12345, min = 1, max = 50000),
             numericInput( inputId = "num_bins", label = "Select number of bins",
                           value = 25, min = 1, max = 100),
             textInput( inputId = "fill_color", label = "Bin Color",
                        value = "plum"),
             checkboxInput( inputId = "add_density", label = "Add density line", FALSE),
             checkboxInput( inputId = "add_avg_vline", label = "Add average line", FALSE)

             ),
        box(h4( "Parameters" ),
            conditionalPanel(
                condition = "input.distr == 'Normal'",
                numericInput("mean",
                             label = "mean",
                             min = -1000, max = 1000, value = 0),
                numericInput("sd",
                             "standard deviation",
                             min = 0, max = 1000, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Exponential'",
                numericInput("exp_rate",
                             label = "rate",
                             min = 0.01, max = 1000, value = 1),
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Gamma'",
                numericInput("shape",
                             label = "shape",
                             min = 0.01, max = 1000, value = 1),
                numericInput("r2",
                             "rate",
                             min = 0.01, max = 1000, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Beta'",
                numericInput("shape1",
                             label = "shape 1",
                             min = 0.01, max = 1000, value = 1),
                numericInput("shape2",
                             "shape 2",
                             min = 0.01, max = 1000, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Pareto'",
                numericInput("pareto_scale",
                             label = "scale",
                             min = 0.01, max = 1000, value = 1),
                numericInput("pareto_rate",
                             "rate",
                             min = 0.01, max = 100, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Cauchy'",
                numericInput("cauchy_location",
                             label = "location",
                             min = -100, max = 100, value = 1),
                numericInput("cauchy_scale",
                             "scale",
                             min = 0.01, max = 100, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Laplace'",
                numericInput("laplace_location",
                             label = "location",
                             min = -100, max = 100, value = 1),
                numericInput("laplace_scale",
                             "scale",
                             min = 0.01, max = 100, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Continuous Uniform'",
                numericInput("cont_uniform_min",
                             label = "minimum",
                             min = -100, max = 100,step = 1, value = 0),
                numericInput("cont_uniform_max",
                             "maximum",
                             min = -100, max = 100,step = 1, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Binomial'",
                numericInput("n",
                             label = "n",
                             min = 0, max = 1000,step = 1, value = 10),
                numericInput("p",
                             "p",
                             min = 0, max = 1,step = .1, value = .5)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Poisson'",
                numericInput("pois_rate",
                             label = "rate",
                             min = .01, max = 1000,step = .01, value = 1)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Geometric'",
                numericInput("geom_prob",
                             label = "p",
                             min = 0, max = 1,step = .01, value = .5)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Negative Binomial'",
                numericInput("neg_bin_n",
                             label = "n",
                             min = 0, max = 1000,step = 1, value = 10),
                numericInput("neg_bin_p",
                             "p",
                             min = 0, max = 1,step = .01, value = .5)
                
            ),
            conditionalPanel(
                condition = "input.distr == 'Discrete Uniform'",
                numericInput("dis_unif_min",
                             label = "minimum",
                             min = -100, max = 100,step = 1, value = 0),
                numericInput("dis_unif_max",
                             "maximum",
                             min = -100, max = 100,step = 1, value = 1)
                
            )),
        
        box( plotOutput("value") )
    ) ),
    
    tags$head(tags$style(HTML("
    h4 {font-family: 'Optima'}
    * {font-family: 'Optima'}
                              ")))

    
    

           
    
    
)

server <- function(input, output) {
    
    
    samp <- reactive({
        fxn = switch(input$distr,
                      "Normal"=function(size) {rnorm(size, input$mean, input$sd)}, 
                     "Exponential"=function(size) {rexp(size, input$exp_rate)}, 
                     "Gamma"=function(size) {rgamma(size, input$shape, input$r2)},
                     "Beta" = function(size){ rbeta(size, input$shape1, input$shape2) },
                     "Pareto" = function(size) { rPareto(size, input$pareto_scale, input$pareto_rate) },
                     "Cauchy" = function(size) { stats::rcauchy(size, input$cauchy_location, input$cauchy_scale) },
                     "Laplace" = function(size) { rLaplace(size, input$laplace_location, input$laplace_scale)},
                     "Continuous Uniform" = function(size) {runif(size, input$cont_uniform_min, input$cont_uniform_max)},
                     "Binomial" = function(size) {rbinom(size, input$n, input$p)},
                     "Poisson" = function(size) {rpois(size, input$pois_rate)},
                     "Geometric" = function(size) {rgeom(size, input$geom_prob)}, 
                     "Negative Binomial" = function(size) {rnbinom(size, input$neg_bin_n, input$neg_bin_p )},
                     "Discrete Uniform" = function(size) {sample(input$dis_unif_min:input$dis_unif_max, size, replace = T)}
                     
        )
        
        sm = switch (input$sum_mean,
            "Mean" = mean,
            "Sum" = sum
        )
        input$seed
        mat_size = input$size * input$num_sim
        mat = matrix(fxn(mat_size), nrow = input$size)
        stat_interest = apply(mat, 2, sm)
        return(stat_interest)
    })

    
    
    output$value <- renderPlot({
        res <- ggplot(data.frame()) +
            xlab(paste0("Sample ", input$sum_mean, "s")) + 
            ylab("Density") + 
            ggtitle(paste0("Histogram of Sample ", input$sum_mean, "s"))+ 
            geom_histogram(aes(x = samp(), y = ..density.. ) ,
                           bins = input$num_bins, fill = input$fill_color)+
            theme_gp()
        
        if ( input$add_density & input$add_avg_vline){
            res = res + geom_density(aes(x = samp(), y = ..density.. , colour = "Density")) +
                geom_vline(aes(xintercept = mean(samp()), colour = "Average") ) + 
                scale_colour_manual(name = "Type of Line",
                                    values = c("Density" = "blue", 
                                               "Average" = "red"))
        }
        
        else if (input$add_density){
            res = res + geom_density(aes(x = samp(), y = ..density.. , colour = "Density"))+ 
                scale_colour_manual(name = "Type of Line",
                                    values = c("Density" = "blue" ))
        }
        
        else if (input$add_avg_vline){
            res = res + geom_vline(xintercept = mean(samp()) )+
                geom_vline(aes(xintercept = mean(samp()), colour = "Average") ) + 
                scale_colour_manual(name = "Type of Line", 
                                    values = c("Average" = "red"))
        }
        res
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
