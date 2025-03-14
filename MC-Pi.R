library(shiny)
library(ggplot2)

# Server Function
server <- function(input, output) {
    
    # Function to perform Monte Carlo simulation
    monte_carlo_pi <- reactive({
        # Get the radius and square side length from the user input
        radius <- input$radius
        square_length <- input$square_length
        
        # Ensure the circle diameter doesn't exceed the square length
        if (radius * 2 > square_length) {
            radius <- square_length / 2  # Adjust radius to ensure it's valid
        }
        
        # Generate random points within the square (-square_length/2, square_length/2) x (-square_length/2, square_length/2)
        n_points <- input$num_points
        x <- runif(n_points, min = -square_length/2, max = square_length/2)
        y <- runif(n_points, min = -square_length/2, max = square_length/2)
        
        # Calculate the distance from the origin
        distance <- sqrt(x^2 + y^2)
        
        # Points inside the circle
        inside_circle <- distance <= radius
        
        # Estimate Pi (adjusted formula to scale based on the square length and radius)
        pi_estimate <- (sum(inside_circle) / n_points) * (square_length^2 / radius^2)
        
        # Create a data frame for plotting
        data <- data.frame(x = x, y = y, inside_circle = inside_circle)
        
        list(data = data, pi_estimate = pi_estimate)
    })
    
    # Render Plot
    output$plot <- renderPlot({
        result <- monte_carlo_pi()
        
        # Get radius and square length from the user input
        radius <- input$radius
        square_length <- input$square_length
        
        # Circle data (a set of points on the circle scaled by the radius)
        circle_data <- data.frame(
            x = radius * cos(seq(0, 2 * pi, length.out = 100)),
            y = radius * sin(seq(0, 2 * pi, length.out = 100))
        )
        
        # Plot the points and the circle
        ggplot(result$data, aes(x = x, y = y, color = inside_circle)) +
            geom_point(size = 2) +
            geom_path(data = circle_data, aes(x = x, y = y), color = "blue", size = 1) +  # Circle trace
            coord_fixed(ratio = 1) +
            theme_minimal() +
            scale_color_manual(values = c("red", "green")) +
            labs(title = paste("Estimate of Pi:", round(result$pi_estimate, 4)),
                 subtitle = paste("Points inside the circle vs total points"),
                 x = "X", y = "Y") +
            xlim(-square_length/2, square_length/2) +
            ylim(-square_length/2, square_length/2)
    })
    
    # Display Pi Estimate
    output$pi_estimate <- renderText({
        result <- monte_carlo_pi()
        paste("Current estimate of Pi:", round(result$pi_estimate, 4))
    })
}

# UI Function
ui <- fluidPage(
    # Custom CSS for background colors and slider width
    tags$style(HTML("
        .container {
            background-color: #f0f0f0;
            padding: 20px;
            border-radius: 10px;
        }
        .panel {
            background-color: #fff3e6;
            padding: 15px;
            margin-bottom: 20px;
            border-radius: 8px;
        }
        .plot-panel {
            background-color: #e6f7ff;
            padding: 20px;
            border-radius: 8px;
        }
        .slider {
            width: 100%;
        }
    ")),
    
    # Title Panel
    titlePanel("Monte Carlo Simulation: Estimating Pi"),
    
    # Container for the entire UI with padding
    div(class = "container",
        # Sliders and explanation
        div(class = "panel",
            sliderInput("num_points", "Number of Points:", min = 100, max = 50000, value = 1000, step = 100, width = '100%'),
            sliderInput("radius", "Radius of Circle:", min = 0.1, max = 2, value = 1, step = 0.1, width = '100%'),
            sliderInput("square_length", "Length of Square:", min = 2, max = 10, value = 2, step = 0.1, width = '100%'),
            hr(),
            
            # Explanation paragraph about how Monte Carlo works to estimate Pi with LaTeX
            withMathJax(
                p(
                    "The unit circle has an area of \\( \\pi r^2 \\). We can bound this circle area with a square of side length \\( 2r \\) and thus have a square of area \\( 4r^2 \\). The Monte Carlo simulation uses random sampling of points inside the bounding square. First, we randomly generate points with \\( x \\) and \\( y \\) coordinates between \\( -r \\) and \\( r \\). Secondly, we check if the point falls inside or outside the unit circle using the equation \\( x^2 + y^2 \\leq r^2 \\) and counting how many points fall inside the circle and outside. The probability that a randomly chosen point in the square also falls inside the circle is \\( \\frac{\\pi r^2}{4r^2} = \\frac{\\pi}{4} \\). Then, we also know \\( \\frac{\\text{Number of points in circle}}{\\text{Number of points inside square}} \\approx \\frac{\\pi}{4} \\). Thus, we can approximate \\( \\pi \\) by rewriting as \\( \\frac{\\text{Number of points in circle}}{\\text{Number of points inside square}} \\times 4 \\approx \\pi \\)."
                )
            ),
            
            hr(),
            withMathJax(
                p("In the more general case, consider an unknown square length \\( s \\) and an unknown radius \\( r \\). Then the formula for \\( \\hat{\\pi} \\) is:",
                  "\\( \\hat{\\pi} = \\frac{\\text{Number of points inside the circle}}{\\text{Total number of points}} \\times \\frac{\\text{Area of square}}{\\text{Area of circle}} = \\frac{\\text{Number of points inside the circle}}{\\text{Total number of points}} \\times \\frac{s^2}{r^2} \\)."
                )
            ),
        ),
        
        # Text output for Pi Estimate
        textOutput("pi_estimate"),
        
        hr(),
        
        # Plot output in a new panel
        div(class = "plot-panel", plotOutput("plot"))
    )
)

# Run the Application
shinyApp(ui = ui, server = server)
