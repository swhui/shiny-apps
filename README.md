# A Repertoire of Shiny Applications

This project contains a collection of shiny web applications, which I have used while teaching and created for fun. Below is a list and description of each shiny web application.

## [The Magic of CLT](https://swhui.shinyapps.io/CLT-Magic/)

The Central Limit Theorem states that under sufficient conditions, when we sum or take the mean of a sequence of independent random variables, their (normalized) sum approaches a normal distribution. Although a powerful and useful theorem, CLT is not the most intuitive. This application demonstrates CLT in action!

## [Euclidean Algorithm Calculator](https://swhui.shinyapps.io/euclidean-alg/)

The Euclidean Algorithm is a method to compute the greatest common divisor (GCD) between two integers. The GCD is the largest number that divides both integers without a remainder.

## [Plot a Function](https://swhui.shinyapps.io/plotting_functions/)

This is a simple application that plots functions between -10,000 to 10,000.

## Monte Carlo Simulation to find $\pi$

The unit circle has an area of $\pi r^2$. We can bound this circle area with a square of side length $2r$ and thus have a square of area $4r^2$. The Monte Carlo simulation uses random sampling of points inside the bounding square. First, we randomly generate points with $x$ and $y$ coordinates between $-r$ and $r$. Secondly, we check if the point falls inside or outside the unit circle using the equation $x^2 + y^2 \\leq r^2 $ and counting how many points fall inside the circle and outside. The probability that a randomly chosen point in the square also falls inside the circle is $\\frac{\\pi r^2}{4r^2} = \\frac{\\pi}{4}$. Then, we also know $\\frac{\\text{Number of points in circle}}{\\text{Number of points inside square}} \\approx \\frac{\\pi}{4} $. Thus, we can approximate $\\pi$ by rewriting as $\\frac{\\text{Number of points in circle}}{\\text{Number of points inside square}} \\times 4 \\approx \\pi$.

In the more general case, consider an unknown square length $s$ and an unknown radius $r$. Then the formula for $\\hat{\\pi}) is:
\\hat{\\pi} = \\frac{\\text{Number of points inside the circle}}{\\text{Total number of points}} \\times \\frac{\\text{Area of square}}{\\text{Area of circle}} = \\frac{\\text{Number of points inside the circle}}{\\text{Total number of points}} \\times \\frac{s^2}{r^2}$.
                
                