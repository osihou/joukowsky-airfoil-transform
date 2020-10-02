
# install packages

install.packages("shiny")
install.packages("shinythemes")
install.packages("dplyr")
install.packages("readr")

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

joukowsky <- function (z){
  z+(1+0i)/z
}

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                
                titlePanel("Joukowsky transform"),
                
                
                
                withMathJax(),
                helpText('Transformation of a circle: $$ |Z - (- \\epsilon c + ia \\cdot tan(\\beta)) | = (a/cos(\\beta))^2,\\ where \\ a = c(1 + \\epsilon) $$ '),
                
                hr(),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    #Side bar
                    sliderInput(inputId = "eps", 
                                label = "EPS (ϵ):",
                                min = 0, 
                                max = 0.5, 
                                value = 0, 
                                step = 0.001,
                                animate = animationOptions(interval = 100)
                                ),
                    
                    sliderInput(inputId = "c", 
                                label = "C:",
                                min = 0, 
                                max = 1, 
                                value = 1, 
                                step = 0.01,
                                animate = animationOptions(interval = 100)
                    ),
                    
                    sliderInput(inputId = "b", 
                                label = "BETA_DEG (β):",
                                min = 0, 
                                max = 25, 
                                value = 0, 
                                step = 0.01,
                                animate = animationOptions(interval = 100)
                    ),
                    
                    textOutput(outputId = "desc")
                    
                    
                    
                  ),
                  
                  # Output: Description and lineplot
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "500px", width = "500px")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  set <- reactive({
    a <- seq(0,2*pi, length.out=10000)
    
    b <- pi*input$b/180
    
    aaa <- input$c*(1+input$eps)
    aa <- (aaa/cos(b))^2
    
    aa*(complex(real = sin(a) - input$eps*input$c, imaginary = cos(a) + aaa*tan(b)))
    
    
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(joukowsky(set()),
         type = "l",
         xlab = "Re", 
         ylab = "Im", 
         col = color, 
         fg = color, 
         col.lab = color, 
         col.axis = color,
         asp =1
         )
    
  })
  
  # Pull in description
  output$desc <- renderText({
    c("ϵ: ", input$eps, "c: ", input$c, "β: ", input$b,"°")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)




