

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Integración Montecarlo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput('fun', 'Función:', 
                    list('sqrt(4-x^2)'=1, 
                         '4/(1+x^2)'=2, 
                         '6/sqrt(4-x^2)'=3), 
                    4, selected = 1, multiple=F),
        numericInput('a', 'Límite superior:', value = 0),
        numericInput('b', 'Límite inferior:', value = 2),
        sliderInput('alpha','Significancia de los intervalos:', min = 0.001, max = 0.1, value = 0.05, step = 0.001),
        numericInput('seed','Semilla:', value = 211213)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        print("Prueba1")
        #plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Prueba1 <- reactive({  
            print(input$fun)
  x <- runif(input$N, input$a, input$b)
  fx <- (input$b - input$a)*sapply(x, function(x) fun(x, fun=input$fun))
              
          })# Cierra reactive
  
   #output$distPlot <- renderPlot({
      
      
     # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   #})
}

# Run the application 
shinyApp(ui = ui, server = server)

