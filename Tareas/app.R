#library(shiny)
#library(plotly)
#library (datasets)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Método de la Función Inversa" ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("semilla",
                   "Semilla:", 
                   34756347),
      sliderInput("RanNum",
                  "Cantidad de simulaciones:",
                  min = 1,
                  max = 100000,
                  value = 1000),
      numericInput("lambda",
                   "Lambda:",
                   value=1/2 ),
      sliderInput("breaks",
                   "Número de break's:",
                   min = 2,
                   max = 50,
                   value = 25)#,
      #numericInput("obs", 
                   #"Número de observaciones:",
                   #10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot2"),
      verbatimTextOutput("summary"), #Crea una tabla de summary
      verbatimTextOutput("ks"),
      plotOutput("distPlot")
      
    )
  )
))

###################################################3

server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    set.seed(input$semilla)   # Semilla
    nsim <- input$RanNum      # Generamos números aleatorios
    lambda <- input$lambda    # Tamaño de la lambda
    U <- runif(input$RanNum)  # Generamos números aleatorios con una distribución uniforme
    Finv <- function(U, lambda=input$lambda){return(-log(1-U)/lambda)} # Función inversa
    X <-Finv(U, input$lambda)
    ks.test(X, pexp)
    
    # Histograma de los números aleatorios uniformes
    hist(U, breaks = as.numeric(input$breaks), main = "Distribución de Probabilidad Uniforme",
         ylab = "Frecuencia",col=c("green"))})
  
  output$distPlot2 <- renderPlot({
    set.seed(input$semilla)  # Semilla
    nsim <- input$RanNum     # Generamos números
    lambda <- input$lambda   # Tamaño de la lambda
    U <- runif(input$RanNum) # creando número aleatorios con una distribución uniforme
    Finv <- function(U, lambda=input$lambda){return(-log(1-U)/lambda)} # Función inversa
    X <-Finv(U, input$lambda)
    ks.test(X, pexp)
    
    hist(X,breaks = as.numeric(input$breaks), main="Distribución Exponencial",
         ylab = "Frecuencia",col=c("orange"),border = "black")
    })
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    summary(X)})
  
  output$ks <- renderPrint({
    ks.test(X, pexp)})
  }#AQUI TERMINA EL SERVER
  )
  
  


# Run the application 
shinyApp(ui = ui, server = server)


