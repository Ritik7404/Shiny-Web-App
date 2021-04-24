library(shiny)
library(shinythemes)
library(shinydashboard)

library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")
golf <- readRDS("Golf.rds")

ui <- fluidPage(
    
    navbarPage(title = "PROJECT", collapsible = TRUE, inverse = FALSE, theme = shinytheme("cosmo"),
               tabPanel("Home", icon = icon("glyphicon glyphicon-home", lib = "glyphicon")),
               tabPanel("Projects",
                        fluidPage(
                            tabsetPanel(
                                tabPanel("Iris",icon = icon("glyphicon glyphicon-leaf", lib = "glyphicon"),
                                         
                                         pageWithSidebar(
                                           
                                           # Page header
                                           headerPanel('Iris Predictor'),
                                           
                                           # Input values
                                           sidebarPanel(
                                             HTML("<h3>Input parameters</h3>"),
                                             sliderInput("Sepal.Length", label = "Sepal Length", value = 5.0,
                                                         min = min(TrainSet$Sepal.Length),
                                                         max = max(TrainSet$Sepal.Length)
                                             ),
                                             sliderInput("Sepal.Width", label = "Sepal Width", value = 3.6,
                                                         min = min(TrainSet$Sepal.Width),
                                                         max = max(TrainSet$Sepal.Width)),
                                             sliderInput("Petal.Length", label = "Petal Length", value = 1.4,
                                                         min = min(TrainSet$Petal.Length),
                                                         max = max(TrainSet$Petal.Length)),
                                             sliderInput("Petal.Width", label = "Petal Width", value = 0.2,
                                                         min = min(TrainSet$Petal.Width),
                                                         max = max(TrainSet$Petal.Width)),
                                             
                                             actionButton("submitbutton0", "Submit", class = "btn btn-primary")
                                           ),
                                           
                                           mainPanel(
                                             tags$label(h3('Status/Output')), # Status/Output Text Box
                                             verbatimTextOutput('contents0'),
                                             tableOutput('tabledata0') # Prediction results table
                                             
                                           )
                                         )
                                         
                                         
                                         ),
                                tabPanel("Mtcars",icon = icon("glyphicon glyphicon-align-center",lib = "glyphicon")),
                                tabPanel("Play Golf",icon = icon("glyphicon glyphicon-fire",lib = "glyphicon"),
                                         
                                         headerPanel('Play Golf?'),
                                         
                                         # Input values
                                         sidebarPanel(
                                           HTML("<h3>Input parameters</h3>"),
                                           
                                           selectInput("outlook", label = "Outlook:", 
                                                       choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                                                       selected = "Rainy"),
                                           sliderInput("temperature", "Temperature:",
                                                       min = 64, max = 86,
                                                       value = 70),
                                           sliderInput("humidity", "Humidity:",
                                                       min = 65, max = 96,
                                                       value = 90),
                                           selectInput("windy", label = "Windy:", 
                                                       choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                                       selected = "TRUE"),
                                           
                                           actionButton("submitbutton1", "Submit", class = "btn btn-primary")
                                         ),
                                         
                                         mainPanel(
                                           tags$label(h3('Status/Output')), # Status/Output Text Box
                                           verbatimTextOutput('contents1'),
                                           tableOutput('tabledata1') # Prediction results table
                                           
                                         )
                                         
                                         ),
                                tabPanel("BMI", icon = icon("
glyphicon glyphicon-scale",lib = "glyphicon"),br(),br(),
                                         sidebarPanel(
                                           HTML("<h3>Input parameters</h3>"),
                                           sliderInput("height", 
                                                       label = "Height", 
                                                       value = 175, 
                                                       min = 40, 
                                                       max = 250),
                                           sliderInput("weight", 
                                                       label = "Weight", 
                                                       value = 70, 
                                                       min = 20, 
                                                       max = 100),
                                           
                                           actionButton("submitbutton", 
                                                        "Submit", 
                                                        class = "btn btn-primary")
                                         ),
                                         
                                         mainPanel(
                                           tags$label(h3('Status/Output')), # Status/Output Text Box
                                           verbatimTextOutput('contents'),
                                           tableOutput('tabledata') # Results table
                                         ) 
                                         )
                            ))),
               tabPanel("Github")
               
               
    )
)

server <- function(input, output, session) {
  # Input Data
  
  datasetInput <- reactive({  
    
    bmi <- input$weight/( (input$height/100) * (input$height/100) )
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
###################
  
  datasetInput0 <- reactive({  
    
    df <- data.frame(
      Name = c("Sepal Length",
               "Sepal Width",
               "Petal Length",
               "Petal Width"),
      Value = as.character(c(input$Sepal.Length,
                             input$Sepal.Width,
                             input$Petal.Length,
                             input$Petal.Width)),
      stringsAsFactors = FALSE)
    
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents0 <- renderPrint({
    if (input$submitbutton0>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata0 <- renderTable({
    if (input$submitbutton0>0) { 
      isolate(datasetInput0()) 
    } 
  })
  
  
###################
  
  datasetInput1 <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df0 <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df0 <- rbind(df0, play)
    input0 <- transpose(df0)
    write.table(input0,"Golfinput.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test0 <- read.csv(paste("Golfinput", ".csv", sep=""), header = TRUE)
    
    test0$outlook <- factor(test0$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(golf,test0), round(predict(golf,test0,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents1 <- renderPrint({
    if (input$submitbutton1>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata1 <- renderTable({
    if (input$submitbutton1>0) { 
      isolate(datasetInput1()) 
    } 
  })
  
  
  
}

shinyApp(ui = ui, server = server)