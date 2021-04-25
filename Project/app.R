library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(randomForest)


source("BlackCarbon.r")
##

top_left <- "1.jpg"
top_right <- "2.jpg"
bottom_left <- "3.jpg"
bottom_right <- "4.jpg"


# Read in the RF model
model <- readRDS("model.rds")
golf <- readRDS("Golf.rds")

ui <- fluidPage(
    
    navbarPage(title = div("web",img(src="web.png", height = '27px', width = '27px'),"APP"), collapsible = TRUE, inverse = FALSE, theme = shinytheme("cosmo"),
               tabPanel("Home", icon = icon("glyphicon glyphicon-home", lib = "glyphicon"),
                        tagList(
                          
                          tags$head(
                            tags$link(href="design.css", rel="stylesheet", type="text/css")
                          ),
                          
                          shinyUI(
                            
                             tags$div(class="landing-wrapper",
                                                         
                                                         tags$div(class="landing-block background-content",
                                                                  
                                                                  img(src=top_left),
                                                                  
                                                                  img(src=top_right),
                                                                  
                                                                  img(src=bottom_left), 
                                                                  
                                                                  img(src=bottom_right)
                                                                  
                                                         ),
                                                         
                                                         tags$div(class="landing-block foreground-content",
                                                                  tags$div(class="foreground-text",
                                                                           tags$h1("Welcome"),tags$hr(),
                                                                           tags$h3("Web Application Using Shiny"),
                                                                           tags$blockquote(tags$p("Designed By :- Ritik Rajput")),
                                                                           tags$div(
                                                                             tags$a(href="https://github.com/Ritik7404",tags$img(src="github-logo.png",height="22px",width="22px")),
                                                                            
                                                                             tags$a(href="https://www.linkedin.com/in/ritik-rajput-7111511ab/",tags$img(src="linkedin.png",height="22px",width="22px"))
                                                                           )
                                                                  )
                                                         )
                                                )
                                       
                            
                          )
                        )
                        ),
               tabPanel("Projects",br(),
                        fluidPage(
                          setBackgroundImage(
                            src = "new.jpg"
                          ),
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
                                             tags$label(h1('Prediction : ')), # Status/Output Text Box
                                             
                                             tableOutput('tabledata0'), # Prediction results table
                                             imageOutput('myimage')
                                           )
                                         )
                                         
                                         
                                         ),
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
                                          
                                           tableOutput('tabledata') # Results table
                                         ) 
                                         ),
                                tabPanel(" BCC - Visualisation",icon = icon("glyphicon glyphicon-pencil",lib = "glyphicon"),br(),
                                         
                                         fluidPage(
                                           
                                           # App title ----
                                           titlePanel("Black Carbon Concentration in Air, Using Aethelometer Dataset"),
                                           
                                           # Sidebar layout with input and output definitions ----
                                           sidebarLayout(
                                             # Sidebar panel for inputs ----
                                             sidebarPanel(
                                               
                                               # Input: Slider for the number of bins ----
                                               selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                                                           selected = "Blue",multiple = F),
                                               
                                               radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                                               
                                               selectInput(inputId="channel1",label="Choose Channel",choices = c("BC1"="BC1",
                                                                                                                 "BC2"="BC2",
                                                                                                                 "BC3"="BC3",
                                                                                                                 "BC4"="BC4",
                                                                                                                 "BC5"="BC5",
                                                                                                                 "BC6"="BC6",
                                                                                                                 "BC7"="BC7"),
                                                           selected = "BC6",multiple = F),
                                               
                                               sliderInput(inputId = "bins1xz",
                                                           label = "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 30),
                                               
                                               sliderInput(inputId = "range1",
                                                           label = "Data Range",
                                                           min = 1,
                                                           max = 31,
                                                           value = c(1,31))
                                               
                                             ),
                                             
                                             # Main panel for displaying outputs ----
                                             mainPanel(
                                               
                                               # Output: Histogram ----
                                               plotOutput(outputId = "distPlot"),
                                               plotOutput(outputId = "distPlot1"),
                                               plotOutput(outputId = "distPlot2")
                                             )
                                           )
                                         )
                                         
                                )
                            )))
               
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
 
  
  # Prediction results table
  output$tabledata0 <- renderTable({
    if (input$submitbutton0>0) { 
      isolate(datasetInput0()) 
    } 
  })
  
  output$myimage <- renderImage({
    if(input$submitbutton0 == 0){
      validate(
        need(input$myimage != "", "Please select a data set")
      )
    }
    
    if (input$submitbutton0>0) { 
      filename <- normalizePath(file.path(
        paste(isolate(datasetInput0())$Prediction, '.jpg', sep='')))
    }
    if (input$submitbutton0>0){
      list(src = filename,
           alt = paste("Image number", input$n))
    }
    
    
  }, deleteFile = FALSE)
  
  
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
  

  
  # Prediction results table
  output$tabledata1 <- renderTable({
    if (input$submitbutton1>0) { 
      isolate(datasetInput1()) 
    } 
  })
  
  
  
  
  ############################
  
  
  
  
  output$distPlot <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    p2 <- data %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot()
    if(input$channel1 == "BC1"){
      p2 <- p2 + geom_histogram(aes(x=BC1),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC2"){
      p2 <- p2 + geom_histogram(aes(x=BC2),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC3"){
      p2 <- p2 + geom_histogram(aes(x=BC3),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC4"){
      p2 <- p2 + geom_histogram(aes(x=BC4),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC5"){
      p2 <- p2 + geom_histogram(aes(x=BC5),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC6"){
      p2 <- p2 + geom_histogram(aes(x=BC6),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC7"){
      p2 <- p2 + geom_histogram(aes(x=BC7),bins = input$bins1xz,col=input$border1,fill=sColor)
    }
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Black Carbon (ng/m3)",y="Count",title=paste("Black Carbon Concentration Histogram",input$channel1,sep = " "))
    
    p2
    #hist(x, breaks = bins, col = sColor, border = input$border1,
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
  })
  
  output$distPlot1 <- renderPlot({
    
    p1 <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot(aes(x=DateTime))
    if(input$channel1 == "BC1"){
      p1 <- p1 + geom_line(aes(y=BC1,col="BC1"),size=0.5)
    }else
      if(input$channel1 == "BC2"){
        p1 <- p1 + geom_line(aes(y=BC2,col="BC2"),size=0.5)
      }else
        if(input$channel1 == "BC3"){
          p1 <- p1 + geom_line(aes(y=BC3,col="BC3"),size=0.5)
        }else
          if(input$channel1 == "BC4"){
            p1 <- p1 + geom_line(aes(y=BC4,col="BC4"),size=0.5)
          }else
            if(input$channel1 == "BC5"){
              p1 <- p1 + geom_line(aes(y=BC5,col="BC5"),size=0.5)
            }else
              if(input$channel1 == "BC6"){
                p1 <- p1 + geom_line(aes(y=BC6,col="BC6"),size=0.5)
              }else
                if(input$channel1 == "BC7"){
                  p1 <- p1 + geom_line(aes(y=BC7,col="BC7"),size=0.5)
                }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Time",y="Black Carbon (ng/m3)",title="Black Carbon Concentration in Air - Dec, 2017",colour="Channel")
    
    p1
    
  })
  
  output$distPlot2 <- renderPlot({
    d <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2])
    
    d <- ddply(d, .variables = c("Hour"),function(x){
      
      BC1avg <- mean(x$BC1,na.rm = T)
      BC2avg <- mean(x$BC2,na.rm = T)
      BC3avg <- mean(x$BC3,na.rm = T)
      BC4avg <- mean(x$BC4,na.rm = T)
      BC5avg <- mean(x$BC5,na.rm = T)
      BC6avg <- mean(x$BC6,na.rm = T)
      BC7avg <- mean(x$BC7,na.rm = T)
      
      data.frame(BC1avg,BC2avg,BC3avg,BC4avg,BC5avg,BC6avg,BC7avg)
    })
    
    p1 <- d %>% ggplot(aes(x=Hour))
    if(input$channel1 == "BC1"){
      p1 <- p1 + geom_line(aes(y=BC1avg,col="BC1"),size=1)
      p1 <- p1 + geom_point(aes(y=BC1avg))
    }else if(input$channel1 == "BC2"){
      p1 <- p1 + geom_line(aes(y=BC2avg,col="BC2"),size=1)
      p1 <- p1 + geom_point(aes(y=BC2avg))
    }else if(input$channel1 == "BC3"){
      p1 <- p1 + geom_line(aes(y=BC3avg,col="BC3"),size=1)
      p1 <- p1 + geom_point(aes(y=BC3avg))
    }else if(input$channel1 == "BC4"){
      p1 <- p1 + geom_line(aes(y=BC4avg,col="BC4"),size=1)
      p1 <- p1 + geom_point(aes(y=BC4avg))
    }else if(input$channel1 == "BC5"){
      p1 <- p1 + geom_line(aes(y=BC5avg,col="BC5"),size=1)
      p1 <- p1 + geom_point(aes(y=BC5avg))
    }else if(input$channel1 == "BC6"){
      p1 <- p1 + geom_line(aes(y=BC6avg,col="BC6"),size=1)
      p1 <- p1 + geom_point(aes(y=BC6avg))
    }else if(input$channel1 == "BC7"){
      p1 <- p1 + geom_line(aes(y=BC7avg,col="BC7"),size=1)
      p1 <- p1 + geom_point(aes(y=BC7avg))
    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Time",y="Black Carbon (ng/m3)",title="Black Carbon Concentration in Air - Average Diurnal Variation - Dec, 2017",colour="Channel")
    
    p1
    
  })
  
  
}

shinyApp(ui = ui, server = server)