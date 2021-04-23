library(shiny)
library(shinythemes)
library(shinydashboard)

ui <- 
    
    navbarPage(title = "PROJECT", collapsible = TRUE, inverse = FALSE, theme = shinytheme("cosmo"),
               tabPanel("Home", icon = icon("glyphicon glyphicon-home", lib = "glyphicon")),
               tabPanel("Projects",
                        fluidPage(
                            tabsetPanel(
                                tabPanel("Iris",icon = icon("glyphicon glyphicon-leaf", lib = "glyphicon")),
                                tabPanel("Mtcars",icon = icon("glyphicon glyphicon-align-center",lib = "glyphicon")),
                                tabPanel("Survival",icon = icon("glyphicon glyphicon-fire",lib = "glyphicon")),
                                tabPanel("BMI", icon = icon("
glyphicon glyphicon-scale",lib = "glyphicon"))
                            ))),
               tabPanel("Github")
               
               
    )


server <- function(input, output) {}

shinyApp(ui = ui, server = server)