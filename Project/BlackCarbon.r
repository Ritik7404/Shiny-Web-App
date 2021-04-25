library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)


# Data Preparation Steps

data <- read.csv("blackcarbon.csv")

data$Date <- strptime(as.character(data$Date.yyyy.MM.dd.),format="%m/%d/%Y")
data$Date <- as.POSIXct(data$Date)

data$DateTime <- strptime(as.character(data$DateTime),format="%m/%d/%Y %H:%M")
data$DateTime <- as.POSIXct(data$DateTime)

data$Day <- as.numeric(as.character(strftime(data$DateTime,format="%d")))
data$Hour <- as.numeric(as.character(strftime(data$DateTime,format="%H")))

data <- data %>% filter(BC6!=0)

