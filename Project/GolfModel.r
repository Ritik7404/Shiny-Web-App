library(data.table)
library(RCurl)
library(randomForest)


weather <- read.csv("weather-weka.csv")

# Converting char column to factor variable

weather[,"outlook"] <- as.factor(weather[,"outlook"])
weather[,"play"] <- as.factor(weather[,"play"])

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

saveRDS(model, "Golf.rds")
