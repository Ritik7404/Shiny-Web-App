library(randomForest)
library(caret)
library(RCurl)
# Importing the Iris data set
iris <- read.csv("iris.csv")

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
str(TrainSet)

#Since here "Species" is a char column, we convert it to a factor

TrainSet[,"Species"] <- as.factor(TrainSet[,"Species"])
TrainSet <- TrainSet[,-1]
str(TrainSet)

# Building Random forest model

model <- randomForest(Species ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

# Save model to RDS file
saveRDS(model, "model.rds")

