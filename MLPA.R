#load libraries
library(caret)
library(dplyr)
library(ggplot2)
library(rattle)

#load files
data<- read.csv("pml-training.csv")
pmltesting <- read.csv("pml-testing.csv")

#selecting columns with at least 80% NAs values or blank values

limit <-length(data$X) * 0.8
data[data==""] <- NA

isna <-data.frame(sapply(data,is.na))
colnames <- names(data)

NAcols <- vector()

for (i in 1:length(colnames)){
    if (sum(isna[[i]]) > 0){
        NAcols <-c(NAcols,i)
    }
}


#selecting columns with temporal valuesand near zero variance (we won't need them)

Timecols<- grep("time",colnames)


#removing unecessary columns from the data and pmltesting 

remcols <- c(NAcols,Timecols,1,2,6,7)


data<-select(data,-remcols)
pmltesting<-select(pmltesting,-remcols)


#set seed
set.seed(12345)

#subset testing into training, testing and validation


inTrain <- createDataPartition(y = data$classe,p = 0.7,list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]


#making a analysis trough Boosting,Recursive Partitioning And Regression Tree and a GAM including both model

modelRPART <- train(classe ~ ., method = "rpart",data = training)
predRPART <- predict(modelRPART,testing)
cmRPART <- confusionMatrix(predRPART,testing$classe)

controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modelGBM <- train(classe ~ ., method = "gbm",data = training,trControl = controlGBM ,verbose = FALSE)
predGBM <- predict(modelGBM,testing)
cmGBM <- confusionMatrix(predGBM,testing$classe)

modelRF <- train(classe ~ ., method = "rf",data = training, verbose = FALSE)
predRF <- predict(modelRF,testing)
cmRF <- confusionMatrix(predRF,testing$classe)


pred <- predict(modelRF,pmltesting)


