
#####################################################################
############## Practical Machine learning Project ###################
#####################################################################


#####---Miguel Andres Porro ---#####


#loading Libraries

library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rattle)
library(scales)
library(randomForest)
set.seed(3006)


####### Downloading and reading the Data ########


download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 'training.csv', method ="curl")
pmlTrainingData <- read.csv('training.csv', na.strings = c("","NA","#DIV/0!"))


download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 'testing.csv', method ="curl")
finalTest <- read.csv('testing.csv', na.strings = c("","NA","#DIV/0!"))



######## Creating training and test set ###########


inTrain <- createDataPartition(pmlTrainingData$classe, p=.7, list=FALSE)
training <- pmlTrainingData[inTrain,]
testing <- pmlTrainingData[-inTrain,]

summary(training$classe)


######## Cleaning the Data ########


training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]
finalTest <- finalTest[, -c(1:7)]

## removing missing values. NAs and blank fields 

mostlyNAs <- which(colSums(is.na(training)) > nrow(training)/2)
training <- training[, -mostlyNAs]
testing <- testing[, -mostlyNAs]
finalTest <- finalTest[, -mostlyNAs]



######## Machine Learning ##########
####################################



###-----------------------------------------------------###
###--Recursive partitioning Model- decision tree model--###
###-----------------------------------------------------###



rpModelFit <- train(classe ~ ., method="rpart", data=training)
rpModelFit$finalModel


#plotting the model 

fancyRpartPlot(rpModelFit$finalModel, sub='')

#Predicting classe for cross validation dataset

rpPreds <- predict(rpModelFit, newdata=testing)
rpConMatrix <- confusionMatrix(rpPreds, testing$classe)
rpConMatrix

# Very poor accuracy with Recursive partitioning model

rpAccuracy = rpConMatrix$overall[[1]]
percent(rpAccuracy)

# Estimated out of sample error with the cross validation dataset for this model is

percent(1.00-rpAccuracy)



###-------------------------###
###-- Random Forest model --###
###-------------------------###


# Random Forests should be a more suitable model


fitControl <- trainControl(method="cv", number=3, verboseIter=F)
rfModelFit <- train(classe ~., method="rf", data=training, trControl=fitControl)
rfModelFit$finalModel


# Predict classe for cross validation dataset

rfPreds <- predict(rfModelFit, newdata=testing)
rfConMatrix <- confusionMatrix(rfPreds, testing$classe)
rfConMatrix

# Greater accuracy with random forest model

rfAccuracy = rfConMatrix$overall[[1]]
percent(rfAccuracy)

percent(1.00-rfAccuracy)

submissionPreds <- predict(rfModelFit, newdata=finalTest)
submissionPreds
