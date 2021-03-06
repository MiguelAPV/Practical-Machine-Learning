
> download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 'training.csv', method ="curl")
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 11.6M  100 11.6M    0     0   189k      0  0:01:03  0:01:03 --:--:--  234k
> pmlTrainingData <- read.csv('training.csv', na.strings = c("","NA","#DIV/0!"))
> download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 'testing.csv', method ="curl")
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 15113  100 15113    0     0  15113      0  0:00:01 --:--:--  0:00:01 18986
> finalTest <- read.csv('testing.csv', na.strings = c("","NA","#DIV/0!"))
> inTrain <- createDataPartition(pmlTrainingData$classe, p=.7, list=FALSE)
> training <- pmlTrainingData[inTrain,]
> testing <- pmlTrainingData[-inTrain,]
> summary(training$classe)
   A    B    C    D    E 
3906 2658 2396 2252 2525 
> 
> 
> training <- training[, -c(1:7)]
> testing <- testing[, -c(1:7)]
> finalTest <- finalTest[, -c(1:7)]
> mostlyNAs <- which(colSums(is.na(training)) > nrow(training)/2)
> training <- training[, -mostlyNAs]
> testing <- testing[, -mostlyNAs]
> finalTest <- finalTest[, -mostlyNAs]
> rpModelFit <- train(classe ~ ., method="rpart", data=training)
> rpModelFit$finalModel
n= 13737 

node), split, n, loss, yval, (yprob)
      * denotes terminal node

 1) root 13737 9831 A (0.28 0.19 0.17 0.16 0.18)  
   2) roll_belt< 130.5 12591 8696 A (0.31 0.21 0.19 0.18 0.11)  
     4) pitch_forearm< -33.95 1073    5 A (1 0.0047 0 0 0) *
     5) pitch_forearm>=-33.95 11518 8691 A (0.25 0.23 0.21 0.2 0.12)  
      10) magnet_dumbbell_y< 439.5 9722 6947 A (0.29 0.18 0.24 0.19 0.11)  
        20) roll_forearm< 123.5 6098 3607 A (0.41 0.18 0.18 0.17 0.06) *
        21) roll_forearm>=123.5 3624 2421 C (0.078 0.17 0.33 0.23 0.19) *
      11) magnet_dumbbell_y>=439.5 1796  879 B (0.029 0.51 0.043 0.22 0.19) *
   3) roll_belt>=130.5 1146   11 E (0.0096 0 0 0 0.99) *
> fancyRpartPlot(rpModelFit$finalModel, sub='')
Warning message:
Bad 'data' field in model 'call'.
To silence this warning:
    Call prp with roundint=FALSE,
    or rebuild the rpart model with model=TRUE. 
> 
> 
> rpPreds <- predict(rpModelFit, newdata=testing)
> rpConMatrix <- confusionMatrix(rpPreds, testing$classe)
> rpConMatrix
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1521  471  472  430  156
         B   29  369   30  165  140
         C  121  299  524  369  290
         D    0    0    0    0    0
         E    3    0    0    0  496

Overall Statistics
                                          
               Accuracy : 0.4945          
                 95% CI : (0.4816, 0.5073)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.3395          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9086   0.3240  0.51072   0.0000  0.45841
Specificity            0.6369   0.9233  0.77794   1.0000  0.99938
Pos Pred Value         0.4987   0.5034  0.32689      NaN  0.99399
Neg Pred Value         0.9460   0.8505  0.88277   0.8362  0.89120
Prevalence             0.2845   0.1935  0.17434   0.1638  0.18386
Detection Rate         0.2585   0.0627  0.08904   0.0000  0.08428
Detection Prevalence   0.5183   0.1246  0.27239   0.0000  0.08479
Balanced Accuracy      0.7728   0.6236  0.64433   0.5000  0.72889
> rpAccuracy = rpConMatrix$overall[[1]]
> percent(rpAccuracy)
[1] "49.4%"
> percent(1.00-rpAccuracy)
[1] "50.6%"
> fitControl <- trainControl(method="cv", number=3, verboseIter=F)
> rfModelFit <- train(classe ~., method="rf", data=training, trControl=fitControl)
> rfModelFit$finalModel

Call:
 randomForest(x = x, y = y, mtry = param$mtry) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 2

        OOB estimate of  error rate: 0.78%
Confusion matrix:
     A    B    C    D    E  class.error
A 3903    3    0    0    0 0.0007680492
B   16 2636    6    0    0 0.0082768999
C    0   18 2375    3    0 0.0087646077
D    0    0   48 2201    3 0.0226465364
E    0    1    2    7 2515 0.0039603960
> rfPreds <- predict(rfModelFit, newdata=testing)
> rfConMatrix <- confusionMatrix(rfPreds, testing$classe)
> rfConMatrix
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1672    4    0    0    0
         B    2 1134    4    0    0
         C    0    1 1021   10    0
         D    0    0    1  954    0
         E    0    0    0    0 1082

Overall Statistics
                                          
               Accuracy : 0.9963          
                 95% CI : (0.9943, 0.9977)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9953          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9988   0.9956   0.9951   0.9896   1.0000
Specificity            0.9991   0.9987   0.9977   0.9998   1.0000
Pos Pred Value         0.9976   0.9947   0.9893   0.9990   1.0000
Neg Pred Value         0.9995   0.9989   0.9990   0.9980   1.0000
Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
Detection Rate         0.2841   0.1927   0.1735   0.1621   0.1839
Detection Prevalence   0.2848   0.1937   0.1754   0.1623   0.1839
Balanced Accuracy      0.9989   0.9972   0.9964   0.9947   1.0000
> rfAccuracy = rfConMatrix$overall[[1]]
> percent(rfAccuracy)
[1] "99.6%"
> percent(1.00-rfAccuracy)
[1] "0.374%"
> submissionPreds <- predict(rfModelFit, newdata=finalTest)
> submissionPreds
 [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
