# Overview

# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 
# In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 
# They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


# Objective

# The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

# Data loading and preparing
  library(caret)
  library(doParallel)
  
  #Reading and confirming data
  fnameTraining <- "pml-training.csv"
  fnameTesting <- "pml-testing.csv"
  
  fileURLTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  fileURLTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  
  # File already exists?
  if (!file.exists(fnameTraining)){
    download.file(url = fileURLTraining, 
                  destfile = fnameTraining)
  }  
  
  # File already exists?
  if (!file.exists(fnameTesting)){
    download.file(url = fileURLTest, 
                  destfile = fnameTesting)
  }  
  
  # Data read
  trainingData <- read.table(fnameTraining,
                             sep = ",",
                             dec = ".",
                             header = TRUE,
                             na.strings = "NA") 

  validationData <- read.table(fnameTesting,
                             sep = ",",
                             dec = ".",
                             header = TRUE,
                             na.strings = "NA")
  
  # Checking datasets
  
  str(trainingData)
  str(validationData)
  
  # Cleaning: Near Zero Variance variables from datasets
  nzv <- nearZeroVar(trainingData, saveMetrics = T)
  trainingData <- trainingData[, !nzv$nzv] 
  validationData <- validationData[, !nzv$nzv]
  
  # Cleaning: Remove columns that contain NA's
  nas <- (colSums(is.na(trainingData)) == 0)
  trainingData <- trainingData[, nas]
  validationData <- validationData[, nas]
  
  # Cleaning: remove 1 to 5 columns from datasets (useless)
  trainingData <- trainingData[ , -c(1:5)]
  validationData <- validationData[ , -c(1:5)]

  # Data partition: 
  # Training data = 60% training file
  # Testing data = 40% training file
  # Validation data = 100% original testing file
  
  set.seed(1609)
  partitionIndex <- createDataPartition(y=trainingData$classe, p=0.6, list=FALSE)
  trainingData <- trainingData[partitionIndex, ]
  testingData <- trainingData[-partitionIndex, ]
  
# Prediction models selection and training

  # Lets test 3 differnt models to see the best out-of-sample accuracy. 
  # * Random forest decision trees (rf)
  # * Stochastic gradient boosting trees (gbm)
  # * Linear Discriminant Analysis (lda)
  
  
# Training our data
  
  # Enabling paralel processing for 3 cores
  #cl <- makePSOCKcluster(3)
  #registerDoParallel(cl)
  
  load(file = "./Modelrf.RData")
  #model_rf <- train(classe ~ ., 
  #                    data=trainingData,
  #                    method='rf')
  #save(model_rf, file="./Modelrf.RData")
  
  load(file = "./Modelgbm.RData")
  #model_gbm <- train(classe ~ .,
  #                  data=trainingData,
  #                 method='gbm')
  #save(model_gbm, file="./Modelgbm.RData")
  
  load(file = "./Modelda.RData")
  #model_lda <- train(classe ~ .,
  #                  data=trainingData,
  #                  method='lda')
  #save(model_lda, file="./Modelda.RData")
  
  
  
# Model analysis

  
  # Run RF model
  predmodel_rf <- predict(model_rf, newdata=testingData)
  cmmodel_rf <- confusionMatrix(table(predmodel_rf, testingData$classe))
  
  
  # Run GBM model
  predmodel_gbm <- predict(model_gbm, newdata=testingData)
  cmmodel_gbm <- confusionMatrix(table(predmodel_gbm, testingData$classe))
 
  
  # Run LDA model 
  predmodel_lda <- predict(model_lda, newdata=testingData)
  cmmodel_lda <- confusionMatrix(table(predmodel_lda, testingData$classe))
 
  
  accuracyTable <- data.frame(Model = c('RF', 'GBM', 'LDA'),
                              Accuracy = rbind(cmmodel_rf$overall[1], cmmodel_gbm$overall[1], cmmodel_lda$overall[1]))
  
  print(accuracyTable)
  
  # Stop paralel processing
  #stopCluster(cl)
  

# Model Prediction
  
  # The best model is choosen to predict values from Validation dataset
  # In this case, RF
  
  predData <- predict(model_rf, newdata=validationData)
  PredResults <- data.frame(problem_id=validationData$problem_id,
                                            predicted=predData)
  print(PredResults)
  
  varImpObj <- varImp(model_rf)
  plot(varImpObj, main = "Important Variables - RF", top = 20)
  