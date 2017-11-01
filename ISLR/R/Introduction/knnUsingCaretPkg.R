#'@author Divakant Pandey
#'
#'@title knn by Caret
#'
#'@Description K nearest neighbour algorithm from Caret package
#'
#'Load Data
prc = read.csv("ISLR/src/main/resources/Prostate_Cancer.csv",
               stringsAsFactors = FALSE)

library(caret)

#'createDataPartition used for splitting train and test data efficiently
idxtrain = createDataPartition(y = prc$diagnosis_result,
                               p = 0.70,
                               list = FALSE)

train_data = prc[idxtrain, ]
test_data = prc[-idxtrain, ]

prop.table(table(train_data$diagnosis_result))
prop.table(table(test_data$diagnosis_result))
prop.table(table(prc$diagnosis_result))

#'Normalize data using center and scaling
trainData = train_data[, names(train_data) != "diagnosis_result"]
normalizedData = preProcess(x = trainData, method = c("center", "scale"))


#'training and trian control data , Note e1071 package is reuired
ctrl = trainControl(method = "repeatedcv", repeats = 3)
model = train(
  diagnosis_result ~ .,
  data = train_data,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 20
)

model

plot(model)

#' Predict
prediction = predict(model, newdata = test_data)

confusionMatrix(prediction, test_data$diagnosis_result)

mean(prediction == test_data$diagnosis_result)




#'#Now verifying 2 class summary function
ctrl <-
  trainControl(
    method = "repeatedcv",
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
model = train(
  diagnosis_result ~ .,
  data = train_data,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 20
)

model

plot(model, prin.thres = 0.5, type = "S")

prediction = predict(model, newdata = test_data)

confusionMatrix(prediction, test_data$diagnosis_result)

mean(prediction == test_data$diagnosis_result)

#' #' Plot ROc curve
#' library(pROC)
#'
#' prediction=predict(model,newdata = test_data,type = "prob")
#'
#' knnROC=roc(test_data$diagnosis_result,prediction,levels=rev(test_data$diagnosis_result))
#' knnROC
#'
#' plot(knnROC, type="S", print.thres= 0.5)

#' Applying Random Forest to see the Performance diffrence
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
rfModel <-
  train(
    diagnosis_result ~ .,
    data = train_data,
    method = "rf",
    trControl = ctrl,
    preProcess = c("center", "scale"),
    tuneLength = 20
  )

rfModel

plot(rfModel)

prediction=predict(rfModel,newdata = test_data)

confusionMatrix(prediction,test_data$diagnosis_result)

mean(prediction==test_data$diagnosis_result)


