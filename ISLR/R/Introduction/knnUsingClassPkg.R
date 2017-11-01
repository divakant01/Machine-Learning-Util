#'@author Divakant Pandey
#'
#'@title knn by class
#'
#'@Description K nearest neighbour algorithm from class package
#'
#'Load Data
prc = read.csv("ISLR/src/main/resources/Prostate_Cancer.csv",
               stringsAsFactors = FALSE)

#'Remove Id
prc = prc[-1]

#'it helps us to get the numbers of patients
table(prc$diagnosis_result)

#' New column diagnosis for changing M and B to Malignant and Benign
prc$diagnosis <-
  factor(
    prc$diagnosis_result,
    levels = c("B", "M"),
    labels = c("Benign", "Malignant")
  )

#'it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)
round(prop.table(table(prc$diagnosis)) * 100, digits = 1)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#'normalize the numeric features in the data set
prc_n <- as.data.frame(lapply(prc[2:9], normalize))

summary(prc_n$radius)

#'Train Data
prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]

#'Label data
prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]

library(class)

#' Build model
prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=9)

library(gmodels)

#'Evaluate model performance
CrossTable(x=prc_test_labels,y=prc_test_pred,prop.chisq = FALSE)
