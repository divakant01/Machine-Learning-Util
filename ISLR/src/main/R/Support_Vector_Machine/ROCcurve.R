#'@author Divakant Pandey
#'
#'@title ROC curve
#'
#'@Description ISLR Lab Exercise

# The ROCR package can be used to produce ROC curves
library(ROCR)

# We first write a short function to plot an ROC curve given a vector containing a numerical score for 
# each observation, pred, and a vector containing the class label for each observation, truth.
rocplot =function (pred , truth , ...){
   predob = prediction (pred , truth)
   perf = performance (predob , "tpr", "fpr")
   plot(perf ,...)}


set.seed(1)
#  Generating the observations, which belong to two classes.
x=matrix(rnorm (20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1


# SVMs and support vector classifiers output class labels for each observation.However, it is also 
# possible to obtain fitted values for each observation,which are the numerical scores used to obtain the class labels.
# In essence, the sign of the fitted value determines on which side of the decision boundary the observation lies.
# if the fitted value exceeds zero then the observation is assigned to one class, and if it is less than zero than it is assigned to the other. 
# In order to obtain the fitted values for a given SVM model fit, we use decision.values=TRUE when fitting svm(). Then the predict() function will output the fitted values.
svmfit.opt=svm(y~., data=dat[train ,], kernel ="radial", gamma=2, cost=1, decision.values =T)
fitted =attributes(predict (svmfit.opt ,dat[train ,], decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot (fitted ,dat[train ,"y"], main="Training Data")

# SVM appears to be producing accurate predictions. By increasing gamma we can produce a more flexible fit and generate further improvements in accuracy
svmfit.flex=svm(y~., data=dat[train ,], kernel ="radial",gamma=50, cost=1, decision.values =T)
fitted=attributes (predict (svmfit.flex ,dat[train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[train ,"y"],add=T,col="red")

# On computation of roc curve on test data model with gamma =2 appears to provide the most accurate results
fitted =attributes(predict (svmfit.opt ,dat[-train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[-train ,"y"], main="Test Data")
fitted=attributes(predict (svmfit.flex ,dat[- train ,], decision.values=T))$decision.values
rocplot (fitted ,dat[-train ,"y"],add=T,col="red")
