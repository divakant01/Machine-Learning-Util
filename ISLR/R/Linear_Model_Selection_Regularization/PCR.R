#'@author Divakant Pandey
#'
#'@title PCR
#'
#'@Description Lab Exercise

# Required for pcr
require(pls)
set.seed(2)

library(ISLR)
attach(Hitters)

sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)

# scale=TRUE -> Effect of standardizing each predictor
# validation ="CV" -> compute 10 folds cv error for each possible value of M, the no. of 
# principle component used.
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")

summary(pcr.fit)

# CV Validation plot , Lowest is at M=16
validationplot(pcr.fit,val.type = "MSEP")

# Perform PCR on training data and evaluate its test set performance
set.seed(1)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
train =sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale="TRUE",validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

# Compute test MSE, M=10 is lowest
pcr.pred=predict(pcr.fit,x[test,],ncomp = 10)
# However, as a result of the way PCR is implemented,the final model is more difficult to 
# interpret because it does not perform any kind of variable selection or even directly produce coefficient estimates
mean((pcr.pred-y.test)^2)

#Finally, we fit PCR on the full data set, using M = 10, the number of components identified 
# by cross-validation
pcr.fit=pcr(y~x,scale="TRUE",ncomp=10)
summary(pcr.fit)
