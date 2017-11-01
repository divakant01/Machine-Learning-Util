#'@author Divakant Pandey
#'
#'@title PLS
#'
#'@Description Lab Exercise

# Required for pls, plsr method
require(pls)
set.seed(2)

library(ISLR)
attach(Hitters)

sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)

# scale=TRUE -> Effect of standardizing each predictor
# validation ="CV" -> compute 10 folds cv error for each possible value of M, the no. of 
# principle component used.
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,validation="CV")

summary(pls.fit)

# CV Validation plot , Lowest is at M=2
validationplot(pls.fit,val.type = "MSEP")

# Evaluate corresponding test set MSE
set.seed(1)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
train =sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]
pls.pred=predict(pls.fit,x[test,],ncomp = 2)
mean((pls.pred-y.test)^2)

# Finally, we perform PLS using the full data set, using M = 2, the number of components 
# identified by cross-validation.
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
