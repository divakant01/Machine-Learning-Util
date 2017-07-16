#'@author Divakant Pandey
#'
#'@title Application to Gene Expression Data
#'
#'@Description ISLR Lab Exercise

# Khan data set, which consists of a number of tissue samples corresponding to four distinct types of small round blue cell tumors.
# For each tissue sample, gene expression measurements are available.
library(ISLR)
attach(Khan)
names(Khan)
dim(Khan$xtrain )
dim(Khan$xtest )
length(Khan$ytrain )
length(Khan$ytest )

table(Khan$ytrain )
table(Khan$ytest )

# We will use a support vector approach to predict cancer subtype using gene expression measurements. In this data set, there are a very large number
# of features relative to the number of observations
# This suggests that we should use a linear kernel, because the additional flexibility that will result
# from using a polynomial or radial kernel is unnecessary.
dat=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain ))
out=svm(y~., data=dat , kernel ="linear",cost=10)
# plot(out,dat)
summary (out)

#We see that there are no training errors. In fact, this is not surprising,because the large number of variables relative to the number of observations
# implies that it is easy to find hyperplanes that fully separate the classes.
table(out$fitted , dat$y)

# Test Accuracy
dat.te=data.frame(x=Khan$xtest , y=as.factor(Khan$ytest ))
pred.te=predict (out , newdata =dat.te)
# Cost=10 yields 2 test set errors on this data!
table(pred.te, dat.te$y)

