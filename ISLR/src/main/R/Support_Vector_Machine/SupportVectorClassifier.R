#'@author Divakant Pandey
#'
#'@title Support Vector Classifier
#'
#'@Description ISLR Lab Exercise

# The e1071 library contains implementations for a number of statistical learning methods.
# Alternative LiblineaR package is used for very large linear problems
library(e1071)

# kernel="linear" in svm method for Support Vector Classifier
# A cost argument allows us to specify the cost of a violation to the margin.For small value margins 
# will be wide and many support vectors will be on the margin or will violate the margin
# For large cost,the margins will be narrow and there will be few support vectors on the margin
# or violating the margin.
set.seed(1)
#  Generating the observations, which belong to two classes.
x=matrix(rnorm (20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1

# Check if class are linearly separable - Its not
plot(x, col=(3-y))

# In order for the svm fun to perform classification (as opposed to SVM-based regression), we must
# encode the response as a factor variable. We now create a df with the response coded as a factor
dat=data.frame(x=x, y=as.factor(y))

# The argument scale=FALSE tells the svm() function not to scale each feature to have mean zero or 
# standard deviation one; depending on the application,one might prefer to use scale=TRUE.
svmfit=svm(y~., data=dat , kernel ="linear", cost=10,scale=FALSE)

plot(svmfit , dat)

# The support vectors are plotted as crosses and the remaining observations are plotted as circles; 
# we see here that there are seven support vectors. We can determine their identities as follows:
svmfit$index

summary(svmfit)

# Use smaller cost value
svmfit=svm(y~., data=dat , kernel ="linear", cost =0.1,scale=FALSE)
 plot(svmfit , dat)

# The e1071 library includes a built-in function, tune(), to perform cross 
# validation. By default, tune() performs ten-fold cross-validation on a set
# of models of interest. The following command indicates that we want to 
# compare SVMs with a linear kernel, using a range of values of the cost parameter

set.seed(1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

# Generating and predicting test data
xtest=matrix(rnorm (20*2) , ncol=2)
ytest=sample (c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]= xtest[ytest==1,] + 1
testdat=data.frame(x=xtest , y=as.factor(ytest))

ypred=predict (bestmod ,testdat)
table(predict =ypred , truth=testdat$y )

# Prediction Accuracy for Cost=0.01
svmfit=svm(y~.,data = dat,kernel="linear",cost=0.01,scale = FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)

# Situation in which 2 classes are linearly separable, Then we can find a separating hyperplane 
# using the svm() function. We first further separate the two classes in our simulated data so that
# they are linearly separable:
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch =19)

dat=data.frame(x=x,y=as.factor(y))
# Large val of cost so that no observations are misclassified
svmfit=svm(y~., data=dat , kernel ="linear", cost=1e5)
summary(svmfit)
# We can see that margin is very narrow hence will perform poor on test data
plot(svmfit,dat)

# Smaller cost
svmfit=svm(y~., data=dat , kernel ="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)
