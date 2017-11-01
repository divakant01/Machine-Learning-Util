#'@author Divakant Pandey
#'
#'@title SVM multi Classifier
#'
#'@Description ISLR Lab Exercise



set.seed(1)
#  Generating the observations, which belong to two classes.
x=matrix(rnorm (20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1

x=rbind(x, matrix(rnorm (50*2) , ncol=2))
y=c(y, rep(0,50))
x[y==0,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit=svm(y~., data=dat , kernel ="radial", cost=10, gamma =1)
plot(svmfit , dat)

# The e1071 library can also be used to perform support vector regression, if the response vector that is passed in to svm() is numerical rather than a factor.
