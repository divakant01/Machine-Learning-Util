#'@author Divakant Pandey
#'
#'@title Support Vector Machine
#'
#'@Description ISLR Lab Exercise

library(e1071)
# Kernel =polynomial (degree is mentioned) or radial (gamma is mentioned) in svm method
set.seed(1)
x=matrix(rnorm (200*2) , ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150 ,]=x[101:150,]-2
y=c(rep(1,150) ,rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample (200,100)
# Radial Kernel
svmfit=svm(y~., data=dat[train ,], kernel ="radial", gamma=1, cost=1)
plot(svmfit , dat[train ,])
summary(svmfit)

# We can see from the figure that there are a fair number of training errors in this SVM fit. If we
# increase the value of cost, we can reduce the number of training errors. However, this comes at 
# the price of a more irregular decision boundary that seems to be at risk of overfitting the data
svmfit=svm(y~., data=dat[train ,], kernel ="radial",gamma=1,cost=1e5)
plot(svmfit ,dat[train ,])

# We can perform cv using tune to select the best choice of gamma and cost for an SVM with a radial kernel:
set.seed(1)
tune.out=tune(svm , y~., data=dat[train ,], kernel ="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
# Best is cost=1, gamma=2     
summary (tune.out)

# Prediction using best model , 38 are misclassified
table(true=dat[-train ,"y"], pred=predict(tune.out$best.model,newx=dat[-train ,]))
