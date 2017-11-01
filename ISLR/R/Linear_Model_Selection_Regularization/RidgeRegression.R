#'@author Divakant Pandey
#'
#'@title Ridge Regression
#'
#'@Description Lab Exercise

library(ISLR)

# Perform Ridge Regression and Lasso
library(glmnet)

attach(Hitters)

#Check NA values for salary var
sum(is.na(Hitters$Salary))

# Remove NA
Hitters=na.omit(Hitters)

# not only does it produce a matrix corresponding to the 19 predictors but it also automatically 
# transforms any qualitative variables into dummy variables.The latter property is important 
# because glmnet() can only take numerical,quantitative inputs
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# glmnet automatically select range of lamda values by default. Here we will cover the  full range of
# scenarios from the null model containing only the intercept, to the least squares fit.
# Also glmnet standardizes the variables by default.
grid=10^seq(10,-2,length=100)
# alpha =0 is ridge
ridge.mod=glmnet(x,y,alpha = 0,lambda = grid)
dim(coef(ridge.mod))

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value 
# of lamdba is used, as compared to when a small value of lamdba is used.
# lambda=11498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# lambda=705
ridge.mod$lambda [60]
coef(ridge.mod)[ ,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2) )

# We can use the predict() function for a number of purposes. For instance, we can obtain the 
# ridge regression coefficients for a new value of lambda, say 50
predict(ridge.mod,s=50,type = "coefficients")[1:20,]

# Split data into test and train
set.seed(1)
train =sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]

# Fit regression model on training set and evaluate its MSE on test set using lamda=4
ridge.mod=glmnet(x[train,],y[train],alpha = 0,lambda = grid,thresh = 1e-12)
ridge.pred=predict(ridge.mod,s=4,newx = x[test,])
mean((ridge.pred-y.test)^2)

# try with very large lamda, Previous MSE was better on comparison
ridge.pred=predict (ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

# check whether least sqr is better or Shrinkage, lamda=0
# 5In order for glmnet() to yield the exact least squares coefficients when lamda = 0,
# we use the argument exact=T when calling the predict() function. Otherwise, the
# predict() function will interpolate over the grid of lamda values used in fitting
ridge.pred=predict (ridge.mod ,s=0, newx=x[test ,], exact=T)
mean((ridge.pred -y.test)^2)
lm(y~x, subset=train)
predict (ridge.mod ,s=0,exact=T,type="coefficients")[1:20,]

# Lets use Cross Validation to choose Lambda using cv.glmnet (By default it performs 10 fold cv)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
# lambda having smallest cv error
bestlambda=cv.out$lambda.min
bestlambda

# test error associated with above lamda
ridge.pred=predict (ridge.mod ,s=bestlambda ,newx=x[test ,])
mean((ridge.pred -y.test)^2)


# Finally, we refit our ridge regression model on the full data set,using the value of lamda 
# chosen by cross-validation, and examine the coefficient estimates.
out=glmnet(x,y,alpha=0)
predict (out ,type="coefficients",s= bestlambda) [1:20,]
