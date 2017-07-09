#'@author Divakant Pandey
#'
#'@title Validation Set approach
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

# Split data into test and train
set.seed(1)
train =sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]

# alpha=1 for lasso
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha = 1,lambda=grid)
plot(lasso.mod)

# Perform cross validaiton and compute associated test error
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlambda=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlambda,newx = x[test,])
mean((lasso.pred-y.test)^2)


# Check the sparse coeffn estimates
out=glmnet (x,y,alpha=1, lambda=grid)
lasso.coef=predict (out ,type="coefficients",s= bestlambda) [1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
