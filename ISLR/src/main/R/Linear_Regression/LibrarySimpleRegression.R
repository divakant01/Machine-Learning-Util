#'@author Divakant Pandey
#'
#'@title Linear Regression Library
#'
#'@Description Regression Analysis
library(MASS)
library(ISLR)

attach(Boston)

lm.fit=lm(medv~lstat)

summary(lm.fit)

names(lm.fit)

coef(lm.fit)

# Confidence interval of coefficient estimates
confint(lm.fit)

#Confidence and Prediction interval of prediction estimates
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = "confidence")

predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = "predict")


plot(lstat,medv)
abline(lm.fit)

abline (lm.fit ,lwd=3,col ="red")
plot(lstat ,medv ,col="red")
plot(lstat ,medv ,pch ="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

#Residual plot
plot(predict(lm.fit),residuals(lm.fit))

#Studentized residuals
plot(predict(lm.fit),rstudent(lm.fit))

# Leverage statistics for any number of predictors
plot(hatvalues(lm.fit))

#Get Max leverage stat value
which.max(hatvalues(lm.fit))
