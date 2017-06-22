#'@author Divakant Pandey
#'
#'@title Validation Set
#'
#'@Description Lab Exercise
library(ISLR)
attach(Auto)

set.seed(1)

train=sample(392,196)

lm.fit=lm(mpg~horsepower,data = Auto,subset = train)

# Calculate MSE
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# Degree 2 Polynomial
lm.fit2=lm(mpg~poly(horsepower,2),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# Degree 3 Polynomial
lm.fit3=lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

