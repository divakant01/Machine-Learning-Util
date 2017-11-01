#'@author Divakant Pandey
#'
#'@title Bootstrap
#'
#'@Description Estimating the Accuracy of a Statistic of Interest
#' Here we use bootstrap approach to assess the variability of Bo and B1.

library(ISLR)
attach(Auto)
library(boot)

# First we will create a fn which will return the coeffn ie Bo and B1
boot.fn=function(data,index){
  return (coef(lm(mpg~horsepower,data = Auto,subset = index)))
}

boot.fn(Auto,1:392)

# The boot.fn() function can also be used in order to create bootstrap estimates for the 
# intercept and slope terms by randomly sampling from among the observations with replacement
set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))
boot.fn(Auto,sample(392,392,replace = T))
boot.fn(Auto,sample(392,392,replace = T))

# Next we use the boot fn to calculate SE of 1000 bootstrap estimates for 
# the intercept and slope terms 
boot(Auto,boot.fn,1000)

# Lets see the SE for the regression coefficients, On comparing we infer that bootstrap values are
# more accurate since they are are not making assumptions like in linear model
summary (lm(mpg~horsepower ,data=Auto))$coef

# Quardatic Model
boot.fn=function(data,index){
  return (coef(lm(mpg~horsepower+I(horsepower^2))))
}

set.seed(1)
boot(Auto,boot.fn,1000)

# Compare with summary
summary(lm(mpg~horsepower+I(horsepower^2),data = Auto))$coef
