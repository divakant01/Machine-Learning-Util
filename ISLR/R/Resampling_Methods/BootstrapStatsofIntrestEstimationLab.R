#'@author Divakant Pandey
#'
#'@title Bootstrap
#'
#'@Description Estimating the Accuracy of a Statistic of Interest
library(ISLR)
attach(Portfolio)

# Required for using boot func
library(boot)

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

# Randomly select 100 observations from the range 1 to 100 with , This is equivalent
# to constructing a new bootstrap data set and recomputing ˆα based on the new data set
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))

boot(Portfolio,alpha.fn,R = 1000)
