#'@author Divakant Pandey
#'
#'@title Forward and Backward selection
#'
#'@Description Lab Exercise
library(ISLR)
attach(Hitters)

library(leaps)

# Forward Selection
regfit.fwd=regsubsets(Salary~.,Hitters,nvmax = 19,method = "forward")

summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~.,Hitters,method = "backward",nvmax = 19)

summary(regfit.bwd)

# Check the best 7 var model identified by each
coef(regfit.fwd,7)

coef(regfit.bwd,7)

#coef(regfit.full,7)

