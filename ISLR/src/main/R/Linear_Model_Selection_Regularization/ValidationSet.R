#'@author Divakant Pandey
#'
#'@title Validation Set approach
#'
#'@Description Lab Exercise
library(ISLR)

# Note that in order to yield the accurate estimates of test error results, We MUST only use training
# observations to perform all aspects of model fitting including variable selection.
# After selecting the best model based on Susbset selection we can now start computing test error.
attach(Hitters)
set.seed(1)

# Assign random vector to TRUE if present in training otherwise false
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)

test = !train

# Perform Best Subset selection
library(leaps)
regfit.full = regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)

# Compute validation set error for the best model for each size, model.matrix() is used in many
# regression packages for building an X matrix for data.
test.mat = model.matrix(Salary ~ ., data = Hitters[test, ])

val.errors = rep(NA, 19)
# For each model size calculate prediction and MSE
for (i in 1:19) {
  coefi = coef(regfit.full, id = i)
  pred = test.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean(Hitters$Salary[test] - pred) ^ 2
}

val.errors
# # Best model is the one that contains 7 variables
which.min(val.errors)
coef(regfit.full, which.min(val.errors))


predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}


# Finally we perform best subset selection on full dataset,  It is important that we make use 
# of the full data set in order to obtain more accurate coefficient estimates.
regfit.best = regsubsets(Salary ~ ., Hitters, nvmax = 19)
coef(regfit.best, which.min(val.errors))

# Chosse among model of diffrent sizes using cross validation
k = which.min(val.errors)
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# Perform cross validation on each models of diffrent sizes and compute test errors.
# This has given us a k×19 matrix, of which the (i, j)th element corresponds to the test MSE
#  for the ith cross-validation fold for the best j-variable model.
for (j in 1:k) {
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred) ^ 2)
  }
}

# We use the apply fn to avg over the col of this matrix in order to obtain a vector for which 
# jth element is the cross validation error for jth variable mdoel
mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type = "b")

# From the above plot we see that cv chooses 11 var model .
# We now perform best subset selection on full data set in order to obtain the full variable model.
reg.best=regsubsets(Salary~.,data = Hitters,nvmax = 19)
coef(reg.best,11)
