#'@author Divakant Pandey
#'
#'@title Boosting
#'
#'@Description ISLR Lab Exercise
library(ISLR)
library(MASS)
library(gbm)

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
boston.test=Boston[-train,"medv"]

# gbm() function for Boosted Regression Tree, We run gbm() with the option distribution="gaussian" 
# since this is a regression problem; if it were a binary classification problem, we would use 
# distribution="bernoulli"
# The argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 
# limits the depth of each tree.
boost.boston=gbm(medv~.,data=Boston[train,],distribution ="gaussian",n.trees = 5000,
                 interaction.depth =  4)
summary(boost.boston)

# We can also produce partial dependence plots for these two variables. These plots illustrate the 
# marginal effect of the selected variables on the response after integrating out the othervariables
# In this case, as we might expect, median house prices are increasing with rm and decreasing with 
# lstat.
par(mfrow=c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

# We now use the boosted model to predict medv on the test set:
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],n.trees=5000)

# The test MSE obtained is 11.8; similar to the test MSE for random forests 
# and superior to that for bagging.
mean((yhat.boost - boston.test)^2)

# If we want to, we can perform boosting with a different value of the shrinkage parameter λ.
# The default value is 0.001, but this is easily modified. Here we take λ = 0.2.
boost.boston=gbm(medv~.,data=Boston[train ,], distribution=
                   "gaussian",n.trees =5000, interaction.depth =4, shrinkage =0.2,
                 verbose=F)
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],
                    n.trees=5000)
mean((yhat.boost - boston.test)^2)
