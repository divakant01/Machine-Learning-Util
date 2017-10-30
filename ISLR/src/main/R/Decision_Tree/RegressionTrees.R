#'@author Divakant Pandey
#'
#'@title Regression Tree
#'
#'@Description ISLR Lab Exercise
library(ISLR)
library(MASS)
library(tree)
set.seed(1)

train=sample(1:nrow(Boston),nrow(Boston)/2)

tree.boston=tree(medv~.,Boston,subset = train)

# Only 3 var have been used in constructing the tree
# In the context of Regression tree the deviance is the sum of squared error for tree
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty = 0)

# Now check if prunign tree will improve performance
# The most complex tree is selected by cross-validation
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type = 'b')

# Prune the tree
prune.boston=prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston,pretty = 0)

# In keeping with the cross-validation results, we use the unpruned tree to make 
# predictions on the test set
yhat=predict(tree.boston,newdata = Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline (0,1)
# Test MSE asociated with regression tree is 25.04 %, Sqr root is around 5.005,indicating that 
# this model leads to test predictions that are within around $5, 005 of the true median home 
# value for the suburb.
mean((yhat -boston.test)^2)
