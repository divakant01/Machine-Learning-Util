#'@author Divakant Pandey
#'
#'@title Bagging and Random forest
#'
#'@Description ISLR Lab Exercise
library(ISLR)
library(MASS)
library(randomForest)

set.seed(1)

train=sample(1:nrow(Boston),nrow(Boston)/2)
# The argument mtry=13 indicates that all 13 predictors should be considered
# for each split of the tree—in other words, that bagging should be done.
bag.boston=randomForest(medv~.,data = Boston,subset = train,mtry=13,importance=TRUE)
bag.boston

# Test Set Accuracy
yhat.bag=predict(bag.boston,newdata = Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat.bag,boston.test)
abline(0,1)

# The test set MSE associated with the bagged regression tree is 13.16, almost
# half that obtained using an optimally-pruned single tree.
mean((yhat.bag -boston.test)^2)

# We could change the number of trees grown by randomForest() using the ntree argument
bag.boston= randomForest( medv~.,data=Boston , subset=train,mtry=13,ntree=25)
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
mean((yhat.bag -boston.test)^2)



# Random Forest
# By default, randomForest uses p/3 variables when building a random forest of regression trees, 
# and √p variables when building a random forest of classification trees.
set.seed(1)
rf.boston= randomForest(medv~.,data=Boston , subset=train ,mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
# The test set MSE is 11.31; this indicates that random forests yielded an 
# improvement over bagging in this case.
mean((yhat.rf-boston.test)^2)

# view the importance of each variable
# IncMSE - The former is based upon the mean decrease of accuracy in predictions on the out of bag 
#          samples when a given variable is excluded from the model.
# IncNodePurity - The latter is a measure of the total decrease in node impurity that results from
#                 splits over that variable, averaged over all trees
importance(rf.boston)

# In the case of regression trees, the node impurity is measured by the training RSS, and for 
# classification trees by the deviance.Plots of these importance measures can be 
# produced using the varImpPlot() function. 
varImpPlot (rf.boston)

# The results indicate that across all of the trees considered in the random forest, the wealth 
# level of the community (lstat) and the house size (rm) are by far the two most important variables