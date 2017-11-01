#'@author Divakant Pandey
#'
#'@title Classiication Tree
#'
#'@Description ISLR Lab Exercise
library(ISLR)
attach(Carseats)

# tree library is used to construct classification and regression trees
library(tree)

# Create High var from continous Sales var
High=ifelse(Sales <=8,"No","Yes")

# Merge High with Carseats data
Carseats =data.frame(Carseats ,High)

# tree function to fit a classification tree in order to predict High using all variables but Sales
tree.carseats=tree(High~.-Sales,Carseats)
# Training Error rate of 9%
# The residual mean deviance reported is simply the deviance divided by n−|T0|,
# which in this case is 400−27 = 373
summary(tree.carseats)

# plot() function to display the tree structure,and the text() function to display the node labels. 
# The argument pretty=0 instructs R to include the category names for any qualitative predictors,
# rather than simply displaying a letter for each category
plot(tree.carseats)
text(tree.carseats ,pretty =0)

# Check the rules
tree.carseats

#  Evaluate the performance of a classification tree
set.seed(2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train]

tree.carseats =tree(High~.-Sales , Carseats ,subset=train)
tree.pred=predict(tree.carseats ,Carseats.test ,type="class")
table(tree.pred ,High.test)

# Prediction accuracy - 71.5% correct predictions
(86+57)/200

# Next, we consider whether pruning the tree might lead to improved results. The function cv.tree() 
# performs cross-validation in order to determine the optimal level of tree complexity
# cost complexity pruning is used in order to select a sequence of trees for consideration.
# FUN=prune.misclass in order to indicate that we want the classification error rate to guide the 
# cross-validation and pruning process rather than the default for the cv.tree fn which is deviance
# The cv.tree fn reports the number of terminal nodes of each tree considered (size) as well as 
# the corresponding error rate and the value of the cost-complexity parameter used 
# (k, which corresponds to α)
set.seed(3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats)
# The tree with 9 leaf nodes results in the lowest cv error rate, with 50 cross-validation errors.
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

# apply the prune.misclass() function in order to prune the tree to obtain the nine-node tree.
prune.carseats =prune.misclass (tree.carseats ,best=9)
plot(prune.carseats )
text(prune.carseats ,pretty =0)

# Compute Prediction Accuracy
tree.pred=predict(prune.carseats ,Carseats.test , type="class")
table(tree.pred ,High.test)

# Accuracy
(94+60) /200

# Try increasing best value , Lead to decrease the prediction accuracy
prune.carseats =prune.misclass (tree.carseats ,best=15)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict(prune.carseats ,Carseats.test , type="class")
table(tree.pred ,High.test)
(86+62) /200
 