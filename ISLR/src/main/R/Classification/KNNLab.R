#'@author Divakant Pandey
#'
#'@title KNN
#'
#'@Description Lab Exercise

library(ISLR)
library(class)
attach(Smarket)

train.x=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
Direction.2005=Direction[!train]
train.Direction=Direction[train]

set.seed(1)

# K=1
knn.pred=knn(train.x,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
mean(knn.pred==Direction.2005)

# K=3
knn.pred=knn(train.x,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
