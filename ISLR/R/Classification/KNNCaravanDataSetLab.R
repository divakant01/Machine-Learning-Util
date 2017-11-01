#'@author Divakant Pandey
#'
#'@title KNN
#'
#'@Description Caravan Dataset

library(ISLR)
attach(Caravan)

dim(Caravan)
summary(Purchase)

# Percent of Insurance purchased
348/(348+5474)

# Standardize the data to make each predictor on same scale (ie sd=1, mean=0 for every column)
standardized.X=scale(Caravan[,-86]) # Exclude Qualitative variable Purchase
var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,1])

# Split Data
test=1:1000
train.X=standardized.X[-test,]
test.x=standardized.X[test,]

train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)

knn.pred=knn(train.X,test.x,train.Y,k=1)

mean(test.Y!=knn.pred)

mean(test.Y!="No")
     
table(knn.pred,test.Y)

# Success Rate
9/(68+9)

# K=3
knn.pred=knn(train.X,test.x,train.Y,k=3)
table(knn.pred,test.Y)
# Success Rate
5/(5+21)

# K=5
knn.pred=knn(train.X,test.x,train.Y,k=5)
table(knn.pred,test.Y)
# Success Rate
4/(4+11)


# Logistic Regression
glm.fit=glm(Purchase~.,data = Caravan,family = binomial,subset = -test)
glm.probs=predict(glm.fit,Caravan[test,],type = "response")

glm.pred=rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test.Y)


glm.pred=rep("No",1000)
glm.pred[glm.probs>0.25]="Yes"
table(glm.pred,test.Y)
11/(11+22)
