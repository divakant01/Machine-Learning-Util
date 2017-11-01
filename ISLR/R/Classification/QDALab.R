#'@author Divakant Pandey
#'
#'@title QDA
#'
#'@Description Lab Exercise
library(ISLR)
library(MASS)

attach(Smarket)

train = (Year<2005)

Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

qda.fit=qda(Direction~Lag1+Lag2,data = Smarket,subset = train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
