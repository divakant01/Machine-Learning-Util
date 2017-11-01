#'@author Divakant Pandey
#'
#'@title LDA
#'
#'@Description Lab Exercise

#' lda function is in MASS
library(MASS)
library(ISLR)
attach(Smarket)

train= (Year < 2005)
Smarket.2005= Smarket[!train,]
Direction.2005=Direction[!train]

lda.fit=lda(Direction~Lag1+Lag2,data = Smarket,subset = train)
lda.fit

plot(lda.fit)


lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class,Direction.2005)

mean(lda.class==Direction.2005)

# Applying threshold to posterior recreate the prediction contained in class
sum(lda.pred$posterior[,1]>=.5)

sum(lda.pred$posterior[,1]<.5)

# Change threshold to 90%
sum(lda.pred$posterior[,1]>.9)
