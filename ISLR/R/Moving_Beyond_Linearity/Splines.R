#'@author Divakant Pandey
#'
#'@title Non Linear Modelling
#'
#'@Description ISLR Lab Exercise
library(ISLR)
attach(Wage)

# Regression Splines
library(splines)

agelims=range(age)
age.grid=seq(from=agelims [1],to=agelims [2])

# Regression splines can be fit by constructing an appropriate matrix of basis functions. The 
# bs() function generates the entire matrix of basis functions for splines with the specified set 
# of knots. By default, cubic splines are produced.
fit=lm(wage~bs(age,knots = c(25,40,60)),data = Wage)
pred=predict (fit ,newdata =list(age=age.grid),se=T)
plot(age ,wage ,col="gray")
lines(age.grid ,pred$fit ,lwd=2)
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

# Produce a spline with knots at uniform quantiles of the data.
dim(bs(age ,df = 6 ))
attr(bs(age ,df=6) ,"knots")

# using degree arg in bs we can fit splines of any degree rather than default of cubic splines

# Fit a natural Spline using ns fn
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata = list(age=age.grid),se=T)
lines(age.grid , pred2$fit ,col="red",lwd=2)

# Smoothig Spline
plot(age ,wage ,xlim=agelims ,cex =.5,col="darkgrey")
title("Smoothing Spline ")
# For smoothing spline
fit=smooth.spline(age ,wage ,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit ,col="red",lwd =2)
lines(fit2 ,col="blue",lwd=2)
legend ("topright",legend=c("16 DF" ,"6.8 DF"),col=c("red","blue"),lty=1,lwd=2, cex =.8)


# Local Regression
#  Here we perform spans of 0.2 and 0.5: that is, each neighborhood consists of 20 % or 50 % of the observations
plot(age ,wage ,xlim=agelims ,cex =.5,col="darkgrey")
title("Local Regression ")
fit=loess(wage~age ,span=.2,data=Wage)
fit2=loess(wage~age ,span=.5,data=Wage)
lines(age.grid ,predict (fit ,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"),col=c("red","blue"),lty=1,lwd=2, cex =.8)

#  GAM

#  Fit a GAM to predict wage using natural spline functions of year and age, treating education 
#  as a qualitative predictor.  Since this is just a big linear regression model using an appropriate choice of
#  basis functions, we can simply do this using the lm() function.
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data = Wage)

# Instead of natural splines we will use smoothing splines, In order to fit more general sorts of 
# GAMs, using smoothing splines or other components that cannot be expressed in terms of basis 
# functions and then fit using least squares regression, we will need to use the gam library in R
library(gam)

# The s() function, which is part of the gam library, is used to indicate that we would like 
# to use a smoothing spline.
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))

# Internally plot.gam is called
#plot(gam.m3, se=TRUE ,col ="blue")

plot.gam(gam1 , se=TRUE , col="red")

# We can perform a series of ANOVA tests in order to determine which of these three models is
# best: a GAM that excludes year (M1), a GAM that uses a linear function of year (M2), or a GAM 
# that uses a spline function of year (M3)
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

summary(gam.m3)

preds=predict (gam.m2,newdata =Wage)

# Using local regerssion with GAM using lo() fn
gam.lo=gam(wage~s(year ,df=4)+lo(age ,span =0.7)+education ,data=Wage)
#plot.gam(gam.lo, se=TRUE , col ="green")

# We can also use the lo() function to create interactions before calling the gam() function.
gam.lo.i=gam(wage~lo(year ,age , span=0.5)+education,data=Wage)


library(akima)
#plot(gam.lo.i)

# GAM for Logistic Regression
# In order to fit a logistic regression GAM, we once again use the I() function in constructing the
# binary response variable, and set family=binomial.
gam.lr=gam(I(wage >250)~year+s(age ,df=5)+education,family=binomial ,data=Wage)
par(mfrow=c(1,3))
#plot(gam.lr,se=T,col="green")

# It is easy to see that there are no high earners in the <HS category:
table(education ,I(wage >250))

# Hence, we fit a logistic regression GAM using all but this category. This provides more sensible  results
gam.lr.s=gam(I(wage >250)~year+s(age ,df=5)+education ,family=binomial ,data=Wage , 
             subset =(education !="1. < HS Grad"))
#plot(gam.lr.s,se=T,col="green")
