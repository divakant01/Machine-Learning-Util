#'@author Divakant Pandey
#'
#'@title Non Linear Modelling
#'
#'@Description ISLR Lab Exercise
library(ISLR)
attach(Wage)

# poly() command used for higher degree, The function returns a matrix whose columns are a basis of 
# orthogonal polynomials, which essentially means that each column is a linear combination of the variables
# age, age^2, age^3 and age^4
fit=lm(wage~poly(age,4),data=Wage)

coef(summary(fit))

# Use raw polynomials and not orthogonal polynomials
fit2=lm(wage~poly(age,4,raw = T))
coef(summary(fit2))

# alternative way of fitting this model
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(summary(fit2a))

# Polynomial basis fn on the fly without wrapper fn I()
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data = Wage)
coef(summary(fit2b))

# Create grid of values of age at which we want predictions
agelims=range(age)
age.grid=seq(from=agelims [1],to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
# mar and oma for margins
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))

plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

preds2=predict(fit2 ,newdata =list(age=age.grid),se=TRUE)

max(abs(preds$fit -preds2$fit ))

# Choosing the dereee- We do this by hypothesis test, 
# Fit models ranging from linear to a degree 5 polynomial
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)

# annova which performs analysis of variance (ANNOVA using f test) in order to test the null 
# hypothesis that a model M1 is sufficient to explain the data against the alternative hypothesis
# that a more complex model is required. In order to use the anova() function, M1 and M2 must be 
# nested models: the predictors in M1 must be a subset of the predictors in M2. In this case,
# we fit five different models and sequentially compare the simpler model to the more complex model.
#
# p val of M1 and M2 is almost 0, p value of 3 and 4 is approx 5% while of degree 5 is 0.37 
# making it insignoficant hence either degree 3 or 4
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# Instead of anova we can use orthogonal polynomials
coef(summary (fit.5))


# Predicting whether an individual earns more than $250,000 per year
fit=glm(I(wage>250)~poly(age,4),data = Wage,family = binomial)
preds=predict (fit ,newdata =list(age=age.grid),se=T)
# Calculating confidence interval since glm binomial gives logit fn
pfit=exp(preds$fit )/(1+exp(preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# Note that we could have directly computed the probabilities by selecting the type="response" 
# option in the predict() function.
preds=predict (fit ,newdata =list(age=age.grid),type="response",se=T)

plot(age ,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch ="|",col="darkgrey")
lines(age.grid ,pfit ,lwd=2, col ="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

# rug plot - jitter() function to jitter the age values a bit so that observations
# jitter() with the same age value do not cover each other up.
# Cut plot - fit a step function
table(cut(age ,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary (fit))
