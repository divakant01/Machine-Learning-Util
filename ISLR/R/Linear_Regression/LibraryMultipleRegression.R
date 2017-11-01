#'@author Divakant Pandey
#'
#'@title Linear Regression Library
#'
#'@Description Regression Analysis
library(MASS)
library(ISLR)

attach(Boston)

lm.fit=lm(medv~lstat+age)

summary(lm.fit)

lm.fit=lm(medv ~ .,data = Boston)
summary(lm.fit)

#R2
summary(lm.fit)$r.sq

# RSE
summary(lm.fit)$sigma

#Calculate VIF
library(car)
vif(lm.fit)

#Remove age as it has high p value
lm.fit1=lm(medv~.-age,data = Boston)
summary(lm.fit1)


#Alternative to above update can be used.
lm.fit1=update(lm.fit,~.-age)
summary(lm.fit1)

# Interaction term- Includes medv and lstat from Hierarchial principle, 
# Also we can use medv~lstat:age
summary(lm(medv~lstat*age,data = Boston))

# Non Linear transformation
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
  
# Further quantify to which Quardatic fit is superior to linear fit , Ho= 2models fits the 
# data equally well, Ha=Full model is superior, Based on the F and p value below  Model 2 is superior
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

# For higher order polynomial, lstat of degree 5
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)


#Qualitative predictor
#fix(Carseats)
attach(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

#Contrast fn return the encoding that R uses for Qualitative vavriables
contrasts(Carseats$ShelveLoc)
