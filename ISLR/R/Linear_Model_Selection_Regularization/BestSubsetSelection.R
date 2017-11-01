#'@author Divakant Pandey
#'
#'@title Best Subset selection
#'
#'@Description Lab Exercise
library(ISLR)
attach(Hitters)
fix(Hitters)

dim(Hitters)

#Check NA values for salary var
sum(is.na(Hitters$Salary))

# Remove NA
Hitters=na.omit(Hitters)

# regsubsets()performs best subset selection
library(leaps)

# Displays top 8 Models by default, Note that 1st 2 contains only Hits and CRBI
regfit.full=regsubsets(Salary~.,Hitters)

summary(regfit.full)

# Displays top 20 Models, Use nvmax option
regfit.full=regsubsets(Salary~.,Hitters,nvmax = 20)


# summary function also returns R2, adjusted R2, RSs, Cp, BIC. Check available options
reg.summary=summary(regfit.full)
names(reg.summary)

reg.summary$rsq

# Plotting RSS, adjusted R2, Cp and BIC together will help us decide which model to select.
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab = "No of Variables",ylab = "RSS", type = "l")
plot(reg.summary$adjr2,xlab = "No of Variables", ylab = "Adjusted R2",type = "l")

# Find the location of maximum point of vector
m=which.max(reg.summary$adjr2)

# points() plot on the existing plot instead of creating a new one.
points(m,reg.summary$adjr2[m],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab = "No of Variables",ylab = "Cp",type="l")

minCp=which.min(reg.summary$cp)
points(minCp,reg.summary$cp[minCp],col="red",cex=2,pch=20)

bicMin=which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = "No of Variable",ylab = "BIC",type = "l")
points(bicMin,reg.summary$bic[bicMin],col="red",cex=2,pch=20)


# Note the regsubsets fn has built in plot command which can be used to display the above variables to
# select the best model.
?plot.regsubsets
par(mfrow=c(2,2))

# Note that top row contains the black square for each var selected ac to the optimal model associated
# with the statistics, For ex check BIC value for -150
plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")

# see the coefficient estimate associated with this model (BIC value for -150) 6 variables are used
coef(regfit.full,6)
