#'@author Divakant Pandey
#'
#'@title Logistic Regression
#'
#'@Description Lab Exercise

library(ISLR)

names(Smarket)
summary(Smarket)
pairs(Smarket)

#Compute Correlation : Note cor(Smarket) will throw error coz Direction var is qualitative,
#Hence remove 9th col
cor(Smarket[, -9])

attach(Smarket)

plot(Volume)

# glm is the generalised linear model contains class of model.
# Logistic Regression - family=binomial
glm.fit <-
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
      data = Smarket,
      family = binomial)
summary(glm.fit)
summary(glm.fit)$coef[, 4]

par(mfrow=c(2,2))

# 1.Residuals vs Fitted - It is a scatter plot of residuals on the y axis and fitted values 
#    (estimated responses) on the x axis. The plot is used to detect non-linearity, unequal error 
#     variances, and outliers. Points for which reiduals ~ 0 is correctly predicted.This plot shows if residuals have non-linear patterns. There could be a non-linear relationship between predictor variables and an outcome variable and the pattern could show up in this plot if the model doesn’t capture the non-linear relationship. If you find equally spread residuals around a horizontal line without distinct patterns, that is a good indication you don’t have non-linear relationships.
# 2.Q-Q or Quantile-Quantile plot The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess if a set 
# of data plausibly came from some theoretical distribution such as a Normal or exponential.A roughly straight
# line indicate the same distriubtion. Plotting two sets of quantiles against one another. Now what 
# are “quantiles”? These are often referred to as “percentiles”. These are points in your data below 
# which a certain proportion of your data fall.This plot shows if residuals are normally distributed. Do residuals follow a straight line well or do they deviate severely? It’s good if residuals are lined well on the straight dashed line.
# 3.Scale - Location -It’s also called Spread-Location plot. This plot shows if residuals are spread equally along the ranges of predictors. This is how you can check the assumption of equal variance (homoscedasticity). It’s good if you see a horizontal line with equally (randomly) spread points.
# 4.Residuals vs Leverage - Unlike the other plots, this time patterns are not relevant. We watch out for outlying values at the upper right corner or at the lower right corner. Those spots are the places where cases can be influential against a regression line. Look for cases outside of a dashed line, Cook’s distance. When cases are outside of the Cook’s distance (meaning they have high Cook’s distance scores), the cases are influential to the regression results. The regression results will be altered if we exclude those cases.


plot(glm.fit)

# Predict - Note type ="response" print the output in the form of P(Y = 1|X),
# Rather than printing LOGIT
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]

#This will show the dummy variable R has created to compare the above predicted values
contrasts(Direction)

# Create sample vector of "Down" equal to size of Smarket
glm.pred = rep("Down", 1250)

# Transform to "Up" for which predicted probability of a market increased 0.5
glm.pred[glm.probs > 0.5] = "Up"

# Create Confusion matrix - table() method
table(glm.pred, Direction)

# 52.16% time the model correctly predicted
(145 + 507) / 1250 * 100.0

# mean func can be used to compute fraction of days for which prediction was correct
mean(glm.pred == Direction)

#' Above model is misleading coz we trained and tes the model on same data set ie 100-52.16=47.8%
#' is the training error rate, which is often overly optimistic, it tends to underestimate
#' the test error rate.

#' Segregate data from 2001 to 2004 and then we will create set for 2005
train <- Year < 2005

# Get Ouput with 2005 dates only
Smarket.2005 <- Smarket[!train, ]
Direction.2005 = Direction[!train]


# we now create a model with dates from 2001 to 2004
glm.fit = glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
   data = Smarket,
  family = binomial,
  subset = train
)

# Predict 2005 probabilities
glm.probs=predict(glm.fit,Smarket.2005,type = "response")

# 2005 Prediction 
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"

# Confusion matrix
table(glm.pred,Direction.2005)

#Accuracy
mean(glm.pred==Direction.2005)

# Error rate
mean(glm.pred!=Direction.2005)

# Remove preditor with higher p values
glm.fit=glm(Direction~Lag1+Lag2,data = Smarket,family = binomial,subset = train)
glm.probs=predict(glm.fit,Smarket.2005,type = "response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

# Predict Particular value
predict(glm.fit,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type = "response")
