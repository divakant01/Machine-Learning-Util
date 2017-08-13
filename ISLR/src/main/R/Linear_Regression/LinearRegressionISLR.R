#'@author Divakant Pandey
#'
#'@title Linear Regression Advertisin Data
#'
#'@Description Regression Analysis on Advertising data
#'
library(ggplot2)
library(data.table)

advertising <- fread("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")

ggplot(advertising,aes(x=TV,y=Sales))+geom_point()+geom_smooth(method = lm)

fit=lm(Sales~TV+Radio+Newspaper,advertising)

summary(fit)

# 1. Larger F stats clearly indicates relationship bw Sales and advertising.
#'2.  How strong is the relationship - From R2 predictors explains almost 89% of Sales, 
#'3.  RSE is 1.686 as compared t mean sales of 14 -> Error = 1.686/14.02*100=12%
#'4.  Which media contribute to sales - Observe p values associated with each predictors t statistics
#'5.  How large is the effect of each medium on sales - Confidence Interval for TV and Radio is far 
#' from 0 while newspaper includes zero, Hence Newspaper isnot statistically significant.
#' See the confidence interval calculation for all variabel given below
confint(fit,level = 0.95)

#' Colinearity can result in very wide standard error, 
#' Could that be the reason for wide confidence interval of NEwspaper - Below values suggest
#'  no evidence of colinearity
library(fmsb)
VIF(lm(Sales~TV,advertising))
VIF(lm(Sales~Radio,advertising))
VIF(lm(Sales~Newspaper,advertising))

#' 6. How accurately can we repdict the future sales - it dpends on whether we wish to predict
#'  Individual response(f(x)+e) use prediction interval or average response f(x) use confidence interval

#' Prediction Interval - lwr and upr values
d=head(advertising,n = 5)
d$Sales=NULL
predict(fit,newdata = d,interval ="predict" )

#' 7. Is the relationship linear - Residual plot (If linear , no pattern) - In Simple regression 
#' Residuals vs Xj is plotted, In multiple Residuals vs Predicted values. Below graph show a pattern.
ggplot(advertising,aes(x=fit$fitted.values,y=fit$residuals))+geom_point()+geom_smooth(method = "auto")

#' Is there synergy among advertising media - Interaction effect - TV*Radio, Check RSE, R2, F stats val diff
fit2=lm(Sales~TV+Radio,advertising)
fit3=lm(Sales~TV+Radio+TV*Radio,advertising)
summary(fit2)
summary(fit3)

ggplot(advertising,aes(x=fit3$fitted.values,y=fit3$residuals))+geom_point()+geom_smooth(method = "auto")
