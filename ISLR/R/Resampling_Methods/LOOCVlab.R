#'@author Divakant Pandey
#'
#'@title Leave one out Cross Validation
#'
#'@Description Lab Exercise

# LOOCV can be automatically calculated for any generalized linear model using glm() or cv.glm() fn
# Note cv.glm does not use shortcut formula for regression.

# cv.glm  fn
library(boot)
library(ISLR)

attach(Auto)

glm.fit=glm(mpg~horsepower,data = Auto)

cv.err=cv.glm(Auto,glm.fit)

cv.err$delta

# Trying Higher Degrees
cv.err=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data = Auto)
  cv.err[i]=cv.glm(Auto,glm.fit)$delta[1]
}

# As we can see there is a large drop from linear to quardatic fit 
# but no clear improvement for using highr order polynomials
cv.err
