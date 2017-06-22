#'@author Divakant Pandey
#'
#'@title K Fold CV
#'
#'@Description Lab Exercise

# We can use cv.glm to implement k fold as well, just pass arguement K

library(ISLR)
library(boot)

set.seed(17)
cv.err.10=rep(0,10)

for(i in 1:10){
   glm.fit=glm(mpg~poly(horsepower,i),data = Auto)
   cv.err.10[i]=cv.glm(Auto,glm.fit,K = 10)$delta[1]
}

cv.err.10