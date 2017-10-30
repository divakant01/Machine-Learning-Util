# Identify Leverage points

library(car)
set.seed(123)
df=data.frame(x1=rlnorm(100),x2=rlnorm(100),y=1:100)

model=lm(y~.,df)

w<-abs(rstudent(model)) < 3 & abs(cooks.distance(model)) < 4/nrow(model$model)

lm <- update(model, weights=as.numeric(w))

leveragePlots(model)
leveragePlots(lm)

# Compare differences between old and updated model
par(mfrow=c(2,2))
plot(model)
plot(lm)
