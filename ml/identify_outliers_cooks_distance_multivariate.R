# Identify outtliers Multivariate approach  using cooks distance 

set.seed(123)
df=data.frame(x1=rlnorm(100),x2=rlnorm(100),y=1:100)

model=lm(y~.,df)

cooksd <- cooks.distance(model)

# Plot cooks distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")

abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line

text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])

influential

df[influential,]


# Outliers Test - This gives the most extreme observation based on the given model
car::outlierTest(model)
