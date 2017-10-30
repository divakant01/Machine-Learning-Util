
# Identifying outliers from boxplot
set.seed(123)
df = data.frame(x = rlnorm(100), y=1:100)

boxplot(df$x)

outliers=boxplot(df$x,plot = FALSE)$out

# Get the labels of outliers
df[df$x %in% outliers,]
