#'@author Divakant Pandey
#'
#'@title PCA
#'
#'@Description ISLR Lab Exercise
summary(USArrests)

# The rows of the data set contain the 50 states, in alphabetical order.
states=row.names(USArrests)
states

# The columns of the data set contain the four variables.
names(USArrests)

# Examine data - We notice that the variables have vastly different means.,The second
# input here denotes whether we wish to compute the mean of the rows, 1,or the columns, 2.
apply(USArrests , 2, mean)

# Examine Variance
apply(USArrests , 2, var)

# If we failed to scale the variables before performing PCA, then most of the principal components
# that we observed would be driven by the Assault variable, since it has by far the largest mean 
# and variance. Thus, it is important to standardize the variables to have mean zero and 
# standard deviation one before performing PCA.

# We now perform principal components analysis using the prcomp() function, which is one of several
# functions in R that perform PCA.By default, the prcomp() function centers the variables to have 
# mean zero. By using the option scale=TRUE, we scale the variables to have standard deviation one.
pr.out=prcomp(USArrests , scale=TRUE)
names(pr.out)

# The center and scale components correspond to the means and standard deviations of the variables 
# that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings; each column of pr.out$rotation 
# contains the corresponding principal component loading vector. We see that there are four distinct
# principal components. This is to be expected because there are in general min(n − 1, p) informative 
# principal components in a data set with n observations and p variables.
# This function names it the rotation matrix, because when we matrix-multiply the X matrix by 
# pr.out$rotation, it gives us the coordinates of the data in the rotated coordinate system. 
# These coordinates are the principal component scores.
pr.out$rotation

# Using the prcomp() function, we do not need to explicitly multiply the data by the principal 
# component loading vectors in order to obtain the principal component score vectors. Rather the 
# 50 × 4 matrix x has as its columns the principal component score vectors. That is, the kth column 
# is the kth principal component score vector.
dim(pr.out$x)
# The scale=0 argument to biplot() ensures that the arrows are scaled to biplot() represent the 
# loadings; other values for scale give slightly different biplots with different interpretations
biplot (pr.out , scale =0)

# Changing sign to obtain a fig similar to chapter
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

#The prcomp() function also outputs the standard deviation of each principal component. 
pr.out$sdev

# Variance for these components
pr.var=pr.out$sdev ^2
pr.var

# To compute the proportion of variance explained by each principal component,we simply divide the 
# variance explained by each principal component by the total variance explained by all four 
# principal components:
pve=pr.var/sum(pr.var)

# 1st pc explains 62%, 2nd PC - 24% and 8 and 4 % for 3rd and 4th PC
pve

# We can plot the PVE explained by each component, as well as the cumulative PVE, as follows:
plot(pve , xlab=" Principal Component ", ylab="Proportion ofVariance Explained ", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab="
Cumulative Proportion of Variance Explained ", ylim=c(0,1),
     type='b')

# Note that the function cumsum() computes the cumulative sum of the elements of a numeric vector.
a=c(1,2,8,-3)
cumsum(a)
