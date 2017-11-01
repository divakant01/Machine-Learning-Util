#'@author Divakant Pandey
#'
#'@title K Means Clustering
#'
#'@Description ISLR Lab Exercise

# The function kmeans() performs K-means clustering in R
#  simulated example in which there truly are two clusters in the data: the first 25 observations have 
# a mean shift relative to the next 25 observations
set.seed(2)
x=matrix(rnorm (50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# We now perform K-means clustering with K = 2
km.out=kmeans (x,2, nstart =20)

# The cluster assignments of the 50 observations are contained in
km.out$cluster

# We can plot the data, with each observation colored according to its cluster assignment.
plot(x, col=(km.out$cluster +1), main="K-Means Clustering
Results with K=2", xlab="", ylab="", pch=20, cex=2)

# Here the observations can be easily plotted because they are two-dimensional. If there were more 
#than two variables then we could instead perform PCA and plot the first two principal components
# score vectors.

# In this ex, clusters are known but for real world it is not known, lets try K=3
# To run the kmeans() function in R with multiple initial cluster assignments, we use the nstart arg.
# If a value of nstart greater than one is used, then K-means clustering will be performed using
# multiple random assignments in Step 1 of Algorithm 10.1, and the kmeans() function will
# report only the best results.
set.seed(4)
km.out=kmeans (x,3, nstart =20)
km.out

#  Here we compare using nstart=1 to nstart=20. Note that km.out$tot.withinss is the total within-
# cluster sum of squares,which we seek to minimize by performing K-means clustering
set.seed(3)
km.out=kmeans (x,3, nstart =1)
km.out$tot.withinss
km.out=kmeans (x,3, nstart =20)
km.out$tot.withinss

#We strongly recommend always running K-means clustering with a large value of nstart, such as 20 or
#50, since otherwise an undesirable local optimum may be obtained.It is also important to set a
# random seed using the set.seed() function. This way, the initial cluster assignments in Step 1
# can be replicated, and the K-means output will be fully reproducible