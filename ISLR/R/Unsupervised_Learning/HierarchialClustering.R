#'@author Divakant Pandey
#'
#'@title Hierarchial Clustering
#'
#'@Description ISLR Lab Exercise

# The hclust() function implements hierarchical clustering in R.
set.seed(2)
x=matrix(rnorm (50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# plot the hierarchical clustering dendrogram using complete, single, and average linkage clustering,
# with Euclidean distance as the dissimilarity measure.

# Begin with using complete linkage. The dist() function is used to compute the 50 × 50 
# inter-observation Euclidean distance matrix
hc.complete =hclust(dist(x), method="complete")
hc.average =hclust(dist(x), method="average")
hc.single =hclust(dist(x), method="single")

# Plot Dendograms
par(mfrow=c(1,3))
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="",cex=.9)
plot(hc.average , main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single , main="Single Linkage ", xlab="", sub="",  cex=.9)

# To determine the cluster labels for each observation associated with a given cut of the dendrogram,
# we can use the cutree() function:
cutree(hc.complete , 2)
cutree(hc.average , 2)
cutree(hc.single , 2)

# For this data, complete and average linkage generally separate the observations into their correct 
# groups. However, single linkage identifies one point as belonging to its own cluster.
# A more sensible answer is obtained when four clusters are selected, although there are still two 
# singletons.
cutree(hc.single , 4)

# To scale the variables before performing hierarchical clustering of the observations, 
xsc=scale(x)
plot(hclust(dist(xsc), method ="complete"), main=" HierarchicalClustering with Scaled Features ")

# Correlation-based distance can be computed using the as.dist() func, which converts an arbitrary 
# square symmetric matrix into a form that the hclust() function recognizes as a distance matrix.
#However, this only makes sense for data with at least three features since the absolute correlation
# between any two observations with measurements on two features isalways 1. 
# Hence, we will cluster a 3d data set
x=matrix(rnorm (30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method ="complete"), main=" Complete Linkage with Correlation -Based Distance ", xlab="", sub ="")
