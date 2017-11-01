#'@author Divakant Pandey
#'
#'@title NC160 Data Example
#'
#'@Description ISLR Lab Exercise

# Unsupervised techniques are often used in the analysis of genomic data. In particular, PCA and 
# hierarchical clustering are popular tools.We illustrate these techniques on the NCI60 cancer cell 
# line microarray data, which consists of 6,830 gene expression measurements on 64 cancer cell lines.
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

#Each cell line is labeled with a cancer type. We do not make use of the cancer types in performing 
# PCA and clustering, as these are unsupervised techniques. But after performing PCA and clustering,
# we will check to see the extent to which these cancer types agree with the results of these unsupervised techniques.

#The data has 64 rows and 6,830 columns.
dim(nci.data)

#We begin by examining the cancer types for the cell lines.
nci.labs[1:4]
table(nci.labs)
nci.labs

# PCA on NCI60 data
# We first perform PCA on the data after scaling the variables (genes) to have standard deviation one,
# although one could reasonably argue that it is better not to scale the genes.
pr.out=prcomp(nci.data , scale=TRUE)

# We now plot the first few principal component score vectors, in order to visualize the data
# The observations (cell lines) corresponding to a given cancer type will be plotted in the same color, so that we can see to what
# extent the observations within a cancer type are similar to each other.

#We first create a simple function that assigns a distinct color to each element of a numeric vector. 
# The function will be used to assign a color to each of the 64 cell lines, based on the cancer type 
# to which it corresponds.
# rainbow func takes as its argument a +ve int,and returns a vector containing that no of distinct colors.
Cols=function (vec){
   cols=rainbow (length(unique(vec)))
   return(cols[as.numeric (as.factor(vec))])
}

# Plot Principle component score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")

# On the whole, cell lines corresponding to a single cancer type do tend to have similar values on the
# first few principal component score vectors. This indicates that cell lines from the same cancer
# type tend to have pretty similar gene expression levels.

#We can obtain a summary of the proportion of variance explained (PVE) of the first few principal 
# components using the summary() method for a prcomp object 
summary (pr.out)

# Using the plot fn, we can also plot the variance explained by the first few principal components.
# Note that the height of each bar in the bar plot is given by squaring the corresponding element of 
# pr.out$sdev.
plot(pr.out)

# plot the PVE of each principal component (i.e. a scree plot) and the cumulative PVE of each principal component.
# Note that the elements of pve can also be computed directly from the summary, 
# summary(pr.out)$importance[2,], and the elements of cumsum(pve) are given by 
# summary(pr.out)$importance[3,]
# the first seven principal components explain around 40 % of the variance in the data.
#  there is an elbow in the plot after approximately the seventh principal component. This suggests 
# that there may be little benefit to examining more than seven or so principal components
pve =100*pr.out$sdev ^2/sum(pr.out$sdev ^2)
par(mfrow=c(1,2))
plot(pve , type="o", ylab="PVE", xlab=" Principal Component ",     col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component ", col="brown3")

# Clustering on NCI60
# Perform Hierarchial

# Scaling - Optional
sd.data=scale(nci.data)

# We now perform hierarchical clustering of the observations using complete, single, and average 
# linkage. Euclidean distance is used as the dissimilarity measure.
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs , main="Complete Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="average"), labels=nci.labs,main="Average Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="single"), labels=nci.labs,main="Single Linkage ", xlab="", sub="",ylab="")

# We see that the choice of linkage certainly does affect the results obtained. Typically, single 
# linkage will tend to yield trailing clusters: very large clusters onto which individual obs
# attach one-by-one. On the other hand, complete and average linkage tend to yield more balanced, 
# attractive clusters. For this reason, complete and average linkage are generally preferred to 
# single linkage. Clearly cell lines within a single cancer type do tend to cluster together, 
# although the clustering is not perfect. 

# We will use complete linkage hierarchical clustering for the analysis that follows.
# We can cut the dendrogram at the height that will yield a particular number of clusters, say four:
# There are some clear patterns. All the leukemia cell lines fall in cluster 3, while the breast 
# cancer cell lines are spread out over three different clusters.
hc.out=hclust(dist(sd.data))
hc.clusters =cutree (hc.out ,4)
table(hc.clusters ,nci.labs)

# Plot the cut on the dendrogram that produces these four clusters:abline() function draws a 
# straight line on top of any existing plot in R. The argument h=139 plots a horizontal line at
# height 139 on the dendrogram; this is the height that results in four distinct clusters. It is 
# easy to verify that the resulting clusters are the same as the ones we obtained using cutree(hc.out,4). 
par(mfrow=c(1,1))
plot(hc.out , labels =nci.labs)
abline(h=139, col="red")

# Printing the output of hclust gives a useful brief summary of the object:
hc.out

# Compare NCI60 hierarchical clustering results compare to what we get if we perform K-means 
# clustering with K = 4?
set.seed(2)
km.out=kmeans(sd.data , 4, nstart =20)
km.clusters =km.out$cluster

#We see that the four clusters obtained using hierarchical clustering and Kmeans clustering are 
# somewhat different. Cluster 2 in K-means clustering is identical to cluster 3 in hierarchical 
#clustering. However, the other clusters differ: for instance, cluster 4 in K-means clustering 
# contains a portion of the observations assigned to cluster 1 by hierarchical clustering, as well as
# all of the observations assigned to cluster 2 by hierarchical clustering.
table(km.clusters ,hc.clusters )

# Rather than performing hierarchical clustering on the entire data matrix, we can simply perform 
# hierarchical clustering on the first few principal component score vectors, as follows:
hc.out=hclust(dist(pr.out$x [,1:5]) )
plot(hc.out , labels =nci.labs , main="Hier. Clust. on First Five Score Vectors ")
table(cutree (hc.out ,4), nci.labs)

# Not surprisingly, these results are different from the ones that we obtained when we performed hierarchical clustering on the full data set.
#  Sometimes performing clustering on the first few principal component score vectors can give better results than performing clustering on the full data.
# In this situation, we might view the principal component step as one of denoising the data.
# We could also perform K-means clustering on the first few principal component score vectors rather than the full data set.