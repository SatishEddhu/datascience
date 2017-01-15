library(ggplot2)
#install.packages("Amelia")
library(Amelia)
library(stats)

setwd("D:/Data Science/Algorithmica/Clustering")
# social network student data
teens = read.csv("snsdata.csv", header = TRUE, na.strings=c("NA",""))

str(teens)
dim(teens)
summary(teens) # NAs in 'gender' and 'age'

# omit even if one feature is missing: many 'age' and 'gender' values missing
teens1 = na.omit(teens)
dim(teens1)
str(teens1)

# Taking subset of features
interests = teens1[5:40]

# Normalizing the variables so that distance calculation is not biased
#interests_z = as.data.frame(lapply(interests, scale))
interests_z = scale(interests)
dim(interests_z) # 24005 x 36

# Step-4: Build the model
#The high-school-age characters in general:
#a Brain, an Athlete, a Basket Case, a Princess, and a Criminal. 
set.seed(120)
# Hierarchical clustering
d = dist(interests_z) # Euclidean distance by default
# Total distances computed for 'n' points = C(n,2) = n(n-1)/2
# Here, n = 24005. So, 24005*24004/2 = 288108010 distances computed

# The methods "ward.D", "ward.D2", "complete" and "average" are used most often; default is "complete"
teen_clusters = hclust(d, method="ward.D2")
# Below plotting code should display a dendrogram but throwing a fatal error somehow
# Must see the dendrogram to decide on the 'number of clusters'
# plot(teen_clusters)

teen_clusters$height
teen_clusters$labels

# splitting into 10 clusters
cluster_cut = cutree(teen_clusters, 10)
head(cluster_cut)
str(cluster_cut)
table(cluster_cut) # see the cluster sizes


# add the 'cluster number' as an extra feature
teens1$cluster = cluster_cut

teens1[1:5, c("cluster", "gender", "age", "friends")]

# cluster-wise 'mean' values
aggregate(data = teens1, age ~ cluster, mean)
aggregate(data = teens1, friends ~ cluster, mean)

# error because 'mean' not applicable on categorical data
aggregate(data = teens1, gender ~ cluster, mean)

# works but unmeaningful statistics - "mean cluster of each gender"
aggregate(data = teens1, cluster ~ gender, mean)
