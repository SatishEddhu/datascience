# Problem 1: K-Means clustering calculations
x = c(2,2,8,5,7,6,1,4)
y = c(10,5,4,8,5,4,2,9)
len = length(x)

dist = function(x1,y1,x2,y2) {
  return (sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

distance = matrix(nrow = len, ncol = len)

for (i in 1:len) {
  for (j in 1:len) {
    distance[i,j] = dist(x[i],y[i],x[j],y[j])
  }
}


mean(c(x[3],x[5],x[6]))
mean(c(y[3],y[5],y[6]))

mean(c(x[1],x[4],x[8]))
mean(c(y[1],y[4],y[8]))

mean(c(x[2],x[7]))
mean(c(y[2],y[7]))

x = c(2,2,8,5,7,6,1,4,2,1.5,6)
y = c(10,5,4,8,5,4,2,9,10,3.5,6)

distance[1:8,9:11]

x = c(2,2,8,5,7,6,1,4,3,1.5,6.5)
y = c(10,5,4,8,5,4,2,9,9.5,3.5,5.25)

x = c(2,2,8,5,7,6,1,4,3.66,1.5,7)
y = c(10,5,4,8,5,4,2,9,9,3.5,4.33)

distance[1:8,9:11]

# Problem 2: K-Medoids clustering calculations
distance[,c(1,4,7)]

distance[c(3,4,5,6,8),c(3,4,5,6,8)]
rowSums(distance[c(3,4,5,6,8),c(3,4,5,6,8)],1)

distance[,c(1,5,7)]

distance[c(3,4,5,6),c(3,4,5,6)]
rowSums(distance[c(3,4,5,6),c(3,4,5,6)],1)

# Problem 3: Hierarchical clustering
library(ggplot2)
library(Amelia)
library(stats)

x = c(2,2,8,5,7,6,1,4)
y = c(10,5,4,8,5,4,2,9)

numbers = data.frame(x,y)
row.names(numbers) = paste0("A",1:8)

num_dist = dist(numbers)

clusters_single = hclust(num_dist, method="single")
clusters_single$height
plot(clusters_single, main = "Single-link dendrogram")

clusters_complete = hclust(num_dist, method="complete")
clusters_complete$height
plot(clusters_complete, main = "Complete-link dendrogram")

clusters_average = hclust(num_dist, method="average")
clusters_average$height
plot(clusters_average, main = "Average-link dendrogram")

clusters_centroid = hclust(num_dist, method="centroid")
clusters_centroid$height
plot(clusters_centroid, main = "Centroid dendrogram")

clusters_median = hclust(num_dist, method="median")
plot(clusters_median, main = "Median dendrogram")

distance[1,4]
distance[1,8]
mean(c(distance[1,4],distance[1,8]))

mean(c(distance[2,3],distance[2,5],distance[2,6],distance[7,3],distance[7,5],distance[7,6]))
mean(c(distance[2,1],distance[2,4],distance[2,8],distance[7,1],distance[7,4],distance[7,8]))
mean(c(distance[6,1],distance[6,4],distance[6,8],distance[3,1],distance[3,4],distance[3,8],distance[5,1],distance[5,4],distance[5,8]))
mean(c(distance[1,2],distance[1,3],distance[1,5],distance[1,6],distance[1,7],
       distance[4,2],distance[4,3],distance[4,5],distance[4,6],distance[4,7],
       distance[8,2],distance[8,3],distance[8,5],distance[8,6],distance[8,7]))

dist(6,4,7,4.33)
dist(6,4,7.5,4.5)
dist(2,10,4.5,8.5)
mean(c(distance[1,4],distance[1,8],distance[4,8]))

# Problem 4: Density based DBSCAN clustering
#install.packages("dbscan")
library(dbscan)

x = c(2,2,8,5,7,6,1,4)
y = c(10,5,4,8,5,4,2,9)
m = matrix(c(x,y),ncol=2,byrow = F)

result1 = dbscan(m, eps = 2, minPts = 2)
result1
# 2 clusters and 3 noise points
result1$cluster
# 0 0 1 2 1 1 0 2 => A1,A2,A7 are noise points; {A3,A5,A6} = cluster 1; {A4,A8} = cluster 2

result2 = dbscan(m, eps = sqrt(10),minPts = 2)
result2
# 3 clusters and 0 noise points
result2$cluster
# 1 2 3 1 3 3 2 1 => {A1,A4,A8} = cluster 1; {A2,A7} = cluster 2; {A3,A5,A6} = cluster 3