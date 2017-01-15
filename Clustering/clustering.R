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

X11()
# yellow lines show the missing 'age' and 'gender' values
missmap(teens, main="Teen data from social network - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)


# omit even if one feature is missing: many 'age' and 'gender' values missing
teens1 = na.omit(teens)
dim(teens1)
str(teens1)

# Taking subset of features
interests = teens1[5:40]

# Normalizing the variables so that distance calculation is not biased
#interests_z = as.data.frame(lapply(interests, scale))
interests_z = scale(interests)

# Step-4: Build the model
#The high-school-age characters in general:
#a Brain, an Athlete, a Basket Case, a Princess, and a Criminal. 
set.seed(120)
teen_clusters = kmeans(interests_z, 3)
str(teen_clusters)

teen_clusters$size
teen_clusters$centers

teen_clusters$withinss # Cohesion within clusters
teen_clusters$tot.withinss # equal to - sum(teen_clusters$withinss)

# 'separation' between clusters
teen_clusters$betweenss

# sum of squares of distances (from the 'mean' of all points) of all points
# value obviously independent of 'k' (the #clusters)
teen_clusters$totss

# add the 'cluster number' as an extra feature
teens1$cluster = teen_clusters$cluster

teens1[1:5, c("cluster", "gender", "age", "friends")]

# cluster-wise 'mean' values
aggregate(data = teens1, age ~ cluster, mean)
aggregate(data = teens1, friends ~ cluster, mean)

# error because 'mean' not applicable on categorical data
aggregate(data = teens1, gender ~ cluster, mean)

# works but unmeaningful statistics - "mean cluster of each gender"
aggregate(data = teens1, cluster ~ gender, mean)