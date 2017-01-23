library(recommenderlab)
library(ggplot2)
library(reshape2)

#ls(pos="package:recommenderlab")

setwd("D:\\Data Science\\Algorithmica\\Assignments\\Recommenders")
ratings_train = read.csv("userItemRating.csv")
dim(ratings_train)
str(ratings_train)

ratings_train1 = acast(ratings_train, user ~ item)
dim(ratings_train1)
class(ratings_train1)
row.names(ratings_train1) = c("user1","user2","user3","user4","user5")
colnames(ratings_train1) = c("item1","item2","item3","item4","item5","item6")
ratings_train1

ratings_train2 = as(ratings_train1, "realRatingMatrix")
dim(ratings_train2)
class(ratings_train2)
image(ratings_train2, main = "Raw Ratings") 

# We will initially solve the 2nd problem on IBCF and then the 1st problem
# on UBCF for reasons that will become clear as we read along.


# -------------------------------------------------------
# Problem 2: Item-Item nearest neighbor based recommender
# -------------------------------------------------------

# a. Compute the item-item similarity matrix for all 6 items. Use the Euclidian Similarity metric.

ibcf_model=Recommender(ratings_train2,method="IBCF", param=list(method="Euclidean",k=3,minRating=1,normalize=NULL,alpha=0.5))
str(ibcf_model)
model_details = getModel(ibcf_model)

# check out the default parameter values for IBCF
recommenderRegistry$get_entry_names()
recommenderRegistry$get_entry("IBCF", dataType = "realRatingMatrix")


ratings_train1
# Given rating matrix (5 rows x 6 columns)
# ----------------------------------------
#         item1 item2 item3 item4 item5 item6
# user1     4     6    NA     5     1    NA
# user2     2    NA     8    NA     3     5
# user3     7     4    NA     8    NA     4
# user4    NA    10     9    NA     7     8
# user5    NA    NA     7     6     6     5

# Compute Euclidean distances between columns
dist(ratings_train1, diag=TRUE, upper=TRUE, method="Euclidean", by_rows=FALSE)
# Euclidean distances between all pairs of columns
# ------------------------------------------------
#         item1     item2     item3     item4     item5     item6
# item1  0.000000  5.700877 13.416408  2.236068  5.000000  6.708204
# item2  5.700877  0.000000  2.236068  6.519202  9.219544  3.162278
# item3 13.416408  2.236068  0.000000  2.236068  7.071068  4.830459
# item4  2.236068  6.519202  2.236068  0.000000  6.324555  6.519202
# item5  5.000000  9.219544  7.071068  6.324555  0.000000  3.162278
# item6  6.708204  3.162278  4.830459  6.519202  3.162278  0.000000

# Missing values are allowed but are excluded from all computations involving the rows within which they occur. 
# If some rows are excluded in calculating a Euclidean, Manhattan, Canberra or Minkowski distance, the sum 
# is scaled up proportionally to the number of rows used. So,
#   dist(item1,item2) = sqrt( ((4-6)^2 + (7-4)^2) * 5/2) = 5.700877
#   dist(item1,item3) = sqrt((2-8)^2 * 5/1) = 13.41641


simil(ratings_train1, diag=TRUE, upper=TRUE, method="Euclidean", by_rows=FALSE)
# Euclidean similarity between all pairs of rows
# ----------------------------------------------
#         item1      item2      item3      item4      item5      item6
# item1 0.00000000 0.14923419 0.06936541 0.30901699 0.16666667 0.12973191
# item2 0.14923419 0.00000000 0.30901699 0.13299283 0.09785172 0.24025307
# item3 0.06936541 0.30901699 0.00000000 0.30901699 0.12389934 0.17151309
# item4 0.30901699 0.13299283 0.30901699 0.00000000 0.13652706 0.13299283
# item5 0.16666667 0.09785172 0.12389934 0.13652706 0.00000000 0.24025307
# item6 0.12973191 0.24025307 0.17151309 0.13299283 0.24025307 0.00000000

#  Similarity = 1/(1+distance)
# The diagonal elements actually have values of 1/(1+0) = 1 but are incorrectly shown as 0 above
# Note how the above calculations of similarity match the below programmatic output:

model_details$sim # similarity matrix
#       item1     item2     item3     item4     item5     item6
# item1 .         0.1492342 .         0.3090170 0.1666667 .        
# item2 0.1492342 .         0.3090170 .         .         0.2402531
# item3 .         0.3090170 .         0.3090170 .         0.1715131
# item4 0.3090170 .         0.3090170 .         0.1365271 .        
# item5 0.1666667 .         .         0.1365271 .         0.2402531
# item6 .         0.2402531 0.1715131 .         0.2402531 .        

# Why are some similarities excluded from the similarity matrix? e.g. (1,3) (1,6) (2,4) (2,5) (3,5) (4,6)
# Because the similarities of only the k(=3) nearest items are shown


# b. Find the 3-nearest neighbors of item1, item3 and item6.
# [Ans] Based on the similarity matrix, the items and their 3-nearest neighbors are as follows:
#   item1 - item4, item5, item2
#   item3 - item2, item4, item6
#   item6 - item2, item5, item3
  
# c. Compute the item preference vectors for user1, user3 and user5 using the formula discussed in class.
#    Use 3-nearest neighbors while computing preferences.

# Predict user1's rating on item3
#    Top 3 nearest neighbors of item3 are items 2,4 and 6 (based on similarity)
#    Among the neighbors, user1 has rated only items 2 and 4
#    So, use their user1 ratings with weightages as per similarity to predict for item3, i.e,
#    user1item3Prediction = [user1item2Rating*itemSim(3,2) + user1item4Rating*itemSim(3,4)] / 
#                           (itemSim(3,2) + itemSim(3,4))
(6*0.3090170 + 5*0.3090170)/(0.3090170 + 0.3090170) # 5.5

# Predict user1's rating on item6
(6*0.2402531 + 1*0.2402531)/(0.2402531 + 0.2402531) # 3.5

# Predict user2's rating on item2
(2*0.1492342 + 8*0.3090170 + 5*0.2402531)/(0.1492342 + 0.3090170 + 0.2402531) # 5.68625

# Predict user3's rating on item5
(7*0.1666667 + 8*0.1365271 + 4*0.2402531)/(0.1666667 + 0.1365271 + 0.2402531) # 5.924951

# Note how the above computed predictions match the below programmatic output
recom_ratings = predict(ibcf_model, ratings_train2, type="ratings")
recom_ratings_matrix = as(recom_ratings,"matrix")
recom_ratings_matrix
#          item1    item2    item3    item4    item5 item6
# user1       NA       NA 5.500000       NA       NA   3.5
# user2       NA 5.686250       NA 4.638129       NA    NA
# user3       NA       NA 5.565541       NA 5.924951    NA
# user4 8.417225       NA       NA 8.387145       NA    NA
# user5 6.000000 6.125191       NA       NA       NA    NA

# Based on above predictions, we see that user1 preferences are item3 and then item6
# Also, user3 preferences are item5 and then item3
# Note how they match with the below programmatic output
recom_topn = predict(ibcf_model, ratings_train2, type="topNList", n=3)
recom_topn_list = as(recom_topn, "list")
recom_topn_list
# [[1]]
# [1] "item3" "item6"
# 
# [[2]]
# [1] "item2" "item4"
# 
# [[3]]
# [1] "item5" "item3"
# 
# [[4]]
# [1] "item1" "item4"
# 
# [[5]]
# [1] "item2" "item1"

# d. Find the first item you will recommend for user1, user3 and user5 respectively.
# [Ans] The above item preference vector gives the answers immediately:
#         user1 -> item3
#         user3 -> item5
#         user5 -> item2


# -------------------------------------------------------
# Problem 1: User-User nearest neighbor based recommender
# -------------------------------------------------------

ubcf_model=Recommender(ratings_train2,method="UBCF", param=list(method="Euclidean",nn=3,minRating=1,normalize=NULL))
# when normalize=NULL, user ratings remain unchanged
# when normalize="center", normalized user rating = user rating - mean(user rating)
# when normalize="Z-score", normalized user rating = [user rating - mean(user rating)]/standardDeviation(user rating)

str(ubcf_model) # similarity matrix unavailable for "UBCF" (although it was available for "IBCF")
model_details = getModel(ubcf_model)
model_details
str(model_details)

model_details$data@data # original data matrix with original values (since data was not normalized)

# check out the default parameter values for UBCF
recommenderRegistry$get_entry_names()
recommenderRegistry$get_entry("UBCF", dataType = "realRatingMatrix")

ratings_train1
# Given rating matrix (5 rows x 6 columns)
# ----------------------------------------
#         item1 item2 item3 item4 item5 item6
# user1     4     6    NA     5     1    NA
# user2     2    NA     8    NA     3     5
# user3     7     4    NA     8    NA     4
# user4    NA    10     9    NA     7     8
# user5    NA    NA     7     6     6     5

# Compute Euclidean distances between rows
dist(ratings_train1, diag=TRUE, upper=TRUE, method="Euclidean", by_rows=TRUE)
# Euclidean distances between all pairs of rows
# ---------------------------------------------
#        user1     user2     user3     user4     user5
# user1  0.000000  4.898979  6.633250 12.489996  8.831761
# user2  4.898979  0.000000  8.831761  7.211103  4.472136
# user3  6.633250  8.831761  0.000000 12.489996  3.872983
# user4 12.489996  7.211103 12.489996  0.000000  5.291503
# user5  8.831761  4.472136  3.872983  5.291503  0.000000

# Missing values are allowed but are excluded from all computations involving the rows within which they occur. 
# If some columns are excluded in calculating a Euclidean, Manhattan, Canberra or Minkowski distance, the sum 
# is scaled up proportionally to the number of columns used. So,
#   dist(user1,user2) = sqrt( ((4-2)^2 + (1-3)^2) * 6/2) = 4.898979
#   dist(user1,user3) = sqrt(((4-7)^2 + (6-4)^2  + (5-8)^2) * 6/3) = 6.63325

# a. Compute the user-user similarity matrix for all 5 users. Use the Euclidian Similarity metric.

simil(ratings_train1, diag=TRUE, upper=TRUE, method="Euclidean", by_rows=TRUE)
# Euclidean similarity between all pairs of rows
# ----------------------------------------------
#       user1      user2      user3      user4      user5
# user1 0.00000000 0.16952085 0.13100580 0.07412901 0.10171118
# user2 0.16952085 0.00000000 0.10171118 0.12178632 0.18274400
# user3 0.13100580 0.10171118 0.00000000 0.07412901 0.20521310
# user4 0.07412901 0.12178632 0.07412901 0.00000000 0.15894454
# user5 0.10171118 0.18274400 0.20521310 0.15894454 0.00000000

#  Similarity = 1/(1+distance)
# The diagonal elements actually have values of 1/(1+0) = 1 but are incorrectly shown as 0 above


# b. Find the 3-nearest neighbors of user1, user3 and user5.
# [Ans] Based on the similarity matrix, the users and their 3-nearest neighbors are as follows:
#   user1 - user2, user3, user5
#   user3 - user5, user1, user2
#   user5 - user3, user2, user4


# c. Compute the item preference vectors for user1, user3 and user5 using the formula discussed in class.
#    Use 3-nearest users while computing preferences.

# Predict user1's rating on item3
#    Top 3 nearest neighbors of user 1 are users 2,3 and 5 (based on similarity)
#    Among the neighbors, only users 2 and 5 have ratings for item3
#    So, use their item3 predictions with weightages as per similarity to predict for user1
#    user1item3Prediction = [user2item3Rating*userSim(1,2) + user5item3Rating*userSim(1,5)] / 
#                           (userSim(1,2) + userSim(1,5))
(8*0.16952085 + 7*0.10171118)/(0.16952085 + 0.10171118) # 7.625003

# Predict user1's rating on item6
(5*0.16952085 + 4*0.13100580 + 5*0.10171118)/(0.16952085 + 0.13100580 + 0.10171118) # 4.674308

# Predict user2's rating on item2
(6*0.16952085 + 10*0.12178632)/(0.16952085 + 0.12178632) # 7.672274

# Predict user3's rating on item5
(6*0.20521310 + 1*0.13100580 + 3*0.10171118)/(0.20521310 + 0.13100580 + 0.10171118) # 3.807498

# Notice how our predictions do NOT MATCH the below programmatic output (not even close to ours)
# I suspect that the UBCF implementation is incorrect
# ==============================================================================================
recom_ratings = predict(ubcf_model, ratings_train2, type="ratings")
class(recom_ratings)
recom_ratings_matrix = as(recom_ratings,"matrix")
recom_ratings_matrix
#             item1     item2    item3     item4    item5    item6
# [user1]        NA        NA 1.042783        NA       NA 1.054671
# [user2]        NA 0.7521641       NA 1.4376387       NA       NA
# [user3]        NA        NA 1.075042        NA 1.019507       NA
# [user4] 0.1901825        NA       NA 0.7446274       NA       NA
# [user5] 1.2982964 0.5914105       NA        NA       NA       NA

# Trying to get the right UBCF output by using IBCF on the transpose of the original matrix
# ==========================================================================================
ratings_train3 = as(t(ratings_train1), "realRatingMatrix")
ubcf_model=Recommender(ratings_train3,method="IBCF", param=list(method="Euclidean",k=3,minRating=1,normalize=NULL,alpha=0.5))

# Note how our similarity matrix matches the below programmatic output
# Only 3 distances are shown for each since we have set k=3
model2_details = getModel(ubcf_model)
model2_details$sim
#       user1     user2      user3     user4     user5
# user1 .         0.1695208 0.13100580 .         0.1017112
# user2 0.1695208 .         .          0.1217863 0.1827440
# user3 0.1310058 0.1017112 .          .         0.2052131
# user4 .         0.1217863 0.07412901 .         0.1589445
# user5 .         0.1827440 0.20521310 0.1589445 .        


# Notice how our predictions MATCH the below programmatic output
recom_ratings2 = predict(ubcf_model, ratings_train3, type="ratings")
recom_ratings_matrix2 = as(recom_ratings2,"matrix")
recom_ratings_matrix2
#          user1    user2    user3    user4    user5
# item1       NA       NA       NA 3.891863 4.644791
# item2       NA 7.672274       NA       NA 6.618831
# item3 7.625003       NA 7.331389       NA       NA
# item4       NA 5.518769       NA 6.636100       NA
# item5       NA       NA 3.807498       NA       NA
# item6 4.674308       NA       NA       NA       NA



# Based on above predictions, we see that user1 preferences are item3 and then item6
# Also, user3 preferences are item3 and then item5

# Note how the below programmatic output is irrelevant
# since it recommends top N users for each item
recom_topn2 = predict(ubcf_model, ratings_train3, type="topNList", k=3)
recom_topn_list2 = as(recom_topn2, "list")
recom_topn_list2

# d. Find the first item you will recommend for user1, user3 and user5 respectively.
# [Ans] The above item preference vector gives the answers immediately:
#         user1 -> item3
#         user3 -> item3
#         user5 -> item2


# -----------------------------------------------------------
# Problem 3: Factorization nearest neighbor based recommender
# -----------------------------------------------------------

# a. Compute the SVD of user-item matrix. You can fill NA ratings with median
#    rating of each user for simplifying computing matrix factors easily.

ratings_train1
class(ratings_train1)
str(ratings_train1)

# Convert the ratings matrix into numerical type to perform imputations and SVD
ratings_train4 = matrix(as.numeric(ratings_train1),ncol=6, byrow = FALSE)

ratings_train4
str(ratings_train4)

library(caret)

# median-based imputation
preObj1 = preProcess(data.frame(ratings_train4), method=c("medianImpute"))
df = predict(preObj1, data.frame(ratings_train4))
df # finds medians on each column/item instead of on user.

# So, let us do median imputation on the transpose of the matrix
preObj2 = preProcess(data.frame(t(ratings_train4)), method=c("medianImpute"))
df = predict(preObj2, data.frame(t(ratings_train4)))
df # finds medians on each user

# re-create the original matrix with the imputed values
ratings_train5 = t(as.matrix(df))
ratings_train5
dim(ratings_train5)

# b. Compute the user-factor and item-factor matrices from SVD matrices.

# factorize the matrix
result = La.svd(ratings_train5)
user_factor_matrix = result$u
user_factor_matrix
item_factor_matrix = result$d*result$vt
item_factor_matrix
# product of the below lhs and rhs matrices is equal to the original matrix
user_factor_matrix %*% item_factor_matrix # same as rating_train5

# c. Find the latent factor based profiles of user1 and user3.
user_factor_matrix[1,]
user_factor_matrix[3,]

# d. Find the latent factor based profiles for item2 and item6.
item_factor_matrix[,2]
item_factor_matrix[,6]

# e. Compute the item preference vectors for user1, user3 and user5 using the formula discussed in class.
#     [Ans] This is not the actual way to compute the unknown ratings. We have already assumed the unknown
# ratings to be the median user ratings. This was just to simplify the process, and to help us familiarize
# with "Singular Value Decomposition" of a matrix. However, ignoring this fact, we have:
#   user1 -> item3,item6
#   user3 -> item3,item5
#   user5 -> item1,item2


# ------------------------------------
# Problem 4: Finding Association Rules
# ------------------------------------

# Apply the Apriori algorithm on the grocery store data given below with support 
# threshold s=33.34% and confidence threshold c=60%. Enumerate all the final frequent 
# itemsets. Also indicate the association rules that are generated and highlight the 
# strong ones, sort them by confidence.

# TransactionID   Items
# -------------   -----
# T1              HotDogs, Buns, Ketchup
# T2              HotDogs, Buns
# T3              HotDogs, Coke, Chips
# T4              Chips, Coke
# T5              Chips, Ketchup
# T6              HotDogs, Coke, Chips

# For transaction association A->B, we have the following metrics:
# support(A->B) = (Number of transactions with both A and B) / (Total number of transactions)
# confidence(A->B) = (Number of transactions with both A and B) / (Number of transactions with A only)

# We thus make the following observations:

# Association             Support     Confidence
# -----------             -------     ----------
# HotDogs -> Buns           2/6           2/4
# Buns -> HotDogs           2/6           2/2
# HotDogs -> Coke           2/6           2/4
# Coke -> HotDogs           2/6           2/3
# HotDogs -> Chips          2/6           2/4
# Chips -> HotDogs          2/6           2/4
# Chips -> Coke             3/6           3/4
# Coke -> Chips             3/6           3/3
# Coke+Chips -> HotDogs     2/6           2/3
# Coke+HotDogs -> Chips     2/6           2/2
# Chips+HotDogs -> Coke     2/6           2/2


# Filtering the associations based on support >= 1/3 and confidence >= 0.6, we have:

# Association             Support     Confidence
# -----------             -------     ----------
# Buns -> HotDogs           2/6           2/2
# Coke -> HotDogs           2/6           2/3
# Chips -> Coke             3/6           3/4
# Coke -> Chips             3/6           3/3
# Coke+Chips -> HotDogs     2/6           2/3
# Coke+HotDogs -> Chips     2/6           2/2
# Chips+HotDogs -> Coke     2/6           2/2

# Sorting them by confidence, we have:

# Association             Support     Confidence
# -----------             -------     ----------
# Buns -> HotDogs           2/6           2/2
# Coke -> Chips             3/6           3/3
# Coke+HotDogs -> Chips     2/6           2/2
# Chips+HotDogs -> Coke     2/6           2/2
# Chips -> Coke             3/6           3/4
# Coke -> HotDogs           2/6           2/3
# Coke+Chips -> HotDogs     2/6           2/3

# See how our associations/rules match the programmatic output below:

library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)

setwd("D:\\Data Science\\Algorithmica\\Assignments\\Recommenders")

transactions = read.csv("transactions.csv", header = T, stringsAsFactors=FALSE)
dim(transactions)
str(transactions)

class(split(transactions1$item, transactions1$id)) # list
showMethods(coerce) # shows the list of transformations applicable: e.g. from="list", to="transactions"

# transactions need to be converted into a  particular key->value form 
# before passing as input to the 'apriori' method
trans = as(split(transactions$Item, transactions$TransactionID), "transactions")
trans
inspect(trans)

rules = apriori(trans, parameter = list(supp=0.3333,conf=0.6))
inspect(rules)

rules=sort(rules, decreasing=TRUE,by="confidence")
inspect(rules)

#   lhs          rhs         support confidence lift
# 1 {Buns}    => {HotDogs} 0.3333333  1.0000000  1.5
# 2 {Coke}    => {Chips}   0.5000000  1.0000000  1.5
# 3 {Coke,                                          
#   HotDogs} => {Chips}   0.3333333  1.0000000  1.5
# 4 {Chips,                                         
#   HotDogs} => {Coke}    0.3333333  1.0000000  2.0
# 5 {Chips}   => {Coke}    0.5000000  0.7500000  1.5
# 6 {}        => {Chips}   0.6666667  0.6666667  1.0
# 7 {}        => {HotDogs} 0.6666667  0.6666667  1.0
# 8 {Coke}    => {HotDogs} 0.3333333  0.6666667  1.0
# 9 {Chips,                                         
#   Coke}    => {HotDogs} 0.3333333  0.6666667  1.0