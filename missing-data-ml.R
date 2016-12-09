library(caret)
# install.packages("RANN")
library(RANN)

set.seed(100)
df = data.frame(sample(1:200,50),sample(1:200,50),sample(1:200,50),sample(1:200,50))
names(df) = c('v1','v2','v3','v4')
df

for (i in 1:50) {
  if (i %% 3 == 0) df[i,1] = NA;
  if (i %% 5 == 0) df[i,2] = NA;
  if (i %% 10 == 0) df[i,3] = NA;
}

median(df[,1], na.rm = TRUE)
median(df[,2], na.rm = TRUE)
median(df[,3], na.rm = TRUE)

# There are built-in methods to impute the missing values using median, knn and bagged-trees.
# All these prediction models are built over only those rows for which ALL columns have valid values (non NAs)

# median-based imputation
preObj1 = preProcess(df, method=c("medianImpute"))
df1 = predict(preObj1, df)
df1 # results not good

# knn-based imputation; all values are transformed to z-scores
# center and scale are automatically done by knnImpute; default k = 5
# Model built using only the 27 rows that have valid (non-NA) values in all columns
preObj2 = preProcess(df, method=c("knnImpute"))
preObj2$k
preObj2
df2 = predict(preObj2, df)
df2
# model_v1 = knn(v2,v3,v4) where v1s are non-NA. Transform v2,v3,v4 into z-scores.
# Predicted v1 = avg(k-NN v1 values) where distances are computed from its (v2,v3,v4|v1=NA) to each 
# (v2,v3,v4|v1=non-NA) by using its own v2,v3,v4 values.
#
# After predicting all v1 NAs, predict for V2, v3 and v4 using the 'original data' (without the predicted v1 values)

# bagging-based imputation
# 10 trees are built by default in the 'bagged tree' ensemble
preObj3 = preProcess(df, method=c("bagImpute"))
preObj3$bagImp
# ensemble is built using the 27 rows that have valid (non-NA) values in all columns
preObj3$bagImp$v1$model$mtrees[[6]] # see the 6th tree in the ensemble for predicting v1
df3 = predict(preObj3, df)
df3
# These take a long time since too many calculations are needed


# install.packages("mice")
library(mice) # Multivariate imputation by chained equations
# Multiple imputation; missing values are computed over several iterations.
# Takes a very long time though
md.pattern(df)
imputed = mice(df)
df4 = complete(imputed)
df4
# initially fill with random values; compute using the random values and check if they match the random
# guesses, and refine accordingly. Do this until the values converge