library(caret)
library(corrplot)

# Problem 1: Analyzing Olympics dataset
# The dataset Olympics.dat contains the result of the decathlon at the olympic games in Atlanta at 1996 
# and is available at algorithmica repository. Answer the following questions:

setwd("D:/Data Science/Algorithmica/Assignments/Olympics")

# modified the 'header' in olympics.dat to denote the 1st column as player names
olympics = read.csv("olympics2.dat", header = TRUE, sep="\t", stringsAsFactors = FALSE)
dim(olympics)
str(olympics)

# ignore the 'player' column
olympics2 = olympics[,-1]
dim(olympics2) # 11 features

# Remove zero variance features before processing with 'pca'. Else, we get error
nzv_obj = nearZeroVar(olympics2, saveMetrics = T)
olympics3 = olympics2[,nzv_obj$zeroVar==F]
dim(olympics3) # none of the 11 features have zero variance. All retained.

# Do NOT use the below caret's preProcess + pca method as it always scales the data.
# (We want to compare how PCA works on unscaled and scaled data)
# caret_var = preProcess(olympics3, method=c("pca"), thresh = 0.999999)
# PC_var = predict(caret_var, olympics3)
# caret_var$rotation

# We could find PCA using the below princomp() method with cor=TRUE/FALSE for correlation/covariance matrix
# But we prefer the prcomp() method as it gives more accurate values of the rotation matrix
# temp1 = princomp(olympics3, cor = FALSE)
# summary(temp1)
# biplot(temp1, scale = 0)
# temp1$loadings # rotation matrix
# temp2 = princomp(olympics3, cor = TRUE)
# summary(temp2)
# temp2$loadings # rotation matrix
# biplot(temp2, scale = 0)

# a) Find PCA using covariance matrix and make a biplot for first two principal components.
pc_covar = prcomp(olympics3, center = FALSE, scale. = FALSE)
pc_covar1
summary(pc_covar1)
X11()
biplot(pc_covar1, scale = 0) # plots how the original features are related to PC1 and PC2
# Plot shows PC1 dominated by 'punkte' and PC2 dominated by 'stab'
result_covar = predict(pc_covar, olympics3)

# b) Find PCA using correlation matrix and make a biplot for first two principal components.
pc_cor = prcomp(olympics3, center = TRUE, scale. = TRUE)
pc_cor
summary(pc_cor)
X11()
biplot(pc_cor, scale = 0)
# Plot shows how various disciplines (original features) affect PC1 and PC2
result_cor = predict(pc_cor1, olympics3)

# c) Which of the two plots in a) and b) seems more advisable? And why?
        # [Ans] The plot in b) is more sensible. In the original data, the scale of 'punkte' is very high compared
# to that of other disciplines, and hence it has the maximum variance. So, when applying PCA WITHOUT scaling,
# 'punkte' dominates and very heavily influences the PC1 values. Similarly, 'stab' has the second highest scale
# and thus dominates the PC2 values. After scaling, however, we get a more realistic picture.


# d) Answer the following questions from the the plot you selected in last part.

# Note that the biplot itself does NOT show the relationships among disciplines. It only shows the relationship
# of the disciplines with PC1 and PC2. To see the relationships among disciplines, scale the original data and
# compute the correlation matrix on the scaled discipline values.

olympics_z = scale(olympics3)
head(olympics_z)
corr_z = cor(olympics_z)
corr_z
# Note that the PCs are always NOT correlated to any other PC; their vectors are orthogonal to each other. Verify
# this fact in the below correlation matrix where the correlation of every PC to other PCs is almost 0.

#  Which discipline has high correlation with the total number of points (i.e. punkte)?
       # [Ans] From the correlation matrix of scaled data, we see that the 'weit' discipline has the highest
#  correlation (0.6890117) with punkte.

#  Which variable is displayed badly by the projection?
       # [Ans] The 'speer' discipline is badly projected as it overlaps with punkte's projection.

#  State two disciplines with high positive correlation.
       # [Ans] weit and punkte have the highest positive correlation of 0.689011670.

#  State two disciplines with high negative correlation.
       # [Ans] m100 and punkte have the highest negative correlation of -0.6108571

#  State two disciplines which are uncorrelated.
       # [Ans] weit and m1500 have a near-zero correlation of -0.001920072 and thus the least correlated.

##########################################################################################################

# Problem 2: Separating US states according to their violence
# We want to generate an index that separates US states best according to their violence. USArrests dataset is 
# directly available in R. You can load it using data(USArrests) function. This data set contains statistics, 
# in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. 
# Also given is the percent of the population living in urban areas. Do the following tasks on USArrests dataset:

data(USArrests)
dim(USArrests)
str(USArrests)
USArrests
   
# a) Make a new data set containing only info on murder, assault and rape for each state.
#    Compare the ranges and spread of the three variables.
USArrests1 = USArrests[,c(1,2,4)]
USArrests1

# The 3 most common measures of spread are range, inter-quartile range & standard deviation
mean(USArrests1$Assault) # 170.76
median(USArrests1$Assault) # 159
range(USArrests1$Assault) # [45, 337]
sd(USArrests1$Assault) # 83.33766
IQR(USArrests1$Assault) # 140

mean(USArrests1$Rape) # 21.232
median(USArrests1$Rape) # 20.1
range(USArrests1$Rape) # [7.3, 46.0]
sd(USArrests1$Rape) # 9.366385
IQR(USArrests1$Rape) # 11.1

mean(USArrests1$Murder) # 7.788
median(USArrests1$Murder) # 7.25
range(USArrests1$Murder) # [0.8, 17.4]
sd(USArrests1$Murder) # 4.35551
IQR(USArrests1$Murder) # 7.175

# b) Draw the star plot using the following R function: stars(data, draw.segments = TRUE, key.loc = c(21,1))
X11()
stars(USArrests1, draw.segments = TRUE, key.loc = c(21,1))

# c) Perform PCA using princomp with cor=T option.
pc = princomp(USArrests1, cor = TRUE)
summary(pc)

# d) Look at the loadings for PC1. Check that PC1 is a (weighted) average of the three violence measures. 
# By looking at the loadings of PC2, describe the violence profile of states that have a high PC2 score.
pc$loadings

# Loadings: (rotation matrix)
#         Comp.1   Comp.2   Comp.3
# Murder  -0.583   0.534    0.613
# Assault -0.608   0.214   -0.765
# Rape    -0.539  -0.818    0.200

# As per the loadings of PC2, states with a high PC2 score have a high incidence of murder and assault, and
# a low incidence (due to high negative value) of rape.

# e) Verify that the PC1 and PC2 scores are uncorrelated. Make a plot of PC2 versus PC1.

pc_vals = predict(pc, USArrests1)
cor_pc_vals = cor(pc_vals)
# Note that the correlation matrix shows almost zero correlation (2.290569e-16) between PC1 and PC2
cor_pc_vals

# Plot of PC1 vs PC2
X11()
# plot(pc_vals[,"Comp.1"], pc_vals[,"Comp.2"], xlab = "PC1", ylab = "PC2", main = "PC1 vs PC2 of US Assaults")
biplot(pc, scale = 0)

# PC1 loading has negative values for all 3 disciplines. Still PC1 has positive values also because the 
# z-scores of all the disciplines are negative for some states -  PCs are computed on scaled (z-score) data
USArrests_z = scale(USArrests1)


# f) Answer the following questions by looking at the plot:
#  i. What is the most violent state according to PC1? And the least violent?
#        [Ans] PC1 loadings are all negative (Murder: -0.583, Assault: -0.608, Rape: -0.539). So, the most
#     violent state (as per PC1) will have the highest negative PC1 value, which as per the biplot is Florida.
#     Similarly, the least violent state has the highest positive PC1 value, which is North Dakota.
#    (The rightmost state is illegible in the biplot but it is North Dakota with the highest PC1 of 2.68145138.
#     The rightmost state legible in the biplot is Vermont/Maine with PC1 score 2.24330516/2.19666992)

#  ii. What is the state with the highest PC2 score?
#         [Ans] We see that Mississippi has the highest PC2 score in the biplot. Can also verify this below.
which(pc_vals[,"Comp.2"] == max(pc_vals[,"Comp.2"]))

# iii. Look at some states that are close together in the plot, for example Alaska, Nevada and California.
#      Are their star plots similar as well?
#         [Ans] Yes, their star plots are similar. And they also have similar values for the 3 disciplines

#  iv. Look at some states that are far apart in the plot made, for example Nevada and Maine.
#      How do their star plots look?
#         [Ans] Their star plots look very different in size. Nevada's star is much bigger than that of Maine.
#      This is because Nevada is much more violent (higher discipline values) than Maine.
#      We thus see that PC1 and PC2 successfully capture most of the details. This is also supported by the
#      summary information on PCs which states that PC1 and PC2 together capture 93.88% of the total variance.
summary(pc)

#############################################################################################################

# Problem 3: Understanding relationship among performances in tests
# R library provides covariance matrix ability.cov and you can access with data(ability.cov).
# It contains the covariance matrix of the results of six ability and intelligence tests which were given to
# 112 individuals. We are interested in the question of whether the performance in and the correlation between 
# the six tests can be explained by two or three variables describing some general concept of intelligence. 
# Do the following tasks:

# a) Convert the covariance matrix to correlation matrix using cov2cor() function.
data(ability.cov)
ability.cov
ability_cor = cov2cor(ability.cov$cov)
ability_cor

# b) Apply PCA and find the principal components. How many principal components are required 
#    to cover 95% of variance?
#         [Ans] Applying prcomp() on the (scaled version of) original data is equivalent to finding
#    the eigen values/vectors of the correlation matrix of the original data. See below code for a demo:

set.seed(3)
x = matrix(sample(1:100,36), ncol=6)
prcomp(scale(x))
eigen(cor(x))

# Note that the 'standard deviations' of prcomp() are the square roots of the eigen values. Also, the principal 
# components are the same as the eigen vectors themselves (though signs may be opposite as they are here).


out = eigen(ability_cor)
out$vectors # eigen vectors = principal components
out$values # eigen values

variance_total = sum(out$values)
variance_ratio = out$values/variance_total
cumsum(variance_ratio)
# We thus see that we need 5 principal components to cover 95% of the variance

# c) Interpret the meaning of first 4 principal components.

out$vectors
#         [,1]         [,2]        [,3]        [,4]        [,5]         [,6]
# [1,] -0.4714203  0.002369545  0.07195244  0.86265257  0.03701190 -0.164440900
# [2,] -0.3575390  0.407885783  0.59283031 -0.26831757  0.53136405  0.002281474
# [3,] -0.4342703  0.403671942  0.06437068 -0.20077030 -0.77549178  0.051170681
# [4,] -0.2878528  0.403521201 -0.79428554 -0.09655567  0.33374665  0.052077340
# [5,] -0.4396318 -0.506727008 -0.01453950 -0.10039371  0.05554021  0.732513888
# [6,] -0.4303518 -0.501069773 -0.09016971 -0.35231465  0.02057477 -0.656541621

# Only PC3 seems somewhat easily interpretable -> high score in picture & low score in maze.
# Other PCs do not point to anything in particular.
# This problem is an example where PCA is NOT very useful. This happened because there is very less
# correlation in the features of the original data, except 'reading' and 'vocab' having a high 
# correlation of 0.79.