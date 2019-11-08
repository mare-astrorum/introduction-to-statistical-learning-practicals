# 10. In this problem, you will generate
# simulated data, and then perform
# PCA and K-means clustering on the data.

set.seed(2)

# (a) 
# 
# Generate a simulated data set with 
# 20 observations in each of
# three classes (i.e. 60 observations 
# total), and 50 variables.
# Hint: There are a number of functions 
# in R that you can use to
# generate data. One example is the 
# rnorm() function; runif() is
# another option. Be sure to add a 
# mean shift to the observations
# in each class so that there are 
# three distinct classes.

x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

 
# (b) 
# 
# Perform PCA on the 60 observations 
# and plot the first two prin-
# cipal component score vectors. 
# Use a different color to indicate
# the observations in each of the 
# three classes. If the three classes
# appear separated in this plot, 
# then continue on to part (c). If
# not, then return to part (a) 
# and modify the simulation so that
# there is greater separation 
# between the three classes. Do not
# continue to part (c) until the 
# three classes show at least some
# separation in the first two 
# principal component score vectors.

pr.out = prcomp(x)

biplot(pr.out, scale = 0)

plot(pr.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

# (c) 
# 
# Perform K-means clustering of the 
# observations with K = 3.
# How well do the clusters that 
# you obtained in K-means cluster-
# ing compare to the true class labels?
# Hint: You can use the table() 
# function in R to compare the true
# class labels to the class labels 
# obtained by clustering. Be careful
# how you interpret the results: 
#   K-means clustering will arbitrarily
# number the clusters, so you cannot 
# simply check whether the true
# class labels and clustering labels 
# are the same.

km.out = kmeans(x, 3, nstart=20)

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

 
# (d) 
# 
# Perform K-means clustering with 
# K = 2. Describe your results.

km.out = kmeans(x, 2, nstart=20)

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# k = 2 forces the three distinct groups into 2 clusters

# (e) 
# 
# Now perform K-means clustering 
# with K = 4, and describe your
# results.

km.out = kmeans(x, 4, nstart=20)
km.out$cluster

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# one of the groups gets split in half

# (f) 
# 
# Now perform K-means clustering 
# with K = 3 on the first two
# principal component score vectors, 
# rather than on the raw data.
# That is, perform K-means 
# clustering on the 60 × 2 matrix of
# which the first column is the 
# first principal component score
# vector, and the second column 
# is the second principal component
# score vector. Comment on the results.

princ.comp.1.2 = pr.out$x[,1:2]
km.out = kmeans(princ.comp.1.2, 3, nstart=20)
km.out$cluster

# PCA finds clusters well

# (g) 
# 
# Using the scale() function, perform 
# K-means clustering with
# K = 3 on the data after scaling 
# each variable to have standard
# deviation one. How do these 
# results compare to those obtained
# in (b)? Explain.

scaled.x = scale(x)

km.out = kmeans(scaled.x, 3, nstart=20)
km.out$cluster

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
