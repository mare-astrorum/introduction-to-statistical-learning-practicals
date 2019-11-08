### Chapter 6, Exercise 10 ###

# 10. We have seen that as the number 
# of features used in a model increases,
# the training error will necessarily 
# decrease, but the test error may not.
# We will now explore this in a simulated 
# data set.
# 
# 
# (a) Generate a data set with p = 20 
# features, n = 1,000 observa-
# tions, and an associated quantitative 
# response vector generated
# according to the model
# Y = Xβ + eps,
# where β has some elements that are 
# exactly equal to zero.

set.seed(1)
p = 20
n = 1000
?matrix
x = matrix(data=rnorm(n * p), nrow = n, ncol = p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
eps = rnorm(p)
y = x %*% B + eps
 
# (b) Split your data set into a 
# training set containing 100 observations
# and a test set containing 
# 900 observations.
 
set.seed(1)
train = sample(1:nrow(x), 100, replace=F)
test = (-train)
y.train=y[train]
y.test = y[test]
x.train = x[train, ]
x.test = x[test, ]


 
# (c) Perform best subset selection 
# on the training set, and plot the
# training set MSE associated 
# with the best model of each size.

library(leaps)
training.data.full = data.frame(x=x.train, y=y.train)
regfit.full = regsubsets(y~., data=data.full, nvmax=100)
reg.summary = summary(regfit.full)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# (d) Plot the test set MSE associated 
# with the best model of each
# size.
# 
# 
# (e) For which model size does the 
# test set MSE take on its minimum
# value? Comment on your results. 
# If it takes on its minimum value
# for a model containing only an 
# intercept or a model containing
# all of the features, then play 
# around with the way that you are
# generating the data in (a) until 
# you come up with a scenario in
# which the test set MSE is minimized
# for an intermediate model
# size.
# 
# 
# (f) How does the model at which
#  the test set MSE is minimized
# compare to the true model used 
# to generate the data? Comment
# on the coefficient values.
# 
# 
# (g) Create a plot displaying
# FORMULA for a range of values
# of r, where β̂ j r is the jth 
# coefficient estimate for the best model
# containing r coefficients. 
# Comment on what you observe. How
# does this compare to the test 
# MSE plot from (d)?