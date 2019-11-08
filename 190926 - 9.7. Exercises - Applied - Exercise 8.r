# 8. 
# 
# This problem involves the OJ data 
# set which is part of the ISLR
# package.

library(ISLR)
attach(OJ)
?OJ
OJ$Purchase[1:20]
 
# (a) 
# 
# Create a training set 
# containing a random sample of 800
# observations, and a test set 
# containing the remaining
# observations.

train = sample(1070, 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]
 
# (b) 
# 
# Fit a support vector classifier 
# to the training data using
# cost=0.01 , with Purchase as the 
# response and the other variables
# as predictors. Use the summary() 
# function to produce summary statistics, 
# and describe the results obtained.

library(e1071)
svmfit = svm(as.factor(Purchase)~., data = OJ.train, kernel = "linear",
             cost = 0.01)
summary(svmfit)

 
# (c) 
# 
# What are the training and 
# test error rates?

pred.train = predict(svmfit, OJ.train)
table(OJ.train$Purchase, pred.train)

# pred.train
#     CH  MM
# CH 413  62
# MM  74 251

pred.test = predict(svmfit, OJ.test)
table(OJ.test$Purchase, pred.test)

# pred.test
#     CH  MM
# CH 155  23
# MM  18  74

  
#   (d) 
# 
# Use the tune() function to select 
# an optimal cost . Consider val-
#   ues in the range 0.01 to 10.

set.seed(1554)
tune.out = tune(svm, Purchase~., data = OJ.train, kernel="linear",
                           ranges = list(cost=c(0.01, 0.1, 1, 5, 10)))

summary(tune.out)

# - best parameters:
#   cost
#   5
# 
# - best performance: 0.16875 

 
# (e) 
# 
# Compute the training and test 
# error rates using this new value
# for cost .

svmfit = svm(as.factor(Purchase)~., data = OJ.train, kernel = "linear",
             cost = 5)
summary(svmfit)


pred.train = predict(svmfit, OJ.train)
table(OJ.train$Purchase, pred.train)

# pred.train
#     CH  MM
# CH 416  59
# MM  73 252

pred.test = predict(svmfit, OJ.test)
table(OJ.test$Purchase, pred.test)

# pred.test
#     CH  MM
# CH 157  21
# MM  18  74

(21+18)/270
# 0.1444444

# (f) 
# 
# Repeat parts (b) through (e) 
# using a support vector machine
# with a radial kernel. Use the 
# default value for gamma.

svmfit.radial = svm(as.factor(Purchase)~., data = OJ.train, 
                    kernel = "radial")
summary(svmfit.radial)

pred.train.radial = predict(svmfit.radial, OJ.train)
table(OJ.train$Purchase, pred.train.radial)

# pred.train.radial
#    CH  MM
# CH 423  52
# MM  79 246

pred.test.radial = predict(svmfit.radial, OJ.test)
table(OJ.test$Purchase, pred.test.radial)

# pred.test.radial
#     CH  MM
# CH 158  20
# MM  21  71

(20+21)/270
# 0.1518519

set.seed(1554)
tune.out = tune(svm, Purchase~., data = OJ.train, kernel="radial",
                ranges = list(cost=c(0.01, 0.1, 1, 5, 10)))

summary(tune.out)

# - best parameters:
#   cost
# 1
# 
# - best performance: 0.18 


# (g) 
# 
# Repeat parts (b) through (e) 
# using a support vector machine
# with a polynomial kernel. 
# Set degree=2.

svmfit.poly = svm(as.factor(Purchase)~., data = OJ.train, 
                    kernel = "polynomial", degree = 2)
summary(svmfit.poly)

pred.train.poly = predict(svmfit.poly, OJ.train)
table(OJ.train$Purchase, pred.train.poly)

# pred.train.radial
#    CH  MM
# CH 432  43
# MM 109 216

pred.test.poly = predict(svmfit.poly, OJ.test)
table(OJ.test$Purchase, pred.test.poly)

# pred.test.radial
#     CH  MM
# CH 162  16
# MM  29  63

set.seed(1554)
tune.out = tune(svm, Purchase~., data = OJ.train, kernel="polynomial",
                degree = 2,
                ranges = list(cost=c(0.01, 0.1, 1, 5, 10)))

summary(tune.out)

# - best parameters:
#   cost
# 5
# 
# - best performance: 0.19

svmfit.poly = svm(as.factor(Purchase)~., data = OJ.train, kernel = "polynomial",
             degree = 2, cost = 5)
summary(svmfit.poly)


pred.train.poly = predict(svmfit.poly, OJ.train)
table(OJ.train$Purchase, pred.train.poly)

# pred.train.poly
#     CH  MM
# CH 430  45
# MM  87 238

pred.test.poly = predict(svmfit.poly, OJ.test)
table(OJ.test$Purchase, pred.test.poly)

# pred.test.poly
#     CH  MM
# CH 164  14
# MM  26  66

(14+26)/270
# 0.1481481

# (h) 
# 
# Overall, which approach seems to 
# give the best results on this
# data?

# Linear