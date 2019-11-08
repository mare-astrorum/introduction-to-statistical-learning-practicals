# 7. 
# 
# In this problem, you will use support 
# vector approaches in order to
# predict whether a given car gets 
# high or low gas mileage based on the
# Auto data set.

library(ISLR)
attach(Auto)
set.seed(1)

# (a) 
# 
# Create a binary variable that 
# takes on a 1 for cars with gas
# mileage above the median, and 
# a 0 for cars with gas mileage
# below the median.

?Auto
pairs(Auto)
plot(mpg)
Auto$mpg[1:20]
median.mpg = median(mpg)
dim(Auto)
Auto$mpglevel = as.factor(ifelse(Auto$mpg > median.mpg, 1, 0))
trial[1:20] 


# (b) 
# 
# Fit a support vector classifier 
# to the data with various values
# of cost , in order to predict 
# whether a car gets high or low gas
# mileage. Report the cross-validation 
# errors associated with dif-
#   ferent values of this parameter. 
# Comment on your results.

library(e1071)

set.seed(3255)
tune.out = tune(svm, mpglevel~.-mpg, data = Auto, kernel="linear",
                ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out)

# best cost = 1
# 4 1e+00 0.09173077 0.04008042

# (c) 
# 
# Now repeat (b), this time using 
# SVMs with radial and polyno-
#   mial basis kernels, with different 
# values of gamma and degree and
# cost . Comment on your results.

gamma = c(0.001, 0.01, 0.1, 1, 10, 100)
for (i in gamma){
  svmfit.rad = svm(mpglevel~.-mpg, data = Auto, kernel="radial", 
                   gamma=i, cost=1, decision.values = T)
  print(table(Auto$mpglevel, predict(svmfit.rad, Auto)))
}


degree = c(3, 4, 5, 6, 7)
for (i in degree){
  svmfit.poly = svm(mpglevel~.-mpg, data = Auto, kernel="polynomial", 
                    gamma=1, cost=1, degree = i, decision.values = T)
  print(table(Auto$mpglevel, predict(svmfit.poly, Auto)))
}

set.seed(21)
tune.out = tune(svm, mpglevel~.-mpg, data = Auto, kernel = "polynomial", 
                ranges = list(cost = c(0.1, 1, 5, 10), 
                              degree = c(2, 3, 4)))
summary(tune.out)

# 4  10.0      2 0.5585897 0.10895475 --> best performance,
# cost = 10, degree = 2

set.seed(463)
tune.out = tune(svm, mpglevel~.-mpg, data = Auto, kernel = "radial", 
                ranges = list(cost = c(0.1, 1, 5, 10),
                              gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
# - best parameters:
#   cost gamma
# 1     1
# 
# - best performance: 0.08410256 

# (d) 
# 
# Make some plots to back up 
# your assertions in (b) and (c).
# Hint: In the lab, we used the 
# plot() function for svm objects
# only in cases with p = 2. When 
# p > 2, you can use the plot()
# function to create plots displaying 
# pairs of variables at a time.
# Essentially, instead of typing
# 
# > plot (svmfit, dat)
# 
# where svmfit contains your 
# fitted model and dat is a data frame
# containing your data, you can type
# 
# > plot (svmfit, dat, x1~x4)
# 
# in order to plot just the first and 
# fourth variables. However, you
# must replace x1 and x4 with the 
# correct variable names. To find
# out more, type ?plot.svm.





svm.linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
               degree = 2)
svm.radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }

plot(svm.linear, Auto, mpg~horsepower)
}
plotpairs(svm.linear)