# 4. 
# 
# Generate a simulated two-class data 
# set with 100 observations and
# two features in which there is a 
# visible but non-linear separation be-
# tween the two classes. 

library(e1071)
set.seed(131)
x1 = rnorm(100)
x2 = 3 * x1^2 + 4 + rnorm(100)
split = sample(100, 50)
x2[split] = x2[split] + 3
x2[-split] = x2[-split] - 3

plot(x1[split], x2[split], ylim = c(-5, 30))
points(x[-split], y[-split], col = "red")
max(y)
min(y)


#Show that 
# in this setting, a support vector
# machine with a polynomial kernel 
# (with degree greater than 1) or a
# radial kernel will outperform a support 
# vector classifier on the train-
#   ing data. 

set.seed(315)
y = rep(0, 100)
y[split] = 1
# Take 25 observations each from train and -train
train = c(sample(split, 25), sample(setdiff(1:100, split), 25))
data.train = data.frame(x1=x1[train], x2=x2[train], y=as.factor(y[train]))
data.test = data.frame(x1=x1[-train], x2=x2[-train], y=as.factor(y[-train]))

svmfit.lin = svm(y~., data = data.train, kernel = "linear", cost = 10, 
                 scale = F, decision.values = T)
plot(svmfit.lin, data.train)
table(y[train], predict(svmfit.lin, data.train))

set.seed(996)
svmfit.rad = svm(y~., data = data.train, kernel = "radial", cost = 10, 
                 gamma = 1, decision.values = T)
plot(svmfit.rad, data.train)
table(y[train], predict(svmfit.rad, data.train))

set.seed(32545)
svmfit.poly = svm(y~., data = data.train, kernel = "polynomial", cost = 10, 
                 gamma = 1, degree = 3, decision.values = T)
plot(svmfit.poly, data.train)
table(y[train], predict(svmfit.poly, data.train))


# Which technique performs 
# best on the test data? 

svmfit.lin = svm(y~., data = data.test, kernel = "linear", cost = 10, 
                 scale = F, decision.values = T)
plot(svmfit.lin, data.test)
table(y[-train], predict(svmfit.lin, data.test))

svmfit.rad = svm(y~., data = data.test, kernel = "radial", cost = 10, 
                 gamma = 1, decision.values = T)
plot(svmfit.rad, data.test)
table(y[-train], predict(svmfit.rad, data.test))

svmfit.poly = svm(y~., data = data.test, kernel = "polynomial", cost = 10, 
                  gamma = 1, degree = 3, decision.values = T)
plot(svmfit.poly, data.test)
table(y[-train], predict(svmfit.poly, data.test))

# Radial works the best


# Make
# plots and report training and test 
# error rates in order to back up
# your assertions.

library(ROCR)
set.seed(1)

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

par(mfrow=c(1,2))
fitted = attributes(predict(svmfit.lin, data.train, 
                            decision.values = T))$decision.values
rocplot(fitted, data.train[, "y"], main = "Train Data")
fitted = attributes(predict(svmfit.rad, data.train, 
                            decision.values = T))$decision.values
rocplot(fitted, data.train[, "y"], add = T, col = "red")
fitted = attributes(predict(svmfit.poly, data.train, 
                            decision.values = T))$decision.values
rocplot(fitted, data.train[, "y"], add = T, col = "blue")

fitted = attributes(predict(svmfit.lin, data.test, 
                            decision.values = T))$decision.values
rocplot(fitted, data.test[, "y"], main = "Test Data")
fitted = attributes(predict(svmfit.rad, data.test, 
                            decision.values = T))$decision.values
rocplot(fitted, data.test[, "y"], add = T, col = "red")
fitted = attributes(predict(svmfit.poly, data.test, 
                            decision.values = T))$decision.values
rocplot(fitted, data.test[, "y"], add = T, col = "blue")