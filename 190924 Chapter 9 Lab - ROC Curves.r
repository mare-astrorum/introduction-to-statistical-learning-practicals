library(ROCR)
set.seed(1)

x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(x=x, y=as.factor(y))
train = sample(200, 100)

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt = svm(y~., data = dat[train, ], kernel = "radial",
                 gamma = 2, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.opt, dat[train, ], 
                            decision.values = T))$decision.values

par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

svmfit.flex = svm(y~., data = dat[train, ], kernel = "radial",
                 gamma = 50, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train, ], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

fitted = attributes(predict(svmfit.opt, dat[-train, ], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "Test Data")

fitted = attributes(predict(svmfit.flex, dat[-train, ], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")
