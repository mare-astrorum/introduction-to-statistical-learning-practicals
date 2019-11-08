### 7 ###

# In the lab, we applied random forests 
# to the Boston data using mtry=6 and
# using ntree=25 and ntree=500 . Create 
# a plot displaying the test error
# resulting from random forests on this 
# data set for a more com- prehensive
# range of values for mtry and ntree . 
# You can model your plot after Figure
# 8.10. Describe the results obtained.

library(MASS)
library(randomForest)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
X.test = Boston[-train, -14]
Y.test = Boston[-train, "medv"]

p = dim(Boston)[2] - 1
p.2 = p/2
p.sq = sqrt(p)

rf.boston.p = randomForest(medv~., data = Boston, subset = train, 
                           xtest = X.test, ytest = Y.test,
                           mtry = p, ntree = 500)
rf.boston.p.2 = randomForest(medv~., data = Boston, subset = train, 
                             xtest = X.test, ytest = Y.test,
                             mtry = p.2, ntree = 500)
rf.boston.p.sq = randomForest(medv~., data = Boston, subset = train, 
                              xtest = X.test, ytest = Y.test,
                              mtry = p.sq, ntree = 500)

plot(1:500, rf.boston.p$test$mse, type = "l", ylim = c(10, 19))
lines(1:500, rf.boston.p.2$test$mse, col = "red")
lines(1:500, rf.boston.p.sq$test$mse, col = "green")
legend("topright", c("P=M", "P=M/2", "Sqrt(P)"), 
       col = c("black", "red", "green"), lty = 1)                                              ))

# forests = rep(NA, 13)
# for (i in 1:13){
#   rf.boston = randomForest(medv~., data = Boston, subset = train,
#                            mtry=i, importance=TRUE, n.trees = 500)
#   yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
#   forests[i] = rf.boston$mse
# }
# 
# plot(forests)
# rf.boston = randomForest(medv~., data = Boston, subset = train,
#                          mtry=13, importance=TRUE, n.trees = 100)
# yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
# forests.10 = mean((yhat.rf - boston.test)^2)
# 
# plot(1:30, forests)
# 
# importance(rf.boston)
# varImpPlot(rf.boston)
