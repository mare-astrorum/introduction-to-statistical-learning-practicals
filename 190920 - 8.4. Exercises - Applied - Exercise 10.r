# ### 10 ###
# 
# We now use boosting to predict 
# Salary in the Hitters data set. 

library(ISLR)
attach(Hitters)
set.seed(1)
?Hitters

# (a) 
# 
# Remove
# the observations for whom the salary 
# information is unknown, and then
# log-transform the salaries. 

dim(Hitters)
summary(is.na(Hitters$Salary))
sum(is.na(Hitters$Salary))
Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))
Hitters$Salary = log(Hitters$Salary)

# (b) 
# 
# Create a training set consisting 
# of the first
# 200 observations, and a test set 
# consisting of the remaining observations. 

train = seq(1, 200, 1)
H.train = Hitters[train, ]
H.test = Hitters[-train, ]


# (c)
# 
# Perform boosting on the training set 
# with 1,000 trees for a range of values of
# the shrinkage parameter λ. Produce a 
# plot with different shrinkage values on
# the x-axis and the corresponding 
# training set MSE on the y-axis. 


library(gbm)


shrinkage = c(0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001)
train.mse = rep(NA, 7)
count = 0
for (i in shrinkage){
  boost.hitters = gbm(Salary~., data = Hitters[train, ], distribution = 
                        "gaussian", n.trees = 1000, interaction.depth = 4,
                      shrinkage = i)
  train.pred = predict(boost.hitters, newdata = H.train, n.trees = 1000)
  H.y = Hitters[train, "Salary"]
  count = count + 1
  train.mse[count] = mean((train.pred - H.y)^2)
  #train.mse[count] = mean((H.train$Salary - train.pred)^2)
}

plot(shrinkage, train.mse)

# (d) 
# 
# Produce a
# plot with different shrinkage values 
# on the x-axis and the corresponding test
# set MSE on the y-axis. 

shrinkage = c(0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001)
mse = rep(NA, 7)
count = 0
for (i in shrinkage){
  boost.hitters = gbm(Salary~., data = Hitters[train, ], distribution = 
                        "gaussian", n.trees = 1000, interaction.depth = 4,
                      shrinkage = i)
  yhat.boost = predict(boost.hitters, newdata = H.test, n.trees = 1000)
  H.y = Hitters[-train, "Salary"]
  count = count + 1
  mse[count] = mean((yhat.boost - H.y)^2)
}

plot(shrinkage, mse, type = "both")

min(mse)
shrinkage[which.min(mse)]

# mse = 0.273397

# (e) 
# 
# Compare the test MSE of boosting 
# to the test MSE
# that results from applying two of the 
# regression approaches seen in Chapters 3
# and 6. 

lm.fit = lm(Salary ~ ., data = H.train)
lm.pred = predict(lm.fit, H.test)
mean((H.test$Salary - lm.pred)^2)

# mse = 0.4917959

library(glmnet)

?model.matrix

set.seed(134)
x = model.matrix(Salary ~ ., data = H.train)
y = H.train$Salary
x.test = model.matrix(Salary ~ ., data = H.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((H.test$Salary - lasso.pred)^2)

# mse = 0.4700537

# Boosting has the lowest mse of all

# (f) 
# 
# Which variables appear to be the 
# most important predictors in the
# boosted model? 

best.boost.hitters = gbm(Salary~., data = Hitters[train, ], distribution = 
                      "gaussian", n.trees = 1000, interaction.depth = 4,
                    shrinkage = 0.01)
summary(best.boost.hitters)

# CAtBat 
# CWalks       
# CRuns

#   (g) 
# 
# Now apply bagging to the training 
# set. What is the test set
# MSE for this approach?


library(randomForest)
set.seed(1)
dim(Hitters)

bag.hitters = randomForest(Salary~., data = Hitters, subset = train,
                          mtry=19, importance=TRUE)
yhat.bag = predict(bag.hitters, newdata = H.test)
mean((yhat.bag - H.test$Salary)^2)

# mse = 0.2291677 --> smaller than boosting
