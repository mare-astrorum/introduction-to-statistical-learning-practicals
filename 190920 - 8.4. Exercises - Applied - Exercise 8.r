# ### 8 ###
# 
# In the lab, a classification tree was 
# applied to the Carseats data set after
# converting Sales into a qualitative 
# response variable. Now we will seek to
# predict Sales using regression trees 
# and related approaches, treating the
# response as a quantitative variable. 

library(tree)
library(ISLR)
attach(Carseats)
set.seed(1)


# #(a) 
# 
# Split the data set into a training
# set and a test set. 

?Carseats
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]
Y.test = Carseats[-train, "Sales"]

# #(b) 
# 
# Fit a regression tree to the training set. 
# Plot the tree, and interpret the results. 
# What test MSE do you obtain? 

C.train.tree = tree(Sales~., Carseats.train)
plot(C.train.tree)
text(C.train.tree, pretty = 0)
summary(C.train.tree)

pred = predict(C.train.tree, newdata = Carseats.test)
mse = mean((Y.test-pred)^2)

  
#   #(c) 
#   
#   Use
# cross-validation in order to determine 
# the optimal level of tree complexity.
# Does pruning the tree improve the test MSE? 

cv.carseats = cv.tree(C.train.tree)
plot(cv.carseats$size, cv.carseats$dev, type = "b")

# Seven looks good

?tree
prune.carseats = prune.tree(C.train.tree, best = 7)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
summary(prune.carseats)

pred7 = predict(prune.carseats, newdata = Carseats.test)
mse7 = mean((Y.test-pred7)^2)

#   (d) 
# 
# Use the bagging approach in
# order to analyze this data. What test 
# MSE do you obtain? Use the importance()
# function to de- termine which 
# variables are most important. 

dim(Carseats.train)

library(randomForest)
bag.Carseats = randomForest(Sales~., data = Carseats.train, 
                           mtry = 10, ntree = 500, importance = TRUE)
pred.bag = predict(bag.Carseats, newdata = Carseats.test)
mse.bag = mean((Y.test - pred.bag)^2)

importance(bag.Carseats)
varImpPlot(bag.Carseats)

# (e) 
# 
# Use random
# forests to analyze this data. What 
# test MSE do you obtain? Use the
# importance() function to determine 
# which vari- ables are most important.
# Describe the effect of m, the number 
# of variables considered at each split, 
# on the error rate obtained.

sqrt.Carseats = sqrt(10)
rf.Carseats = randomForest(Sales~., data = Carseats.train, 
                            mtry = 3, ntree = 500, importance = TRUE)
pred.rf = predict(rf.Carseats, newdata = Carseats.test)
mse.rf = mean((Y.test - pred.rf)^2)

importance(rf.Carseats)
varImpPlot(rf.Carseats)
