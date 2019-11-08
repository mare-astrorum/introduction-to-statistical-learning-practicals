# ### 11 ###

# This question uses the Caravan data set. 

library(ISLR)
attach(Caravan)
set.seed(342)
?Caravan
Caravan$Purchase[1:20]
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan$Purchase[1:20]

# (a) 
# 
# Create a training set
# consisting of the first 1,000 observations, 
# and a test set consisting of the
# remaining observations. 

train = 1:1000
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]
 
# (b) 
# 
# Fit a boosting model to the 
# training set with
# Purchase as the response and the other 
# variables as predictors. Use 1,000
# trees, and a shrinkage value of 0.01. 
# Which predictors appear to be the most
# important? 

library(gbm)
caravan.boost = gbm(Purchase~., data = Caravan.train, n.trees = 1000,
                    shrinkage = 0.01) 
summary(caravan.boost)

   
#   (c) 
# 
# Use the boosting model to predict 
# the response on the test
# data. Predict that a person will make 
# a purchase if the estimated prob-
#   ability of purchase is greater than 20 %. 
# Form a confusion ma- trix. What
# fraction of the people predicted to 
# make a purchase do in fact make one? How
# does this compare with the results 
# obtained from applying KNN or logistic
# regression to this data set?

pred = predict(caravan.boost, newdata = Caravan.test, n.trees=1000,
               type = "response")
pred[1:20]
pred.adj = ifelse(pred >= 0.2, 1, 0)
table(Caravan.test$Purchase, pred.adj)
34/(137 + 34)


lm.caravan = glm(Purchase~., data = Caravan.train, family = binomial)
lm.pred = predict(lm.caravan, newdata = Caravan.test, type = "response")
lm.pred[1:20]

lm.pred.adj = ifelse(lm.pred > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred.adj)
58/(350+58)
