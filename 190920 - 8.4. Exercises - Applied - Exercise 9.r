# ### 9 ###
# 
# This problem involves the OJ data set 
# which is part of the ISLR package.

library(ISLR)
attach(OJ)
set.seed(1031)
?OJ
dim(OJ)

# (a) 
# Create a training set containing a 
# random sample of 800 obser-vations,
# and a test set containing the 
# remaining observations. 

train = sample(1:nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

 
# (b) 
# 
# Fit a tree to the
# training data, with Purchase as the 
# response and the other variables as
# predictors. Use the summary() function 
# to produce summary statistics about the
# tree, and describe the results obtained. 
# What is the training error rate? How
# many terminal nodes does the tree have? 

library(tree)
OJ.tree = tree(Purchase~., data = OJ.train)
summary(OJ.tree)

# Training error rate = 0.165
# Number of terminal nodes:  8


#   (c) 
# 
# Type in the name of the tree
# object in order to get a detailed text 
# output. Pick one of the terminal nodes,
# and interpret the information displayed. 

OJ.tree
# 8) LoyalCH < 0.0356415 57   10.07 MM ( 0.01754 0.98246 ) *
# split == LoyalCH < 0.0356415, 
# n == 57, 
# deviance == 10.07, 
# yval MM

# (d) 
# 
# Create a plot of the tree, and
# interpret the results. 

plot(OJ.tree)
text(OJ.tree, pretty = 0)
 
# (e) 
# 
# Predict the response on the test 
# data, and produce
# a confusion matrix comparing the test 
# labels to the predicted test labels.
# What is the test error rate? 

OJ.pred = predict(OJ.tree, newdata = OJ.test, type = "class")
table(OJ.pred, OJ.test$Purchase)
(16+26)/270
#0.156
   
#   (f) 
# 
# Apply the cv.tree() function to the training
# set in order to determine the optimal tree size. 

cv.OJ = cv.tree(OJ.tree)
cv.OJ

 
# (g) 
# 
# Produce a plot with tree
# size on the x-axis and cross-validated 
# classification error rate on the
# y-axis. 

plot(cv.OJ$size, cv.OJ$dev, type = "b")

# (h) 
# 
# Which tree size corresponds to 
# the lowest cross-validated classi-
#   fication error rate? 


# 5
   
#   (i) 
# 
# Produce a pruned tree corresponding 
# to the optimal
# tree size obtained using cross-validation. 
# If cross-validation does not lead
# to selection of a pruned tree, then 
# create a pruned tree with five terminal
# nodes. 

pruned.OJ = prune.tree(OJ.tree, best = 5)
plot(pruned.OJ)
text(pruned.OJ)


 
# (j) 
# 
# Compare the training error rates 
# between the pruned and un- pruned
# trees. Which is higher? 

summary(pruned.OJ)
# Misclassification error rate: 0.1762 = 141 / 800 
summary(OJ.tree)
# Misclassification error rate: 0.1662 = 133 / 800 

# Pruned is higher




  
#   (k) 
# 
# Compare the test error rates between the pruned
# and unpruned trees. Which is higher?

pred.pruned = predict(pruned.OJ, newdata = OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)

# 0.21

OJ.pred = predict(OJ.tree, newdata = OJ.test, type = "class")
table(OJ.pred, OJ.test$Purchase)
misclass.unpruned = sum(OJ.test$Purchase != OJ.pred)
misclass.unpruned/length(OJ.pred)

# 0.156

# Unpruned is better