# 5. 
# 
# We have seen that we can fit an SVM 
# with a non-linear kernel in order
# to perform classification using a 
# non-linear decision boundary. We will
# now see that we can also obtain a 
# non-linear decision boundary by
# performing logistic regression using 
# non-linear transformations of the
# features.
# 
# (a) 
# 
# Generate a data set with n = 500 
# and p = 2, such that the obser-
#   vations belong to two classes with 
# a quadratic decision boundary
# between them. For instance, you 
# can do this as follows:
#   
#   > x1 = runif (500) -0.5
# > x2 = runif (500) -0.5
# > y =1*( x1 ^2 - x2 ^2 > 0)

?runif
set.seed(421)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1 * (x1^2 - x2^2 > 0)

# (b) 
# 
# Plot the observations, colored 
# according to their class labels.
# Your plot should display X 1 on 
# the x-axis, and X 2 on the y-
#   axis.

plot(x1, x2, col = y+1)

# (c) 
# 
# Fit a logistic regression model to 
# the data, using X 1 and X 2 as
# predictors.

dat = data.frame(x1 = x1, x2 = x2, y = y)
glm.fits = glm(y~., data = dat, family = binomial)

summary(glm.fits) 

# (d) 
# 
# Apply this model to the training 
# data in order to obtain a pre-
#   dicted class label for each training 
# observation. Plot the ob-
#   servations, colored according to the 
# predicted class labels. The
# decision boundary should be linear.


pred.log = predict(glm.fits, dat, type = "response")
pred.cat = ifelse(pred.log > 0.52, 1, 0)
data.pos = dat[pred.cat == 1, ]
data.neg = dat[pred.cat == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red")

# (e) 
# 
# Now fit a logistic regression model 
# to the data using non-linear
# functions of X 1 and X 2 as predictors 
# (e.g. X 1 2 , X 1 ×X 2 , log(X 2 ),
#   and so forth).

glm.fits = glm(y~poly(x1, 2) + x2, data = dat, family = binomial)
pred.log = predict(glm.fits, dat, type = "response")
pred.cat = ifelse(pred.log > 0.52, 1, 0)
data.pos = dat[pred.cat == 1, ]
data.neg = dat[pred.cat == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red")

glm.fits = glm(y~I(x1 * x2), data = dat, family = binomial)
pred.log = predict(glm.fits, dat, type = "response")
pred.cat = ifelse(pred.log > 0.52, 1, 0)
data.pos = dat[pred.cat == 1, ]
data.neg = dat[pred.cat == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red")

glm.fits = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = dat, family = binomial)
pred.log = predict(glm.fits, dat, type = "response")
pred.cat = ifelse(pred.log > 0.52, 1, 0)
data.pos = dat[pred.cat == 1, ]
data.neg = dat[pred.cat == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red")

# (f) 
# 
# Apply this model to the training 
# data in order to obtain a pre-
#   dicted class label for each training 
# observation. Plot the ob-
#   servations, colored according to the 
# predicted class labels. The
# decision boundary should be obviously 
# non-linear. If it is not,
# then repeat (a)-(e) until you come 
# up with an example in which
# the predicted class labels are 
# obviously non-linear.
# 
# 
# (g) 
# 
# Fit a support vector classifier 
# to the data with X 1 and X 2 as
# predictors. Obtain a class prediction 
# for each training observa-
#   tion. Plot the observations, 
# colored according to the predicted
# class labels.

library(e1071)
svmfit.lin = svm(as.factor(y)~., data = dat, kernel = "linear", cost = 10)
#svmfit.lin = svm(y~x1+x2, kernel = "linear", cost = 10, 
#                 scale = F, decision.values = T)
pred.svm = predict(svmfit.lin, dat)
data.pos = dat[pred.svm == 1, ]
data.neg = dat[pred.svm == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red")


 
# (h) 
# 
# Fit a SVM using a non-linear kernel 
# to the data. Obtain a class
# prediction for each training 
# observation. Plot the observations,
# colored according to the predicted 
# class labels

svmfit.poly = svm(as.factor(y)~., data = dat, kernel = "polynomial",
                  cost = 10, degree = 5, gamma = 1)
#svmfit.lin = svm(y~x1+x2, kernel = "linear", cost = 10, 
#                 scale = F, decision.values = T)
pred.svm = predict(svmfit.poly, dat)
data.pos = dat[pred.svm == 1, ]
data.neg = dat[pred.svm == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red") 

svmfit.rad = svm(as.factor(y)~x1 + x2, data = dat, gamma = 1, cost = 10)
#svmfit.lin = svm(y~x1+x2, kernel = "linear", cost = 10, 
#                 scale = F, decision.values = T)
pred.svm = predict(svmfit.rad, dat)
data.pos = dat[pred.svm == 1, ]
data.neg = dat[pred.svm == 0, ]
plot(data.pos$x1, data.pos$x2)
points(data.neg$x1, data.neg$x2, col = "red") 
 
# (i) 
# 
# Comment on your results.