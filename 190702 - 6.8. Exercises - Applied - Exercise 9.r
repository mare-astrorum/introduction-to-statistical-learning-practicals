### Chapter 6, Exercise 9 ###


# In this exercise, we will predict 
# the number of applications received
# using the other variables in the 
# College data set.

# (a) Split the data set into a 
# training set and a test set.

library(ISLR)
fix(College)
attach(College)
names(College)
?College


x = model.matrix(Apps~., College)[,-1]
y = College$Apps

set.seed(11)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
College.train = College[train, ]
College.test = College[test, ]


# (b) Fit a linear model using least 
# squares on the training set, and
# report the test error obtained.

linear.mod = lm(Apps~., College.train)
summary(linear.mod)
lm.pred = predict(linear.mod, College.test, 
	type="response")
mean((lm.pred-y.test)^2)

# (c) Fit a ridge regression model 
# on the training set, with λ chosen
# by cross-validation. Report 
# the test error obtained.

library(glmnet)

set.seed(1)
grid = 10^seq(10, -2, length = 100)
cv.out = cv.glmnet(x[train,], y[train], 
                   lambda=grid, alpha=0)
plot(cv.out)
bestlam.ridge = cv.out$lambda.min
bestlam.ridge

ridge.mod = glmnet(x[train,], y[train], alpha=0,
                   lambda=bestlam.ridge)
ridge.pred = predict(ridge.mod, s=bestlam.ridge, newx=x[test,])
mean((ridge.pred-y.test)^2)


# (d) Fit a lasso model on the 
# training set, with λ chosen by cross-
# validation. Report the test error 
# obtained, along with the num-
# ber of non-zero coefficient estimates.

library(glmnet)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam.lasso = cv.out$lambda.min
bestlam.lasso

lasso.mod = glmnet(x[train,], y[train], alpha=1,
                   lambda=bestlam.lasso)
lasso.pred = predict(lasso.mod, s=bestlam.lasso,
                     newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x, y, alpha=1, lambda=bestlam.lasso)
lasso.coef = predict(out, type = "coefficients", s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

# (e) Fit a PCR model on the 
# training set, with M chosen by cross-
# validation. Report the test 
# error obtained, along with the value
# of M selected by cross-validation.

library(pls)

set.seed(1)
pcr.fit = pcr(Apps~., data=College.train,
              scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred = predict(pcr.fit, x[test,], ncomp = 5)
mean((pcr.pred-y.test)^2)

# (f) Fit a PLS model on the 
# training set, with M chosen by cross-
# validation. Report the test 
# error obtained, along with the value
# of M selected by cross-validation.

library(pls)

set.seed(1)
plsr.fit = plsr(Apps~., data=College.train,
              scale=TRUE, validation="CV")
validationplot(plsr.fit, val.type = "MSEP")

plsr.pred = predict(plsr.fit, x[test,], ncomp = 10)
mean((plsr.pred-y.test)^2)


# (g) Comment on the results 
# obtained. How accurately can we pre-
# dict the number of college 
# applications received? Is there much
# difference among the test errors 
# resulting from these five ap-
# proaches?

# Can predict very accurately, not much
# difference between approaches.