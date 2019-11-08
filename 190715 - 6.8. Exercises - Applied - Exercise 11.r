# 11. We will now try to predict per 
# capita crime rate in the Boston dataset.

library(MASS)
library(ISLR)
attach(Boston)
fix(Boston)
names(Boston)


# (a) Try out some of the regression 
# methods explored in this chapter,
# such as best subset selection, the 
# lasso, ridge regression, and
# PCR. Present and discuss results 
# for the approaches that you
# consider.

# Best Subset Selection

sum(is.na(Boston$crim))
Boston = na.omit(Boston)
dim(Boston)
sum(is.na(Boston$crim))

library(leaps)
regfit.full = regsubsets(crim~., Boston)
summary(regfit.full)

regfit.full = regsubsets(crim~., data = Boston, 
                         nvmax = 13)
?regsubsets
reg.summary = summary(regfit.full)
reg.summary

which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)



# Lasso

x.lasso = model.matrix(crim~., Boston)[,-1]
y.lasso = Boston$crim
grid = 10^seq(10, -2, length = 100)

set.seed(1)
train.lasso = sample(1:nrow(x.lasso), nrow(x.lasso)/2)
test.lasso = (-train.lasso)
y.lasso.test = y.lasso[test]
dim(x)


# Using cross validation to select lambda

set.seed(1)
cv.out = cv.glmnet(x.lasso[train.lasso,], y.lasso[train.lasso], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

# The Lasso

lasso.mod = glmnet(x.lasso[train.lasso,], y.lasso[train.lasso], alpha=1, lambda=grid)
plot(lasso.mod)

lasso.pred = predict(lasso.mod, s=bestlam, newx=x.lasso[test.lasso,])
mean((lasso.pred-y.lasso.test)^2)

out=glmnet(x.lasso, y.lasso, alpha=1, lambda = bestlam)
lasso.coef = predict(out, type = "coefficients", s=bestlam)[1:14,]
lasso.coef
lasso.coef[lasso.coef!=0]


# PCR

x = model.matrix(crim~., Boston)[,-1]
y = Boston$crim

library(pls)
set.seed(1)
?pcr
pcr.fit = pcr(crim~., data=Boston, scale=TRUE,
              validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")


set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
dim(x)


set.seed(1)
pcr.fit = pcr(crim~., data=Boston, subset=train,
              scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred = predict(pcr.fit, x[test,], ncomp = 3)
mean((pcr.pred-y.test)^2)

pcr.fit = pcr(y~x, scale=TRUE, ncomp = 3)
summary(pcr.fit)



# (b) Propose a model (or set of models) 
# that seem to perform well on
# this data set, and justify your answer. 
# Make sure that you are
# evaluating model performance using 
# validation set error, cross-
# validation, or some other reasonable 
# alternative, as opposed to
# using training error.

# (c) Does your chosen model involve all 
# of the features in the data
# set? Why or why not?