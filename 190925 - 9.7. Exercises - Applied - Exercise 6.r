# 6. 
# 
# At the end of Section 9.6.1, it is 
# claimed that in the case of data that
# is just barely linearly separable, 
# a support vector classifier with a
# small value of cost that misclassifies 
# a couple of training observations
# may perform better on test data than 
# one with a huge value of cost
# that does not misclassify any 
# training observations. You will now
# investigate this claim.
# 
# (a) 
# 
# Generate two-class data with p = 2 
# in such a way that the classes
# are just barely linearly separable.

set.seed(1)


x = matrix(rnorm(200*2), ncol=2)
y = c(rep(-1, 100), rep(1, 100))
x[y==1, ] = x[y==1, ] + 2
plot(x, col=(3-y))



# (b) 
# 
# Compute the cross-validation 
# error rates for support vector
# classifiers with a range of cost 
# values. How many training er-
#   rors are misclassified for each 
# value of cost considered, and how
# does this relate to the cross-validation 
# errors obtained?

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 0.01, scale = F)
plot(svmfit, dat)

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel="linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 
                                       100, 1000, 10000)))
summary(tune.out)
misclass = tune.out$performances$error
misclass*200

# - Detailed performance results:
#   cost error dispersion
# 1 1e-03 0.410 0.28460499
# 2 1e-02 0.000 0.00000000
# 3 1e-01 0.005 0.01581139
# 4 1e+00 0.005 0.01581139
# 5 5e+00 0.005 0.01581139
# 6 1e+01 0.000 0.00000000
# 7 1e+02 0.000 0.00000000

   
#   (c) 
# 
# Generate an appropriate test data 
# set, and compute the test
# errors corresponding to each 
# of the values of cost considered.
# Which value of cost leads to the 
# fewest test errors, and how
# does this compare to the values 
# of cost that yield the fewest
# training errors and the fewest 
# cross-validation errors?

xtest = matrix(rnorm(200*2), ncol=2)
ytest = sample(c(-1, 1), 200, rep = T)
xtest[ytest==1, ] = xtest[ytest==1, ] + 3.75
testdat = data.frame(x=xtest, y=as.factor(ytest))

tune.out.test = tune(svm, y~., data = testdat, kernel="linear",
                ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out.test)

# cost error dispersion
# 1 1e-03 0.460 0.14869805
# 2 1e-02 0.010 0.02108185
# 3 1e-01 0.010 0.02108185
# 4 1e+00 0.010 0.02108185
# 5 5e+00 0.015 0.02415229
# 6 1e+01 0.015 0.02415229
# 7 1e+02 0.025 0.02635231

   
#   (d) 
# 
# Discuss your results.

# Although large cost values got no training errors, in the test
# set they quickly got lots of errors.






set.seed(3154)
# Class one
x.one = runif(500, 0, 90)
x.one[1:20]
y.one = runif(500, x.one + 10, 100)
y.one[1:20]
x.one.noise = runif(50, 20, 80)
x.one.noise[1:20]
y.one.noise = 5/4 * (x.one.noise - 10) + 0.1
y.one.noise[1:20]

# Class zero
x.zero = runif(500, 10, 100)
y.zero = runif(500, 0, x.zero - 10)
x.zero.noise = runif(50, 20, 80)
y.zero.noise = 5/4 * (x.zero.noise - 10) - 0.1

# Combine all
class.one = seq(1, 550)
x = c(x.one, x.one.noise, x.zero, x.zero.noise)
y = c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)


library(e1071)
set.seed(555)
z = rep(0, 1100)
z[class.one] = 1
data = data.frame(x = x, y = y, z = z)
tune.out = tune(svm, as.factor(z) ~ ., data = data, kernel = "linear", 
                ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)
data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 
             1100)

# - Detailed performance results:
#   cost      error dispersion
# 1 1e-02 0.06000000 0.02542567
# 2 1e-01 0.04909091 0.02235041
# 3 1e+00 0.04909091 0.02235041
# 4 5e+00 0.04909091 0.02235041
# 5 1e+01 0.04909091 0.02235041
# 6 1e+02 0.05000000 0.02317736
# 7 1e+03 0.03454545 0.01954487
# 8 1e+04 0.00000000 0.00000000
# 
# > data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 
#                +              1100)
# cost misclass
# 1 1e-02       66
# 2 1e-01       54
# 3 1e+00       54
# 4 5e+00       54
# 5 1e+01       54
# 6 1e+02       55
# 7 1e+03       38
# 8 1e+04        0


set.seed(1111)
x.test = runif(1000, 0, 100)
class.one = sample(1000, 500)
y.test = rep(NA, 1000)
# Set y > x for class.one
for (i in class.one) {
  y.test[i] = runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {
  y.test[i] = runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)



set.seed(30012)
z.test = rep(0, 1000)
z.test[class.one] = 1
all.costs = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.errors = rep(NA, 8)
data.test = data.frame(x = x.test, y = y.test, z = z.test)
for (i in 1:length(all.costs)) {
  svm.fit = svm(as.factor(z) ~ ., data = data, kernel = "linear", cost = all.costs[i])
  svm.predict = predict(svm.fit, data.test)
  test.errors[i] = sum(svm.predict != data.test$z)
}

data.frame(cost = all.costs, `test misclass` = test.errors)
