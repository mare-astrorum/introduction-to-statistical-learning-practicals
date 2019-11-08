# In this exercise, we will generate 
# simulated data, and will then use
# this data to perform best subset selection.

# (a) Use the rnorm() function to 
# generate a predictor X of length
# n = 100, as well as a noise 
# vector eps of length n = 100.

?rnorm
set.seed(1)
x = rnorm(100)
eps = rnorm(100)

b0 = 3
b1 = 2
b2 = -3
b3 = 0.3



# (b) Generate a response vector Y of length 
# n = 100 according tothe model
# Y = β 0 + β 1 X + β 2 X 2 + β 3 X 3 + eps,
# where β 0 , β 1 , β 2 , and β 3 
# are constants of your choice.

y = b0 + (b1*x) + (b2*x^2) + (b3*x^3) + eps


# (c) Use the regsubsets() function 
# to perform best subset selection in
# order to choose the best model 
# containing the predictors X, X 2 , . .
# . , X 10 . What is the best model 
# obtained according to C p , BIC, and
# adjusted R 2 ? Show some plots 
# to provide evidence for your answer,
# and report the coefficients of the 
# best model ob- tained. Note you
# will need to use the data.frame() 
# function to create a single data set
# containing both X and Y .

library(leaps)
?data.frame
data.full = data.frame(x=x, y=y)
?regsubsets
?poly
regfit.full = regsubsets(y~poly(x, 10, raw=T),
                         data=data.full, nvmax=10)
reg.summary = summary(regfit.full)

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type = "l")

# Adjusted R^2
plot(reg.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(3, reg.summary$adjr2[3], col="red", cex=2,
       pch=20)

# Cp

plot(reg.summary$cp, xlab = "Number of Variables", 
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(3, reg.summary$cp[3], col="red", cex=2,
       pch=20)

# BIC

plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(3, reg.summary$bic[3], col="red", cex=2,
       pch=20)

# All 3 models select 3 as best number of variables

coef(regfit.full, 3)


# (d) Repeat (c), using forward stepwise 
# selection and also using back-
# wards stepwise selection. How 
# does your answer compare to the
# results in (c)?

regfit.fwd = regsubsets(y~poly(x, 10, raw=T),
                         data=data.full, nvmax=10,
                         method = "forward")
regfit.bwd = regsubsets(y~poly(x, 10, raw=T),
                         data=data.full, nvmax=10, 
                         method = "backward")
fwd.summary = summary(regfit.fwd)
bwd.summary = summary(regfit.bwd)

which.max(fwd.summary$adjr2)
which.max(bwd.summary$adjr2)

which.min(fwd.summary$cp)
which.min(bwd.summary$cp)

which.min(fwd.summary$bic)
which.min(bwd.summary$bic)

# Plots

par(mfrow = c(2, 3))

plot(fwd.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
points(3, fwd.summary$adjr2[3], col="red", cex=2,
       pch=20)
plot(fwd.summary$cp, xlab = "Number of Variables", 
     ylab = "Cp", type = "l")
points(3, fwd.summary$cp[3], col="red", cex=2,
       pch=20)
plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab = "BIC", type = "l")
points(3, fwd.summary$bic[3], col="red", cex=2,
       pch=20)

plot(bwd.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
points(3, bwd.summary$adjr2[3], col="red", cex=2,
       pch=20)
plot(bwd.summary$cp, xlab = "Number of Variables", 
     ylab = "Cp", type = "l")
points(3, bwd.summary$cp[3], col="red", cex=2,
       pch=20)
plot(bwd.summary$bic, xlab = "Number of Variables", 
     ylab = "BIC", type = "l")
points(3, bwd.summary$bic[3], col="red", cex=2,
       pch=20)

# Both for forward and backward selection
# the same amount of variables was selected

coef(regfit.fwd, 3)
coef(regfit.bwd, 3)

# Forward selection selects poly7, 
# backward selection - poly9


# (e) Now fit a lasso model to the simulated 
# data, again using X, X 2 ,
# . . . , X 10 as predictors. Use cross-
# validation to select the optimal
# value of λ. Create plots of the cross-
# validation error as a function
# of λ. Report the resulting coefficient 
# estimates, and discuss the
# results obtained.

library(glmnet)

x = model.matrix(y~poly(x, 10, raw=T), data.full)[,-1]
y = data.full$y


set.seed(1)
cv.out=cv.glmnet(x, y, alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

out=glmnet(x, y, alpha=1)
lasso.coef = predict(out, type = "coefficients", s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]


# (f) Now generate a response 
# vector Y according to the model
# Y = β 0 + β 7 X 7 + eps,
# and perform best subset selection 
# and the lasso. Discuss the
# results obtained.

b7 = 7 
y7 = b0 + (b7*x^7) + eps


# Best subset selection

library(leaps)

data.full.y7 = data.frame(x=x, y=y7)

regfit.y7 = regsubsets(y7~poly(x, 10, raw=T), data.full)
y7.summary = summary(regfit.y7)

# Plots

par(mfrow = c(2, 2))
plot(y7.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type = "l")

plot(y7.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
which.max(y7.summary$adjr2)
points(4, y7.summary$adjr2[4], col="red", cex=2,
       pch=20)

plot(reg.summary$cp, xlab = "Number of Variables", 
     ylab = "Cp", type = "l")
which.min(y7.summary$cp)
points(2, y7.summary$cp[2], col="red", cex=2,
       pch=20)

plot(y7.summary$bic, xlab = "Number of Variables", 
     ylab = "BIC", type = "l")
which.min(y7.summary$bic)
points(1, y7.summary$bic[1], col="red", cex=2,
       pch=20)

coef(regfit.y7, 4)
coef(regfit.y7, 2)
coef(regfit.y7, 1)
