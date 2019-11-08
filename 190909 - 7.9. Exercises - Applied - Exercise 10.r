# This question relates to the College data set.

library(ISLR)
attach(College)
library(leaps)
?College



### A ###
# Split the data into a training set 
# and a test 
# set. Using out-of-state
# tuition as the response and the 
# other variables as the predictors,
# perform forward stepwise selection 
# on the training set in order
# to identify a satisfactory model 
# that uses just a subset of the
# predictors.

set.seed(1)

train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]


regfit.fwd = regsubsets(Outstate~., data = College.train,
                        nvmax = 17, method = "forward")
reg.summary = summary(regfit.fwd)

par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
reg.coef = coef(regfit.fwd, 6)
names(reg.coef)


### B ###

# Fit a GAM on the training data, 
# using out-of-state tuition as
# the response and the features 
# selected in the previous step as
# the predictors. Plot the results, a
# nd explain your findings.

?College
library(gam)
gam.fit = gam(Outstate ~ Private + s(Room.Board, 3) + s(Terminal, 3) +
            s(perc.alumni, 3) + s(Expend, 3) + s(Grad.Rate),
          data = College.train)
par(mfrow = c(2, 3))
plot(gam, se=T, col="blue")


### C ###
# Evaluate the model obtained on 
# the test set, and explain the
# results obtained.

gam.predict = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.predict)^2)
gam.err


gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss


### D ###
# For which variables, if any, 
# is there evidence of a non-linear
# relationship with the response?

summary(gam.fit)

# Expend and Grad.Rate