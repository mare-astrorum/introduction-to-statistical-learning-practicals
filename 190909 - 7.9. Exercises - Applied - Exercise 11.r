# In Section 7.7, it was mentioned
# that GAMs are generally fit using
# a backfitting approach. The idea behind
# backfitting is actually quite
# simple. We will now explore backfitting
# in the context of multiple
# linear regression.
#
#
# Suppose that we would like to
# perform multiple linear regression, but
# we do not have software to do so.
# Instead, we only have software
# to perform simple linear regression.
# Therefore, we take the following
# iterative approach: we repeatedly
# hold all but one coefficient esti-
#   mate fixed at its current value,
# and update only that coefficient
# estimate using a simple linear
# regression. The process is continued un-
#   til convergence—that is, until
# the coefficient estimates stop changing.
# We now try this out on a toy example.
# 
# 
# 
# 
# (a) Generate a response Y and two 
# predictors X1 and X2 , with
# n = 100.

set.seed(1)
x1 = \rnorm(100)
x2 = rnorm(100)
eps = rnorm(100, sd = 0.1)

y = -2.1 + 1.3 * x1 + 0.54 * x2 + eps
# 
# 
# (b) Initialize β̂1 to take on a value 
# of your choice. It does not matter
# what value you choose.
# 
beta1 = 10
# 
# (c) Keeping β̂ 1 fixed, fit the model
# Y − β̂^1*X1 = β0 + β2*X2 + err.
# You can do this as follows:
#   
a = y - beta1 * x1
beta2 = lm(a~x2)$coef[2]



#beta0 = 4.82

# 
# 
# (d) Keeping β̂ 2 fixed, fit the model
# Y − β̂ 2*X2 = β0 + β1*X1 + err.
# You can do this as follows:
#   
a = y - beta2 * x2
beta1 = lm(a~x1)$coef[2]

# 
# 
# (e) Write a for loop to repeat (c) 
# and (d) 1,000 times. Report the
# estimates of β̂ 0 , β̂ 1 , and β̂ 2 
# at each iteration of the for loop.
# Create a plot in which each of these 
# values is displayed, with β̂ 0, β̂ 1,
# and β̂ 2 each shown in a different color.
# 
beta0 = rep(NA, 1000)
beta1 = rep(NA, 1000)
beta2 = rep(NA, 1000)
beta1[1] = 10
for (i in 1:1000){
  a = y - beta1[i] * x1
  beta2[i] = lm(a~x2)$coef[2]
  
  a = y - beta2[i] * x2
  if (i < 1000) {
    beta1[i+1] = lm(a~x1)$coef[2]
  }
  beta0[i] = lm(a~x1)$coef[1]
}

plot(1:1000, beta0, type = "l", col = "red", ylim = c(-2.3, 1.5),
     xlab = "Iteration", ylab = "Beta value")
lines(1:1000, beta1, col = "blue")
lines(1:1000, beta2, col = "green")
legend("right", c("beta0", "beta1", "beta2"), lty = 1,
       col = c("red", "blue", "green"))

# 
# (f) Compare your answer in (e) 
# to the results of simply performing
# multiple linear regression to 
# predict Y using X1 and X2 . Use
# the abline() function to overlay 
# those multiple linear regression
# coefficient estimates on the 
# plot obtained in (e).
 
a.mr = glm(y~x1 + x2)
abline(h = a.mr$coef[1], col=rgb(0, 0, 0, alpha = 0.4), lty="dashed",
      lwd=3)
abline(h = a.mr$coef[2], col=rgb(0, 0, 0, alpha = 0.4), lty="dashed",
       lwd=3)
abline(h = a.mr$coef[3], col=rgb(0, 0, 0, alpha = 0.4), lty="dashed",
       lwd=3)
legend("right", c("beta0", "beta1", "beta2", "multiple regression"),
       lty = c(1, 1, 1, 2),
       col = c("red", "blue", "green", "black"))


# (g) On this data set, how many 
# backfitting iterations were required
# in order to obtain a “good” 
# approximation to the multiple re-
#   gression coefficient estimates?

1