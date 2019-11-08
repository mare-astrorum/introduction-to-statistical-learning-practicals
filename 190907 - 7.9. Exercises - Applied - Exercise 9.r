# This question uses the variables dis
# (the weighted mean of distances
# to five Boston employment centers) 
# and nox (nitrogen oxides concen-
# tration in parts per 10 million) 
# from the Boston data. We will treat
# dis as the predictor and nox as 
# the response.


library(MASS)
attach(Boston)


### A ###

Use the poly() function to fit a 
cubic polynomial regression to
predict nox using dis . Report 
the regression output, and plot
then resulting data and polynomial 
fits.


fit = lm(nox~poly(dis, 3), data=Boston)
coef(summary(fit))

dis.lim = range(dis)
disgrid = seq(from=dis.lim[1], to=dis.lim[2], by=0.1)


pred = predict(fit, newdata = list(dis=disgrid), se=T)
se.bands = cbind(pred$fit+2*pred$se.fit, pred$fit
                 -2*pred$se.fit)

plot(dis, nox, col="grey")
title("Degree 3 Polynomial")
lines(disgrid, pred$fit, lwd=2, col="blue")
matlines(disgrid, se.bands, lwd=1, col="blue", lty=3)



### B ###

Plot the polynomial fits for a 
range of different polynomial
degrees (say, from 1 to 10), 
and report the associated residual
sum of squares.

set.seed(1)
library(boot)
all.rss = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(nox~poly(dis, i), data=Boston)
  all.rss[i] = sum(glm.fit$residuals^2)
}

all.rss
which.min(all.rss)

  
### C ###

Perform cross-validation or another
approach to select the opti-
mal degree for the polynomial, 
and explain your results.

all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(nox~poly(dis, i), data=Boston)
    all.deltas[i] = cv.glm(Boston, glm.fit, K=10)$delta[2]
}

which.min(all.deltas)
#4


plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)
points(4, all.deltas[4], col="red")


### D ###

Use the bs() function to fit a 
regression spline to predict nox
using dis . Report the output 
for the fit using four degrees of
freedom. How did you choose the 
knots? Plot the resulting fit.

library(splines)
fit.bs = lm(nox~bs(dis, df=4, knots = c(4, 7, 11)), data=Boston)
summary(fit.bs)

pred = predict(fit.bs, newdata = list(dis = disgrid), se=T)

plot(nox~dis, col = "darkgrey")
lines(disgrid, pred$fit, col="blue")
lines(disgrid, pred$fit + 2*pred$se, col="blue", lty = "dashed")
lines(disgrid, pred$fit - 2*pred$se, col="blue", lty = "dashed")

attr(bs(dis, df=4), "knots")


### E ###

Now fit a regression spline for 
a range of degrees of freedom, and
plot the resulting fits and report 
the resulting RSS. Describe the
results obtained.

all.rss = rep(NA, 16)
for (i in 3:16) {
  fit.bs = lm(nox~bs(dis, df=i), data=Boston)
  all.rss[i] = sum(fit.bs$residuals^2)
}

which.min(all.rss)

plot(1:16, all.rss, lty="solid")


### F ###

Perform cross-validation or 
another approach in order to select
the best degrees of freedom 
for a regression spline on this data.
Describe your results.

all.cvs = rep(NA, 100)
for (i in 3:100){
  fit.bs = glm(nox~bs(dis, df=i), data=Boston)
  all.cvs[i] = cv.glm(Boston, fit.bs, K=10)$delta[2]
}

which.min(all.cvs)

plot(3:100, all.cvs[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")
points(11, all.cvs[11])
