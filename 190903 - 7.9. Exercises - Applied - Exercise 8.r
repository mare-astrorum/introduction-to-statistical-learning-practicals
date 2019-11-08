library(ISLR)
attach(Auto)

?Auto

# Poly

fit = lm(weight~poly(horsepower, 4), data=Auto)
coef(summary(fit))


fit.1 = lm(weight~horsepower, data=Auto)
fit.2 = lm(weight~poly(horsepower, 2), data=Auto)
fit.3 = lm(weight~poly(horsepower, 3), data=Auto)
fit.4 = lm(weight~poly(horsepower, 4), data=Auto)
fit.5 = lm(weight~poly(horsepower, 5), data=Auto)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

hplims = range(horsepower)
hp.grid = seq(from=hplims[1], to=hplims[2])
preds = predict(fit.3, newdata = list(horsepower=hp.grid), se=T)
se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit
                 -2*preds$se.fit)


plot(weight~horsepower, data=Auto, xlim=hplims, cex=0.5, col="darkgrey")
title("Degree 3 Polynomial", outer=T)
lines(hp.grid, preds$fit, lwd=2, col="blue")
matlines(hp.grid, se.bands, lwd=1, col="blue", lty=3)

# Step

fit_step = lm(weight~cut(horsepower,4), data=Auto)
coef(summary(fit_step))

plot(weight~horsepower, data=Auto, col="darkgrey")
lm.pred = predict(fit_step, data.frame(horsepower=hp.grid))
lines(hp.grid, lm.pred, col="blue", lwd=2)



# Cubic spline

library (splines)

fit.cubic = lm(weight~bs(horsepower, df=6), data=Auto)
pred = predict(fit.cubic, newdata=list(horsepower=hp.grid), se=T)

lines(hp.grid, pred$fit.cubic, lwd=2)
plot(horsepower, weight, col="gray")
lines(hp.grid, pred$fit, lwd=2)
lines(hp.grid, pred$fit+2*pred$se, lty="dashed")
lines(hp.grid, pred$fit-2*pred$se, lty="dashed")

summary(fit.cubic)
attr(bs(horsepower, df=6), "knots")

# Natural spline

fit.ns = lm(weight~ns(horsepower, df=4), data=Auto)
pred.ns = predict(fit.ns, newdata = list(horsepower=hp.grid), se=T)
lines(hp.grid, pred.ns$fit, col="red", lwd=2)

# Smoothing spline

fit.ssp = smooth.spline(horsepower, weight, cv=TRUE)
fit.ssp$df
lines(fit.ssp, col="green", lwd=2)



# Local regression

fit.lr = loess(weight~horsepower, span=.2, data=Auto)
fit.lr2 = loess(weight~horsepower, span=.5, data=Auto)
lines(hp.grid, predict(fit.lr, data.frame(horsepower=hp.grid)),
      col="orange", lwd=2)
lines(hp.grid, predict(fit.lr2, data.frame(horsepower=hp.grid)),
      col="purple", lwd=2)

# GAM

library(foreach)
?Auto
pairs(Auto)
gam1 = lm(weight~ns(mpg, 4) + ns(horsepower, 3) + cylinders, data = Auto)

library(gam)
gam2 = gam(weight~ns(mpg, 4) + ns(horsepower, 3) + cylinders, data = Auto)

par(mfrow = c(1, 3))
plot(gam2, se=T, col="blue")

plot.Gam(gam1, se=T, col="red")

# ANOVA for different GAMs

gam.m1 = gam(weight~s(horsepower,3) + cylinders, data = Auto) # excludes year
gam.m2 = gam(weight~mpg + s(horsepower,3) + cylinders, data = Auto) # linear function of year
gam.m3 = gam(weight~s(mpg, 4) + s(horsepower, 3) + cylinders, data = Auto) # spline function for year

anova(gam.m1, gam.m2, gam.m3, test="F")


summary(gam.m3)



