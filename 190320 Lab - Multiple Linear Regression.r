library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)

lm.fit = lm(medv~., data=Boston)
summary(lm.fit)

?summary.lm

summary(lm.fit)$r.squared
summary(lm.fit)$sigma


library(car)
vif(lm.fit)


lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)

lm.fit1 = update(lm.fit, ~.-age)

summary(lm(medv~lstat*age, data=Boston))

attach(Boston)
summary(lm.fit2)
lm.fit2 = lm(medv~lstat+I(lstat^2))

lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow=c(2, 2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat, 5))
summary(lm.fit5)

summary(lm(medv~log(rm), data = Boston))