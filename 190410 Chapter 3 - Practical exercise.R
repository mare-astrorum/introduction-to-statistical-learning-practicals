# Chapter 3, exercise 8

library(MASS)
library(ISLR)

attach(Auto)

lm.fit = lm(mpg~horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=c(98)),
        interval="confidence")

plot(horsepower, mpg)
abline(lm.fit)

par(mfrow = c(2,2))
plot(lm.fit)


# Chapter 3, exercise 9

pairs(Auto)

cor(subset(Auto, select=-name))

lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)

par(mfrow = c(2,2))
plot(lm.fit1)
par(mfrow = c(1,1))
plot(predict(lm.fit1), rstudent(lm.fit1))


summary(lm(mpg~displacement*weight*year*origin, data=Auto))
summary(lm(mpg~displacement*weight*year, data=Auto))
summary(lm(mpg~displacement*weight, data=Auto))

summary(lm(mpg~cylinders*displacement+displacement*weight, data = Auto))


summary(lm(mpg~weight+I(log(weight))))
summary(lm(mpg~weight+I(sqrt(weight))))
summary(lm(mpg~weight+I(weight^2)))


lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

par(mfrow = c(1,1))
plot(predict(lm.fit3), rstudent(lm.fit3))


# Chapter 3, exercise 10

library(ISLR)
summary(Carseats)
attach(Carseats)

lm.fit4 = lm(Sales~Price+Urban+US)
summary(lm.fit4)

lm.fit5 = lm(Sales~Price+US)
summary(lm.fit5)

confint(lm.fit5)

par(mfrow=c(2,2))
plot(lm.fit5)

par(mfrow = c(1,1))
plot(predict(lm.fit5), rstudent(lm.fit5))


# Chapter 3, Exercise 11

set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

# Linear regression without intercept
lm.fit6 = lm(y~x+0)
summary(lm.fit6)


lm.fit7 = lm(x~y+0)
summary(lm.fit7)


(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))


lm.fit8 = lm(y~x)
summary(lm.fit8)

lm.fit9 = lm(x~y)
summary(lm.fit9)


# Chapter 3, Exercise 12

# b
set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

# c

set.seed(1)
x = rnorm(100)
y = -sample(x)

summary(lm(y~x+0))
summary(lm(x~y+0))


# Chapter 3, exercise 13

# a
set.seed(1)
x = rnorm(100)

# b
eps = rnorm(100, mean = 0, sd = sqrt(0.25))

# c
y = -1 + 0.5*x + eps

# d
plot(x,y)
# looks like there is a linear relationship

# e
lm.fit10 = lm(y~x)
summary(lm.fit10)
# beta-0 and beta-1 fit almost perfectly

# f
plot(x, y)
abline(lm.fit10, lwd=3, col=2) # least squares line
abline(-1, 0.5, lwd=3, col=3) # population regression line
legend("bottomright", legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# g
lm.fit11 = lm(y~x+I(x^2))
summary(lm.fit11)
# x^2 has a small improvement effect

# h
set.seed(1)
x = rnorm(100)
eps = rnorm(100, mean = 0, sd = sqrt(0.1))
# Residual standard error is smaller and R-squared is larger

# i
set.seed(1)
x = rnorm(100)
eps = rnorm(100, mean = 0, sd = sqrt(0.5))
# Residual standard error is larger and R-squared is smaller

# j 
confint(lm.fit10)


# Chapter 3, exercise 14

# a

set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm (100) /10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm (100)

# y = 2 + 2x1 + 0.3x2 + e

# b 
cor(x1, x2)
# correlation is 0.8351212
plot(x1, x2)
# positive linear relationship

# c
lm.fit14 = lm(y~x1+x2)
summary(lm.fit14)

#beta-0 = 2.13
#beta-1 = 1.44
#beta-2 = 1

#beta-0 match well, beta-1 matches somewhat OK
#it's significant p=0.05, can reject H-0,
#beta-2 doesn't match well, value is insignificant
#cannot reject H-0


# d
lm.fit14d = lm(y~x1)
summary(lm.fit14d)

# beta-1 is much more significant now,
# can reject H-0


# e
lm.fit14e = lm(y~x2)
summary(lm.fit14e)
# beta-1 is much significant now,
# can reject H-0

# f
#No, they don't contradict each other.
#The values are highly collinear,
#so just one almost completely
#explains the other.

# g
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

par(mfrow = c(2,2))
plot(lm.fit14)
par(mfrow = c(1,1))
plot(predict(lm.fit14), rstudent(lm.fit14))

-- c -- repeated
#beta-0 = 2.13
#beta-1 = 1.44
#beta-2 = 1

#beta-0 match well, beta-1 doesn't match 
# cannot reject H-0,
# beta-2 match well, can reject H-0
# 
# Changes model, not really improves.
# High leverage point, not an outlier

--- d --- repeated
par(mfrow = c(2,2))
plot(lm.fit14d)
par(mfrow = c(1,1))
plot(predict(lm.fit14d), rstudent(lm.fit14d))

#beta-0 = 2.26
#beta-1 = 1.76

# beta-1 is again highly significant
# Model stays about the same
# Not a high leverage point, an outlier

--- e --- repeated
par(mfrow = c(2,2))
plot(lm.fit14e)
par(mfrow = c(1,1))
plot(predict(lm.fit14e), rstudent(lm.fit14e))

#beta-0 = 2.34
#beta-1 = 3.11

# beta-1 is again highly significant
# Model improved
# High leverage point, not an outlier


Chapter 3, Exercise 15

library(MASS)
library(ISLR)

summary(Boston)
attach(Boston)

a

lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas) 
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes

In all models there is a statistically
significant association, except chas

par(mfrow = c(2,2))
plot(lm.medv)
plot(lm.lstat)
plot(lm.black)
par(mfrow = c(1,1))

b

lm.fit15all = lm(crim~., data=Boston)
summary(lm.fit15all)

F-statistic is large and higly significant
H-0 can be rejected:
*** variables = dis, rad
** variables = medv
* variables = zn, black


c

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])

y = coefficients(lm.fit15all)[2:14]

plot(x, y)


d

# lm.zn3 = lm(crim~zn+I(zn^2)+I(zn^3))
lm.zn3poly = lm(crim~poly(zn,3))
summary(lm.zn3poly)

