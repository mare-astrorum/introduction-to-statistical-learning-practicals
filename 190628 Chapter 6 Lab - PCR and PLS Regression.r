library(ISLR)
fix(Hitters)
Hitters = na.omit(Hitters)
dim(Hitters)

library(pls)
set.seed(2)
?pcr
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE,
              validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")


set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
dim(x)


set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, subset=train,
              scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred = predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred-y.test)^2)

pcr.fit = pcr(y~x, scale=TRUE, ncomp = 7)
summary(pcr.fit)



### Partial Least Squares ###

set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train,
               scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred = predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)


pls.fit = plsr(Salary~., data=Hitters, scale=TRUE,
               ncomp = 2)
summary(pls.fit)
