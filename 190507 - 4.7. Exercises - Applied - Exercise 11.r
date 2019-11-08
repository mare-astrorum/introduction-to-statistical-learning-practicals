# Exercise 11

library(ISLR)
attach(Auto)

dim(Auto)
summary(Auto)


### (a)

mpg.median = median(mpg) # calculate median

mpg01 = rep(0, length(mpg)) 
mpg01[mpg > mpg.median] = 1

Auto.df = data.frame(Auto, mpg01)
dim(Auto.df)


### (b)

pairs(Auto.df)
cor(Auto.df[,-9])


### (c)

train = (year%%2 == 0)
test = !train

Auto.train = Auto.df[train,]
dim(Auto.train)
Auto.test = Auto.df[test,]
dim(Auto.test)
mpg01.test = mpg01[test]


### (d)

library(MASS)

lda.fit = lda(mpg01~cylinders+displacement+horsepower+
                weight, data = Auto, subset = train)
lda.pred = predict(lda.fit, Auto.test)
lda.class = lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class != mpg01.test)

# Test error = 12.6%


### (e)

qda.fit = qda(mpg01~cylinders+displacement+horsepower+
                weight, data = Auto, subset = train)

qda.class = predict(qda.fit, Auto.test)$class
table(qda.class, mpg01.test)
mean(qda.class != mpg01.test)

# Test error = 13.2%


### (f)

glm.fits = glm(mpg01~cylinders+displacement+horsepower+
                 weight, data = Auto, family = binomial,
               subset = train)
summary(glm.fits)
glm.probs = predict(glm.fits, Auto.test, type="response")


glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > .5] = 1

table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test)

# Test error = 12.1%


### (g)

library(class)

train.X = cbind(cylinders,displacement,horsepower,
                  weight)[train,]
test.X = cbind(cylinders,displacement,horsepower,
               weight)[!train,]
train.mpg01 = mpg01[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=1)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test)

# Test error = 15.4%

set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=100)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test)

# Test error = 14.3%

set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=10)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test)

# Test error = 15.4%