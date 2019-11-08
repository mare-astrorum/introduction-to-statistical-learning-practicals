# Exercise 13

# Using the Boston data set, fit classification 
# models in order to predict
# whether a given suburb has a crime rate above 
# or below the median.
# Explore logistic regression, LDA, and KNN 
# models using various sub-
# sets of the predictors. Describe your findings.

library(MASS)
attach(Boston)

dim(Boston)
summary(Boston)


### Calculate crime rate median

crim.median = median(crim)
crim.median

crim01 = rep(0, length(crim)) 
crim01[crim > crim.median] = 1
crim01[1:20]
crim[1:20]

Boston.df = data.frame(Boston, crim01)
dim(Boston.df)


### Explore Boston data

pairs(Boston.df)
cor(Boston.df)


### Create a training and a test set

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]

Boston.train = Boston.df[train,]
dim(Boston.train)
Boston.test = Boston.df[test,]
dim(Boston.test)
crim01.test = crim01[test]


### LDA

lda.fit = lda(crim01~indus+nox+age+dis+rad+tax,
              data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
lda.class = lda.pred$class
table(lda.class, crim01.test)
mean(lda.class != crim01.test)

# Test error = 10.7%


### QDA

qda.fit = qda(crim01~indus+nox+age+dis+rad+tax,
              data = Boston, subset = train)

qda.class = predict(qda.fit, Boston.test)$class
table(qda.class, crim01.test)
mean(qda.class != crim01.test)

# Test error = 62.1%


### Logistic

glm.fits = glm(crim01~indus+nox+age+dis+rad+tax,
               data = Boston, family = binomial,
               subset = train)
summary(glm.fits)
glm.probs = predict(glm.fits, Boston.test, type="response")


glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > .5] = 1

table(glm.pred, crim01.test)
mean(glm.pred != crim01.test)

# Test error = 9.1%


### KNN

library(class)

train.X = cbind(indus,nox,age,dis,rad,tax)[train, ]
test.X = cbind(indus,nox,age,dis,rad,tax)[test, ]
train.crim01 = crim01[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.crim01, k=1)
table(knn.pred, crim01.test)
mean(knn.pred != crim01.test)

# Test error = 62.8%

set.seed(1)
knn.pred = knn(train.X, test.X, train.crim01, k=10)
table(knn.pred, crim01.test)
mean(knn.pred != crim01.test)

# Test error = 11.9%

set.seed(1)
knn.pred = knn(train.X, test.X, train.crim01, k=100)
table(knn.pred, crim01.test)
mean(knn.pred != crim01.test)

# Test error = 64.4%

# Logistic regression has the least error, followed
# by LDA