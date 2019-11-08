### Exercise 10 ###

### (a)

library(ISLR)
attach(Weekly)
names(Weekly)

dim(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly [,-9])
plot(Weekly$Volume)

# Appears that there's a relationship between 
# year and volume, correlation = 0.84


### (b)

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Weekly, family = binomial)
summary(glm.fits)

# Lag 2 is statistically significant


### (c)

glm.probs = predict(glm.fits, type="response")

glm.pred = rep("Down", 1089)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)
mean(glm.pred==Direction)
557/(557+430)
54/(54+48)

# The confusion matrix tells that in 56% cases 
# the prediction was made correctly. Also, 56%
# 'up' prediction were correct, and 53% 'down'
# predictions. This is only training data, so
# the test error rate is likely to be higher.


### (d)

train = (Year<2009)
Weekly.09.10 = Weekly[!train,]
dim(Weekly.09.10)
Direction.09.10 = Direction[!train]

glm.fits = glm(Direction~Lag2,
               data = Weekly, family = binomial,
               subset = train)
glm.probs = predict(glm.fits, Weekly.09.10, type="response")

glm.pred = rep("Down", 104)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.09.10)
mean(glm.pred == Direction.09.10)
mean(glm.pred != Direction.09.10)
56/(56+34)
9/14

# The confusion matrix tells that in 62% cases 
# the prediction was made correctly. Also, 62%
# 'up' prediction were correct, and 64% 'down'
# predictions. This is only training data, so
# the test error rate is likely to be higher.


### (e)

library(MASS)

lda.fit = lda(Direction~Lag2, 
              data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.09.10)
lda.class = lda.pred$class
table(lda.class, Direction.09.10)
mean(lda.class == Direction.09.10)

# 62.5% correct predictions


### (f)

qda.fit = qda(Direction~Lag2, 
              data = Weekly, subset = train)
qda.fit
qda.class = predict(qda.fit, Weekly.09.10)$class
table(qda.class, Direction.09.10)
mean(qda.class == Direction.09.10)

# 59% correct predictions


### (g)

library(class)

train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.09.10)
mean(knn.pred == Direction.09.10)

# 50% correct predictions


### (h)

# The best methods seem to be logistic
# regression and LDA