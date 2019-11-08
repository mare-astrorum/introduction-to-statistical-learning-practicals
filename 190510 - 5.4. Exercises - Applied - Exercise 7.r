### Exercise 7 ###

# In Sections 5.3.2 and 5.3.3, we saw that 
# the cv.glm() function can be
# used in order to compute the LOOCV test 
# error estimate. Alternatively, one could compute 
# those quantities using just the glm() and
# predict.glm() functions, and a for loop. 
# You will now take this approach in order to 
# compute the LOOCV error for a simple logistic
# regression model on the Weekly data 
# set. Recall that in the context
# of classification problems, the LOOCV 
# error is given in (5.4).


# (a) Fit a logistic regression model 
# that predicts Direction using Lag1
# and Lag2 .

library(ISLR)
attach(Weekly)
summary(Weekly)
dim(Weekly)
set.seed(1)

glm.fit.weekly = glm(Direction~Lag1+Lag2,
                     data = Weekly,
                     family = binomial)

# (b) Fit a logistic regression model 
# that predicts Direction using Lag1
# and Lag2 using all but the first observation.


glm.fit.weekly = glm(Direction~Lag1+Lag2,
                     data = Weekly[-1, ],
                     family = binomial)

# (c) Use the model from (b) to predict 
# the direction of the first observation. 
# You can do this by predicting that the first observation
# will go up if P ( Direction="Up" | Lag1, Lag2 )>0.5. 
# Was this observation correctly classified?


glm.probs = predict(glm.fit.weekly,
                    Weekly[1, ], type="response")


glm.pred = "Down"
glm.pred[glm.probs > .5] = "Up"


mean(glm.pred == Weekly$Direction[1])
Weekly$Direction[1]


# (d) Write a for loop from i = 1 to i = n, 
# where n is the number of
# observations in the data set, that 
# performs each of the following
# steps:

# i. Fit a logistic regression model using
#  all but the ith observation to predict 
#  Direction using Lag1 and Lag2 .

# ii. Compute the posterior probability 
# of the market moving up
# for the ith observation.

# iii. Use the posterior probability for 
# the ith observation in order
# to predict whether or not the market 
# moves up.

# iv. Determine whether or not an error 
# was made in predicting
# the direction for the ith observation. 
# If an error was made,
# then indicate this as a 1, and otherwise 
# indicate it as a 0.

count = rep(0, dim(Weekly)[1])
for (i in 1:(dim(Weekly)[1])) {
	glm.fit.weekly = glm(Direction~Lag1+Lag2,
                     data = Weekly[-i, ],
                     family = binomial)
	glm.probs = predict(glm.fit.weekly,
                    Weekly[i, ], type="response")
	glm.pred = "Down"
	if(glm.probs > 0.5)
		glm.pred = "Up"
	if(glm.pred != Weekly$Direction[i])
		count[i] = 1

}
sum(count)


# (e) Take the average of the n numbers 
# obtained in (d)iv in order to
# obtain the LOOCV estimate for the 
# test error. Comment on the
# results.

mean(count)

# Test error is 45%