### Exercise 5

# In Chapter 4, we used logistic regression 
# to predict the probability of
# default using income and balance on the 
# Default data set. We will
# now estimate the test error of this logistic 
# regression model using the
# validation set approach. Do not forget to 
# set a random seed before
# beginning your analysis.


library(ISLR)
attach(Default)
summary(Default)
dim(Default)

# (a) Fit a logistic regression model that uses 
# income and balance to
# predict default.

glm.fits = glm(default~income+balance,
               data = Default, family = binomial)
summary(glm.fits)


# (b) Using the validation set approach, 
# estimate the test error of this
# model. In order to do this, you must 
# perform the following steps:

# i. Split the sample set into a training 
# set and a validation set.

set.seed(1)
train = sample(dim(Default)[1], dim(Default)[1]/2)
# take a sample of 10000 and pick 5000 random vars.


# ii. Fit a multiple logistic regression 
# model using only the training observations.

glm.fits.train = glm(default~income+balance,
               data = Default, family = binomial,
               subset = train)
summary(glm.fits.train)


# iii. Obtain a prediction of default status 
# for each individual in
# the validation set by computing the 
# posterior probability of
# default for that individual, and 
# classifying the individual to
# the default category if the posterior 
# probability is greater
# than 0.5.

glm.probs = predict(glm.fits.train,
                    Default[-train, ], type="response")


glm.pred = rep("No", length(default))
glm.pred[glm.probs > .5] = "Yes"


# iv. Compute the validation set error, 
# which is the fraction of
# the observations in the validation set 
# that are misclassified.


table(glm.pred, default)
mean(glm.pred != Default[-train, ]$default)
mean(glm.pred != default[-train])


# (c) Repeat the process in (b) three times, 
# using three different splits
# of the observations into a training set 
# and a validation set. Comment on the 
# results obtained.

PredDef = function(){
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  glm.fits.train = glm(default~income+balance,
                       data = Default, family = binomial,
                       subset = train)
  glm.probs = predict(glm.fits.train,
                      Default[-train, ], type="response")
  glm.pred = rep("No", length(default))
  glm.pred[glm.probs > .5] = "Yes"
  return(mean(glm.pred != Default[-train, ]$default))
}

PredDef()
PredDef()
PredDef()


# (d) Now consider a logistic regression model 
# that predicts the probability of default 
# using income , balance , and a dummy variable
# for student . Estimate the test error for 
# this model using the validation set approach. 
# Comment on whether or not including a
# dummy variable for student leads to a 
# reduction in the test error rate.


glm.fits.train = glm(default~income+balance+student,
                     data = Default, family = binomial,
                     subset = train)
summary(glm.fits.train)


glm.probs = predict(glm.fits.train,
                    Default[-train, ], type="response")


glm.pred = rep("No", length(default))
glm.pred[glm.probs > .5] = "Yes"


table(glm.pred, default)
mean(glm.pred != Default[-train, ]$default)

# Dummy variable doesn't seem to change the test 
# error rate.