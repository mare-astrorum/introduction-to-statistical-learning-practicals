### Exercise 8 ###

# 8. We will now perform cross-validation on a simulated data set.


# (a) Generate a simulated data set as follows:

set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

# In this data set, what is n and what is p? Write out the model
# used to generate the data in equation form.

n = 100
p = 2



# (b) Create a scatterplot of X against Y. Comment on what you find.

plot(x, y)

# There is a non-linear relationship
# between the data points.



# (c) Set a random seed, and then compute the LOOCV errors that
# result from fitting the following four models using least squares:


# Note you may find it helpful to use the data.frame() function
# to create a single data set containing both X and Y.

library(boot)
Data = data.frame(x, y)
set.seed(1)

# i. Y = β 0 + β 1 X + err
glm.fit = glm(y~x, data=Data)
cv.err = cv.glm(Data, glm.fit)
cv.err$delta

# ii. Y = β 0 + β 1 X + β 2 X^2 + err
glm.fit2 = glm(y~poly(x, 2), data=Data)
cv.err = cv.glm(Data, glm.fit2)
cv.err$delta

# iii. Y = β 0 + β 1 X + β 2 X^2 + β 3 X^3 + err
glm.fit3 = glm(y~poly(x, 3), data=Data)
cv.err = cv.glm(Data, glm.fit3)
cv.err$delta

# iv. Y = β 0 + β 1 X + β 2 X^2 + β 3 X^3 + β 4 X^4 + err
glm.fit4 = glm(y~poly(x, 4), data=Data)
cv.err = cv.glm(Data, glm.fit4)
cv.err$delta



# (d) Repeat (c) using another random seed, and report your results.
# Are your results the same as what you got in (c)? Why?

set.seed(10)

# i. Y = β 0 + β 1 X + err
glm.fit = glm(y~x, data=Data)
cv.err = cv.glm(Data, glm.fit)
cv.err$delta

# ii. Y = β 0 + β 1 X + β 2 X^2 + err
glm.fit = glm(y~poly(x, 2), data=Data)
cv.err = cv.glm(Data, glm.fit)
cv.err$delta

# iii. Y = β 0 + β 1 X + β 2 X^2 + β 3 X^3 + err
glm.fit = glm(y~poly(x, 3), data=Data)
cv.err = cv.glm(Data, glm.fit)
cv.err$delta

# iv. Y = β 0 + β 1 X + β 2 X^2 + β 3 X^3 + β 4 X^4 + err
glm.fit = glm(y~poly(x, 4), data=Data)
cv.err = cv.glm(Data, glm.fit)
cv.err$delta

# The results are the same because the
# datasets tested are the same


# (e) Which of the models in (c) had the smallest LOOCV error? Is
# this what you expected? Explain your answer.

# Model (ii.), it is what is expected because data plot
# looked quadratic.

# (f) Comment on the statistical significance of the coefficient esti-
# mates that results from fitting each of the models in (c) using
# least squares. Do these results agree with the conclusions drawn
# based on the cross-validation results?

summary(glm.fit)

# Higly significant p-value at x^2