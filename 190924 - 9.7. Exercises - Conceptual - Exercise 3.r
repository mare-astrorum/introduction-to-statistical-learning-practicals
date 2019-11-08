# 3. 
# Here we explore the maximal margin classifier on a toy data set.
# 
# (a) 
# 
# We are given n = 7 observations in p = 2 dimensions. For each
# observation, there is an associated class label.


x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
x = cbind(x1, x2)
y = c("Red", "Red", "Red", "Red", "Blue", "Blue", "Blue")
dat = data.frame(x = x, y = as.factor(y))
y.fact = as.factor(y)

plot(x, col = y.fact)


# (b) Sketch the optimal separating hyperplane, and provide the equa-
#   tion for this hyperplane (of the form (9.1)).


plot(x, col = y.fact, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)

# (c) Describe the classification rule for the maximal margin classifier.
# It should be something along the lines of “Classify to Red if
# β 0 + β 1 X 1 + β 2 X 2 > 0, and classify to Blue otherwise.” Provide
# the values for β 0 , β 1 , and β 2 .




# (d) On your sketch, indicate the margin for the maximal margin
# hyperplane.

abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)
?abline

# (e) Indicate the support vectors for the maximal margin classifier.

points(c(2, 4, 2, 4), c(2, 4, 1, 3), 
       col = c("blue", "blue", "green", "green"), pch=4)
?points

# (f) Argue that a slight movement of the seventh observation would
# not affect the maximal margin hyperplane.

# The observation is far from support vectors

# (g) Sketch a hyperplane that is not the optimal separating hyper-
#   plane, and provide the equation for this hyperplane.

abline(-1, 1.5, lty = 2, col = "purple")

# (h) Draw an additional observation on the plot so that the two
# classes are no longer separable by a hyperplane.

points(3, 2.5, col = "orange")
