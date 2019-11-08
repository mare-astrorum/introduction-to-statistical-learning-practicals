# 1. 
# This problem involves hyperplanes 
# in two dimensions.

set.seed(1)

# (a) 
# 
# Sketch the hyperplane 1 + 3X 1 − X 2 = 0. 
# Indicate the set of
# points for which 1 + 3X 1 − X 2 > 0, 
# as well as the set of points
# for which 1 + 3X 1 − X 2 < 0.

set.seed(1)

x1 = rnorm(100)
x2 = 3 * x1 + 1

plot(x1, x2, type = "l")

# (b) 
# 
# On the same plot, sketch the 
# hyperplane −2 + X 1 + 2X 2 = 0.
# Indicate the set of points for 
# which −2 + X 1 + 2X 2 > 0, as well
# as the set of points for which 
# −2 + X 1 + 2X 2 < 0.

x2a = (2 - x1)/2
lines(x1, x2a, col = "red")
