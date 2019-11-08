# 2. 
# We have seen that in p = 2 dimensions, 
# a linear decision boundary
# takes the form β 0 +β 1 X 1 +β 2 X 2 = 0. 
# We now investigate a non-linear
# decision boundary.

# (1+X1)2+(2−X2)2=4 is a circle with radius 2 and center (-1, 2).

# (a) 
# 
# Sketch the curve
# (1 + X 1 ) 2 + (2 − X 2 ) 2 = 4.

radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 9), asp = 1, xlab = "X1", 
     ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)
?plot
?symbols


# (b) 
# 
# On your sketch, indicate the 
# set of points for which
# (1 + X 1 ) 2 + (2 − X 2 ) 2 > 4,
# as well as the set of points for which
# (1 + X 1 ) 2 + (2 − X 2 ) 2 ≤ 4.

text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")
 
# (c) 
# 
# Suppose that a classifier assigns 
# an observation to the blue class if
# (1 + X 1 ) 2 + (2 − X 2 ) 2 > 4,
# and to the red class otherwise. 
# To what class is the observation
# (0, 0) classified? (−1, 1)? (2, 2)? (3, 8)?

points(c(0, -1, 2, 3), c(0, 1, 2, 8), 
       col = c("blue", "red", "blue", "blue"))
  
#   (d) 
# Argue that while the decision boundary 
# in (c) is not linear in
# terms of X 1 and X 2 , it is linear in 
# terms of X 1 , X 1 2 , X 2 , and
# X 2 2 .