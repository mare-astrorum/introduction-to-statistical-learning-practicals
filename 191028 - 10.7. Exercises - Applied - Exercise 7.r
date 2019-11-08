library(ISLR)
set.seed(1)

dsc = scale(USArrests)
a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(b/a)

?cor
?t
