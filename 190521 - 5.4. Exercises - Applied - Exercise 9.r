9. We will now consider the Boston housing data set, from the MASS
library.

library(MASS)
attach(Boston)
?Boston
summary(Boston)

(a) Based on this data set, provide an estimate for the population
mean of medv. Call this estimate μ̂ .

mu = mean(medv)
mu

(b) Provide an estimate of the standard error of μ̂ . Interpret this
result.

Hint: We can compute the standard error of the sample mean by
dividing the sample standard deviation by the square root of the
number of observations.

std.medv = sd(medv)
std.medv
se.medv = std.medv/sqrt(length(medv))
se.medv


# (c) Now estimate the standard error of μ̂  using the bootstrap. How
# does this compare to your answer from (b)?

library(boot)
set.seed(1)


boot.fn = function(data, index){
	return(mean(data[index]))
} 

boot.medv = boot(medv, boot.fn, 1000)
boot.medv

The values from (b) and (c) are very similar.


(d) Based on your bootstrap estimate from (c), provide a 95 % con-
fidence interval for the mean of medv. Compare it to the results
obtained using t.test(Boston$medv) .

Hint: You can approximate a 95 % confidence interval using the
formula [μ̂ − 2SE(μ̂ ), μ̂ + 2SE(μ̂ )].


REGULAR
mu - 2 * se.medv
mu + 2 * se.medv

CI[21.71508, 23.35053]

BOOTSTR
c(boot.medv$t0 - 2 * 0.4119, boot.medv$t0 + 2 * 0.4119)
ci[21.70901 23.35661]

TTEST
t.test(Boston$medv)
CI[21.72953 23.33608]

The values are very similar.

(e) Based on this data set, provide an estimate, μ̂ med , for the median
value of medv in the population.

medv.median = median(medv)
medv.median

(f) We now would like to estimate the standard error of μ̂ med . Unfor-
tunately, there is no simple formula for computing the standard
error of the median. Instead, estimate the standard error of the
median using the bootstrap. Comment on your findings.

boot.fn2 = function(data, index){
	return(median(data[index]))
} 

boot.medv = boot(medv, boot.fn2, 1000)
boot.medv



(g) Based on this data set, provide an estimate for the tenth per-
centile of medv in Boston suburbs. Call this quantity μ̂ 0.1 . (You
can use the quantile() function.)

?quantile
quantile(medv)
quantile(medv, probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)
mu.0.1 = quantile(medv, probs = c(10)/100)
mu.0.1

(h) Use the bootstrap to estimate the standard error of μ̂ 0.1 . Com-
ment on your findings.

boot.fn3 = function(data, index){
	return(quantile(data[index], probs = c(10)/100))
}


boot.fn4 = function(data, index){
  return(quantile(data[index], probs = c(0.1)))
}

boot.0.1 = boot(medv, boot.fn4, 1000)
boot.0.1 

boot.answ = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.answ, 1000)
