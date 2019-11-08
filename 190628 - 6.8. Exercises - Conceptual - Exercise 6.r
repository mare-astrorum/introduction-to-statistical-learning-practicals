### Ridge

# For p=1, (6.12) takes the form (y−β)2+λβ2. 
# We plot this function for y=2,λ=2.


y = 2
lambda = 2
betas = seq(-10, 10, 0.1)
func = (y - betas)^2 + lambda * betas^2
plot(betas, func, pch = 20, xlab = "beta", ylab = "Ridge optimization")
est.beta = y/(1 + lambda)
est.func = (y - est.beta)^2 + lambda * est.beta^2
points(est.beta, est.func, col = "red", pch = 4, lwd = 5, cex = est.beta)



### Lasso

# For p=1, (6.13) takes the form (y−β)2+λ|β|. 
# We plot this function for y=2,λ=2.


y = 2
lambda = 2
betas = seq(-10, 10, 0.1)
func = (y - betas)^2 + lambda * abs(betas)
plot(betas, func, pch = 20, xlab = "beta", ylab = "Lasso optimization")
est.beta = y - lambda/2
est.func = (y - est.beta)^2 + lambda * abs(est.beta)
points(est.beta, est.func, col = "red", pch = 4, lwd = 5, cex = est.beta)
