states = row.names(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean)
?apply
apply(USArrests, 2, var)

pr.out = prcomp(USArrests, scale = T)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale = 0)

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

pr.out$sdev
pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

plot(pve, xlab = "Principal Component", ylab = "Propoartion of
     Variance Explained", ylim=c(0, 1), type = "b")
plot(cumsum(pve), xlab = "Principal Component", ylab = "Propoartion of
     Variance Explained", ylim=c(0, 1), type = "b")

a = c(1, 2, 8, -3)
cumsum(a)
