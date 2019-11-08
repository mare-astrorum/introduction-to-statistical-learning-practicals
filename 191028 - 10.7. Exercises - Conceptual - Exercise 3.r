x1 = c(1, 1, 0, 5, 6, 4)
x2 = c(4, 3, 4, 1, 2, 0)

plot(x1, x2)

df = data.frame(x1, x2)
plot(df)

sample.df = sample(6, 3)

cl1 = df[sample.df,]
cl2 = df[-sample.df,]

mean.cl1.x = mean(cl1[, 1])
mean.cl1.y = mean(cl1[, 2])
centroid1 = c(mean.cl1.x, mean.cl1.y)
plot(centroid1[1], centroid1[2])

mean.cl2.x = mean(cl2[, 1])
mean.cl2.y = mean(cl2[, 2])
centroid2 = c(mean.cl2.x, mean.cl2.y)
plot(centroid2[1], centroid2[2])

plot(df)
points(cl1, col = "blue")
points(centroid1[1], centroid1[2], col = "red")
points(centroid2[1], centroid2[2], col = "green")

euclid = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_labels = function(x, centroid1, centroid2) {
  labels = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], centroid1) < euclid(x[i,], centroid2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}
labels = assign_labels(x, centroid1, centroid2)
labels

?prcomp
