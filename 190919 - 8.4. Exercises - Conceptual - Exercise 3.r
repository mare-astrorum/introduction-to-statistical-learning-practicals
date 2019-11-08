pm1 = seq(0, 1, 0.1)
pm2 = 1 - pm1

G = pm1 * (1-pm1) + pm2 * (1-pm2)
D = -(pm1 * (log(pm1)) + pm2 * (log(pm2)))
entropy = -(pm1 * log(pm1) + (1 - pm1) * log(1 - pm1))

E = 1 - pmax(pm1, pm2)
?pmax


matplot(pm1, cbind(E, D, G), type = "l")
