fix(Carseats)
names(Carseats)

lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

?contrasts