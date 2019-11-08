library(ISLR)
attach(Wage)
?Wage


# maritl


plot(wage~maritl, data=Wage, col="darkgrey")
fit.1 = lm(wage~maritl, data=Wage)
summary(fit.1)
deviance(fit.1)


plot(wage~jobclass, data=Wage, col="darkgrey")
fit.2 = lm(wage~jobclass, data=Wage)
summary(fit.2)
deviance(fit.2)

fit.3 = lm(wage~jobclass + maritl, data=Wage)
summary(fit.3)
deviance(fit.3)


library(gam)
fit.4 = gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
summary(fit.4)
deviance(fit.4)
