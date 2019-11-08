# Exercise 12

### (a)

Power = function(){
  result = 2^3
  print(result)
}

Power()


### (b)

Power2 = function(x, a){
  result = x^a
  print(result)
}

Power2(3, 8)


### (c)

Power2(10, 3)
Power2(8, 17)
Power2(131, 3)


### (d)

Power3 = function(x, a){
  result = x^a
  return(result)
}

Power3(131, 3)


### (e)

x = 1:10
y = Power3(x, 2)
plot(x, y, xlab = 'x', ylab = 'x^2', 
     main = 'f(x) = x^2')
plot(x, Power3(x, 2), log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")
plot(x, y, log = "x", xlab = 'log(x)', ylab = 'x^2',
     main = "x^2 versus Log of x")


### (f)

PlotPower = function(x, a){
  y = x^a
  plot(x, y, xlab = 'x', ylab = 'x^a', 
       main = 'f(x) = x^a')
}

PlotPower(1:100, 5)
