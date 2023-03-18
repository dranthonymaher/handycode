x<-1:10
y<-x+runif(length(x))
plot(x,y)

z <- seq(-3,3,.1)
d <- dnorm(z)
plot(z,d,type="l")
title("The Standard Normal Density",col.main="cornflowerblue")

pie(rep(1,16),col=rainbow(16))