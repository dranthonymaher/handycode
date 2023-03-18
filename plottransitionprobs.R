t<-seq(0,10,0.1)
lambda<-1.1

j<-5
p1<-exp(-lambda*t)*(lambda*t)^j/factorial(j)

plot(t,p1)



z=rnorm(20000, mean=10, sd=3)
y=rnorm(20000, mean=10 ,sd=3)

plot(z,y)
cov(z,y)