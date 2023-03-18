
x0=sin(rnorm(1))
x<-NULL
x[1]<-x0
for (i in 1:100) {
	x[i+1]<-sin(1*i+runif(1)*2*pi*0.1)
}
acf<-NULL
for (i in 1:100) {
l1<-50
acf[i]<-cor(x[1:l1],x[(1+(i-1)):(l1+(i-1))])
}

par(mfrow=c(2,1)) 
plot(x,type="l")
plot(acf)

