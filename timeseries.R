#

x0=rnorm(1)
x<-NULL
x[1]<-x0
for (i in 1:1000) {
	x[i+1]<-x[i]*.9+rnorm(1)*1
}
acf<-NULL
for (i in 1:100) {
l1<-100
acf[i]<-cor(x[1:l1],x[(1+(i-1)):(l1+(i-1))])
}

par(mfrow=c(2,1)) 
plot(x,type="l")
plot(acf,type="l")







#this one depends on i so is not stationary - note the acf does not go to 0

x0=rnorm(1)
x<-NULL
x[1]<-x0
for (i in 1:1000) {
	x[i+1]<-x[i]*.5+rnorm(1)*1+0.01*i
}
acf<-NULL
for (i in 1:100) {
l1<-100
acf[i]<-cor(x[1:l1],x[(1+(i-1)):(l1+(i-1))])
}

par(mfrow=c(2,1)) 
plot(x,type="l")
plot(acf,type="l")





#moving average process
z<-rnorm(1001)
x0=rnorm(1)
x<-NULL
x[1]<-x0
for (i in 1:1000) {
	if (1) {
	x[i+1]<-rnorm(1)+1.0*z[i]
	}
	if (i>1) {
	x[i+1]<-rnorm(1)+1.0*z[i]+0.3*z[(i-1)]
	}
	}

plot(x,type="l")
mean(x)


