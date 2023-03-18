
t<-seq(0,1,0.001)
r1<-runif(length(t))
#f<-lam*exp(-lam*t)
# simulate alive to dead markov chain
muAD<- 0.01
x<-NULL
y<-NULL
x[1]<-1
for (i in 1:length(t)){
r1<-runif(1)
	if (x[i]==3){
		x[i+1]<-x[i]
		} else {
		if ((x[i]==1 & runif(1)>0.15)){
		x[i+1]<-x[i]+1
		} else {
		x[i+1]<-x[i]
		}
}
}

plot(x,type="l")
