
t<-seq(0,1,0.001)
r1<-runif(length(t))
#f<-lam*exp(-lam*t)
# simulate alive to dead markov chain
muAD<- 0.01
x<-NULL
y<-NULL
x[1]<-2
for (i in 1:length(t)){
r1<-runif(1)
	if (r1>muAD){
		
x[i+1]<-x[i]
} else {
	if (r1<muAD*0.25){
x[i+1]<-2
	} else {
		x[i+1]<-3
	}
	
}
}

plot(x,type="l")
