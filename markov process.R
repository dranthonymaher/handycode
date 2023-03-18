
t<-seq(0,1,0.01)
r1<-runif(length(t))
#f<-lam*exp(-lam*t)
# simulate alive to dead markov chain
muAD<- 0.02
x<-NULL
x[1]<-1
for (i in 1:length(t)){

	if (runif(1)>muAD){
		
x[i+1]<-x[i]
} else {
x[i+1]<-0
	
}
}
x
