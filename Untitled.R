t<-seq(0,10, by=0.1)

alpha<-0.2
beta<-2.5
f<-alpha*exp(-alpha*t^beta+0.5 -4)
f1<-alpha*exp(-alpha*t^beta+.2-2)
#f1<-NULL
#for (i in 1:length(t)){
#	f1[i]<-sum(f[1:i])
#	}

#par(mfrow=c(2,1))
plot(t,f,type='l',col='blue',ylim=c(0,.5))
par(new=T)
plot(t,f1,type='l',col='red',ylim=c(0,.5))


