x<-c(rep(0,4),rep(1,3),rep(0,2))
n<-c(4,3,4,6,10,14,12,2,3)
nx<-data.frame(n,x)
nx

plot(nx)

x<-c(0,1,2,3)
n<-c(.3, 1.2, 2, 3.9)
nx<-data.frame(n,x)
nx

plot(nx)

#lets first try to build a method to estimate parameters by maximising #likelihood

#1. flip coin - what is P(Heads)?
x<-c(0,0,1)
p<-seq(0,1,0.1)
px<-matrix(data=NA,nrow=length(x),ncol=length(p))

for(i in 1:length(x)){
r1<-p^x[i]*(1-p)^(1-x[i])
px[i,]<-r1
}
l<-apply(px,2,prod)
getmax<-which.max(l)
#now we have our phat estimate
phat<-p[getmax]

#what if we have more than 2 dimensions? - i.e. more than 1 parameter to maximise

px<-NULL
px<-array(data=1,dim=c(3,3,3))
px
apply(px,1,sum)
px[1,,1]




