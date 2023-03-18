x10<-1:5
y10<-x1+rnorm(5)*0.2
x1<-x1-mean(x1)
y1<-y1-mean(y1)
plot(x1,y1)

n<-5000
ms<-rnorm(n)

sumsq<-NULL
for (i in 1:n){
yhat<-ms[i]*x1
sumsq[i]<-sum((y1-yhat)^2)
}

plot(ms,sumsq)
ms[which.min(sumsq)]

