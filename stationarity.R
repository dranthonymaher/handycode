#stationarity
i<-1
for i in 1:10
{
x<-runif(100,5,7.5)
#choose two subsets of the time series
n<-5
s<-2
t<-80

sn<-x[s:(s+n)]
tn<-x[t:(t+n)]

bar[i]=cor(sn,tn)
i<-i+1
i
}

x<-
