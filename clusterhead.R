# K-MEANS simulation
 
# 9/8/18
#setwd("C:/Users/anthonym/OneDrive - Quantium/R stuff")
 
n<-500
numgroups<-2
#generate dummy data
x<-rnorm(n,0,2)
y<-rnorm(n,4,2)
 
x1<-rnorm(n,3,1)
y1<-rnorm(n,0,1)
x2<-rnorm(n,-3,1)
y2<-rnorm(n,0,1)
 
data<-as.data.frame(cbind(c(x,x1,x2),c(y,y1,y2)))
 noisw<-runif(n)*5
noisw2<-runif(n)*5
noisw3<-runif(n)*5
data$V3<-noisw
data$V4<-noisw2
data$V5<-noisw3
customer<-seq(1:length(data[,1]))
 
mydata<-cbind(customer,data)
 head(mydata)
plot(data)
 
model<-kmeans(data,3, iter.max=1000)
# summary(model)
groups<-model$cluster
r2<-model$withinss / model$totss 

par(mfcol = c( 2, 2))
plot(r2,type="l")
plot(mydata$V1,mydata$V2,col=groups,main=min(r2))
plot(mydata$V1,mydata$V3,col=groups,main=min(r2))
plot(mydata$V2,mydata$V3,col=groups,main=min(r2))
 
install.packages("cluster")
library("cluster")

m2<-pam(data,3)
plot(m2)