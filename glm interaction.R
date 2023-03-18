
                                                                              
#setwd("/Users/niamhmallon/Downloads/")
# 29/2/16 - test GLM on test data
#mydata<-read.csv("testdata2.csv")

#define the data
nsamp<-10000 #number of samples
age<-runif(nsamp,18,70)
gender<-rbinom(nsamp,1,0.5)
rando<-runif(nsamp)
r<-round(exp(age*0.03+gender*(2)+age*gender*-0.05 + rando + 2.3)) #add interaction here
mydata<-as.data.frame(cbind(age,gender,r))
mydata

#define the GLM
mymodel_int<-glm(r~age+gender+age:gender, family = poisson, mydata)
mymodel_noint<-glm(r~age+gender, family = poisson, mydata)
summary(mymodel_int)
summary(mymodel_noint)


p3<-predict(mymodel_int,mydata)
plot(exp(p3),mydata$r)

pcnt<-seq(0,1,0.01)
quantile(mydata$r, pcnt)
plot(quantile(mydata$r, pcnt))











summary(glm(r~a+g+c+h, family = poisson, mydata))

mymodel<-glm(r~a+g+c+h, family = poisson, mydata)
summary(mymodel)
p1<-predict(mymodel,mydata,type="link")
p1
exp(p1)
aggregate(exp(p1),by=cbind(mydata["a"],mydata["g"]),FUN=mean)



exp(p)-mydata$r

#q1: check obs
aggregate(mydata["r"],by=mydata["a"],FUN=mean)

#check

#q2. what does CM represent?

#try fitting a model with just a and g fit.
mymodel2<-glm(r~a+g, family = poisson, mydata)
summary(mymodel2)
p2<-predict(mymodel2)
p2
exp(p2)
exp(p2)-mydata$r
aggregate(exp(p2),by=mydata["a"],FUN=mean)


mymodel2<-glm(r~a, family = poisson, mydata)
summary(mymodel2)
p3<-predict(mymodel2,mydata)
p3
exp(p3)
exp(p3)-mydata$r
aggregate(exp(p3),mydata["a"],FUN=mean)