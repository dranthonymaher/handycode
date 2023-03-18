
library(ROCR)
library(randomForest)
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")
# generate data

mydata<-read.csv("dummydata.csv", skip = 2)

myglm<-glm(r1~a+b+c+d,data = mydata, family =binomial(link = "logit"))
summary(myglm)

myglm<-glm(r1~a+b+c,data = mydata, family =binomial(link = "logit"))
summary(myglm)

mydata$r1<-as.factor(mydata$r1)
myrf<-randomForest(r1~a+b1+b2+c+y1+y2,data = mydata,ntree=500,# type = "classification", importance=TRUE,
                   proximity=TRUE)
plot(myrf)
print(myrf)
round(importance(myrf), 2)
# Variable Importance
varImpPlot(myrf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

prob=predict(myglm,type=c("response"))
pred<-prediction(prob,mydata$r1)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line 

myglm<-glm(r1~y1+c,data = mydata, family =binomial(link = "logit"))
summary(myglm)

myglm<-glm(r1~y2+c,data = mydata, family =binomial(link = "logit"))
summary(myglm)
prob=predict(myglm,type=c("response"))
pred<-prediction(prob,mydata$r1)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line 

# remove 90% of the responses with 1s
rando<-runif(length(mydata[,1]))
mydata<-cbind(mydata,rando)
mydata<-mydata[mydata$rando<0.1 | mydata$r1==0,]


myglm<-glm(r1~y2+c,data = mydata, family =binomial(link = "logit"))
summary(myglm)
prob=predict(myglm,type=c("response"))
pred<-prediction(prob,mydata$r1)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line 


# remove 90% of the responses with 1s

mydata<-mydata[mydata$rando<0.01 | mydata$r1==0,]


myglm<-glm(r1~y2+c,data = mydata, family =binomial(link = "logit"))
summary(myglm)
prob=predict(myglm,type=c("response"))
pred<-prediction(prob,mydata$r1)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line 
