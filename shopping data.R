

library(reshape)
library(plotly)
library(readxl)
library(reshape)

rm(list = ls(all.names = TRUE))
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")

mydata<-read.csv("shoppingdata2.csv")

model<-glm(data = mydata,toffeeapples~apples+bananas+sugar+shownad+am1+bm1+sm1+shm1, family = "gaussian")

summary(model)
pred<-predict.glm(model,data = mydata[,c(3,5,6,7,8,9,10)],type = "response")

plot(mydata$toffeeapples[2:35], pred)
abline(0,1)


reshapeddata<-cast(mydata,customer~week)
head(reshapeddata)

