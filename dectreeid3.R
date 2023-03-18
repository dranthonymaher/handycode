setwd("/Users/anthonymaher/Documents/R stuff")
getwd()

#import the data 
mydata<-read.csv("playgolf2.csv")
summ<-summary(mydata)

r1<-table(mydata[1])

#step 1: calculate entropy of the target

#extract the target
target<-mydata[5]
#count up number of things in each
r1<-table(target)
#get the number of levels
len<-length(r1)

#normalise r1
r2<-r1/sum(r1)

#now surely entropy is just
#ent<--t*log(t,2)-(1-t)*log((1-t),2)
entvec<-r2*log(r2,2)
ent<-sum(entvec)*-1
#sweet!


#but the t has to be defined recursively and the denominator must sum up all of them
#because t is a probability

#might use apply here!!



