setwd("/Users/anthonymaher/Documents/R stuff")
getwd()

#import the data 
mydata<-read.csv("playgolf2.csv")
summ<-summary(mydata)

#step 1: calculate entropy of the target

#extract the target
target<-mydata[,5]
#count up number of things in each
r1<-table(target)

#normalise r1
r2<-r1/sum(r1)

#now entropy is just
entvec<-r2*log(r2,2)
ent<-sum(entvec)*-1
#sweet!

ent1<-NULL

#step 2: get the information gain from each vector
#2a. get probabilities
for(i in 1:4){
p1<-table(mydata[,i])/sum(table(mydata[,i]))

#2b. get entropies
t2<-table(mydata[,i],target)
#normalise it
t3<-t2/rowSums(t2)
#get the logs
t3logs<-log(t3,2)
#convert inf to 0
t3logs[is.infinite(t3logs)]<-0
t4<--(t3*t3logs)
#get the row sums
t5<-rowSums(t4)
#finally compute the entropy
ent1[i]<-sum(p1*t5)
}

#now compute the information gain
igain<-ent-ent1

#find which is maximum
maxi<-which.max(igain)



