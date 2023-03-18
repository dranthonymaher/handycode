# read in Ionas data

setwd("/Users/anthonymaher/Documents/R stuff/KoRV B Study all data/")

mydata1<-read.csv("Sheet1-Table 1.csv",strip.white=TRUE)
head(mydata1)
# mydata2<-read.csv("Sheet2-Table 1.csv")
# mydata3<-read.csv("Sheet3-Table 1.csv")
# mydata4<-read.csv("Sheet4-Table 1.csv")

# mydata<-rbind(c(mydata1,mydata2, mydata3, mydata4))
# head(mydata)

#mydata1 has everything!
# first look at the data
plot((mydata1[,6]))
plot(log(mydata1[,6]))
#defo needs a log transform - indeed the data are transcriptomics so this makes sense

#check the others
plot(log(mydata1[,7]))
plot(log(mydata1[,8]))
plot(log(mydata1[,9]))
plot(log(mydata1[,10]))
plot(log(mydata1[,11]))


#preprocessing
#create log transormed dataset
X<-log(mydata1[,6:11])
#X<-(mydata1[,6:11])
#X<-uv(as.matrix(X))

#replace NAs with means
means<-colMeans(X, na.rm = TRUE)
for (i in 1:length(X[1,])){
	X[,i][is.na(X[,i])]<-means[i]
}

#mean centre
source('/Users/anthonymaher/Documents/R stuff/mc.R')
Xmc<-mc(as.matrix(X))

#put back in
mydata1[,6:11]<-Xmc

#plot data overview
pairs(Xmc,upper.panel = NULL,pch = 21, bg = c("red", "blue")[unclass(mydata1$KoRV.Status)])


#split up by time
# X1<-mydata1
# X1<-mydata1[mydata1$Season=="April",]
 # X1<-mydata1[mydata1$Season=="August",]
 X1<-mydata1[mydata1$Season=="December",]
 #X1<-mydata1[mydata1$Season=="February",]


#build the model
model<-princomp(X1[,6:11])

#define the groups
g1<-X1$KoRV.Status=="A"
g2<-X1$KoRV.Status=="B"

#plot the scores
plot(model$scores[g1,1],model$scores[g1,2],col=2)
points(model$scores[g2,1],model$scores[g2,2],col=4)

#install.packages("plsdepot")
library(plsdepot)
r<-NULL
r[g1]<--1
r[g2]<-1


model1a<-plsreg1(X1[,6:11],r)
model1a$Q2
#plot the scores
plot(model1a$x.scores[g1,1],model1a$x.scores[g1,2],col=2 ) #red 
points(model1a$x.scores[g2,1],model1a$x.scores[g2,2],col=4) #blue

barplot(model1a$x.loads[,1])

#add a cor plot
cor(r,X1[,11])

#possible reltionship!!!!

source('/Users/anthonymaher/Documents/R stuff/pcat.R')
source('/Users/anthonymaher/Documents/R stuff/uv.R')
source('/Users/anthonymaher/Documents/R stuff/mc.R')


X3<-diag(4)
X3[2,1]<--1
model1<-pcat(X3,4)
model1$scores


model2<-princomp(X3)
model2$scores
