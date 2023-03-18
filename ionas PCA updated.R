# read in Ionas data

#setwd("/Users/anthonymaher/Documents/R stuff/KoRV B Study all data/")
setwd("C:/Users/anthonym/Downloads/")

mydata<-read.csv("Shee1-Table 1.csv",strip.white=TRUE)

mydata1<-read.csv("Shee1-Table 1.csv",strip.white=TRUE)
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
hist(log(mydata1[,6]))

#defo needs a log transform - indeed the data are transcriptomics so this makes sense

#do some tests for normality
myvar<-12
v1<-log(mydata1[,myvar])
shapiro.test(v1)


#check the others
plot(log(mydata1[,7]))
plot(log(mydata1[,8]))
plot(log(mydata1[,9]))
plot(log(mydata1[,10]))
plot(log(mydata1[,11]))



#create log transormed dataset
X<-log(mydata1[,6:11])
#X<-(mydata1[,6:11])


#replace NAs with means
means<-colMeans(X, na.rm = TRUE)
medians<-NULL

for (i in 1:length(X[1,])){
	X[,i][is.na(X[,i])]<-means[i]
}
#put back in
mydata1[,6:11]<-X

#plot data overview
pairs(X,upper.panel = NULL,pch = 21, bg = c("red", "blue")[unclass(mydata1$KoRV.Status)])


#split up by time
# X1<-mydata1
# X1<-mydata1[mydata1$Season=="April",]
 # X1<-mydata1[mydata1$Season=="August",]
 X1<-mydata1[mydata1$Season=="December",]
 #X1<-mydata1[mydata1$Season=="February",]


#build the model

pcadata<-X1[,6:11]

#mean centre and scale to unit variance
pcadatamc<-pcadata-colMeans(pcadata)
pcadatamcuv<-pcadatamc/apply(pcadatamc,2,sd)

model<-princomp(pcadata)

#define the groups
g1<-X1$KoRV.Status=="A"
g2<-X1$KoRV.Status=="B"

#plot the scores
plot(model$scores[g1,1],model$scores[g1,2],col=2)
points(model$scores[g2,1],model$scores[g2,2],col=4)
text(model$scores[,1],model$scores[,2],X1$Stimulation)

#plot the loadings
plot(model$loadings)

plot(model$scores[g1,3],model$scores[g1,4],col=2)
points(model$scores[g2,3],model$scores[g2,4],col=4)
#text(model$scores[,1],model$scores[,2],X1$Stimulation)

install.packages("plsdepot")
library(plsdepot)
r<-NULL
r[g1]<-1
r[g2]<-2



#model1<-plsreg1(pcadatamc,r)
model1<-plsreg1(mydata[,6:11],r)
model1$Q2
#plot the scores
plot(model1$x.scores[g1,1],model1$x.scores[g1,2],col=2 ) #red 
points(model1$x.scores[g2,1],model1$x.scores[g2,2],col=4) #blue

#possible reltionship!!!!

