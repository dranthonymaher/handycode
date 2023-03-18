library(klaR)
library(ROCR)
library(ggplot2)
library(gbm)


library(OneR)
# set.seed(700)
# 
# clientID <- round(runif(200, min = 2000, max = 3000), 0)
# orders <- round(runif(200, min = 1, max = 50), 0)
# df <- data.frame(cbind(clientID, orders))
# 
# df$orders <- bin(df$orders, method = "content")
# table(df$Quintile)


# op <- par(mar=c(6,4,1,1))
# plot(x, y, t="l", xaxt="n", xlab="")
# axis(1, at=tlab, labels=FALSE)
# text(x=tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
#      labels=lab, srt=45, adj=1, xpd=TRUE)
# par(op)


setwd("D:/Engineered Features")
dev.off()
mydata<-NULL
mydata<-read.csv("model_Data.csv")


mydata<-read.csv("SES_Responses_NEW.csv")

head(mydata)


a1<-aggregate(mydata$QOESAT,by=list(mydata$faculty),FUN=mean)
plot(a1, xaxt="n")
# Changing x axis
xtick<-seq(1, length(a1[,2]), by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = as.character(a1$Group.1), srt = 60, pos = 1, xpd = TRUE, cex=0.75)

plot(aggregate(mydata$QOESAT,by=list(mydata$Avg_Mark),FUN=mean))

plot(aggregate(mydata$QOESAT,by=list(mydata$Avg_Class_Size),FUN=mean))

colnames(mydata)
plot(density(mydata[mydata$QOESAT==1,6]), col="red")
lines(density(mydata[mydata$QOESAT==0,6]), col="blue")



summary(mydata)
colnames(mydata)
colours<-rainbow(7)
#CHECK the classes of each column
sapply(mydata,class)

# note that some of the  "id" columns have been imported as integers - convert these to factors:
mydata$E329<-as.factor(mydata$E329)
mydata$English_First_Lang_Flag<-as.factor(mydata$English_First_Lang_Flag)
mydata$Student_Stage<-as.factor(mydata$Student_Stage)
mydata$Sem1_Cluster<-as.factor(mydata$Sem1_Cluster)
mydata$Sem2_Cluster<-as.factor(mydata$Sem2_Cluster)


#rounding
plot(density(mydata$Avg_Mark))
mydata$Avg_Mark<-round(mydata$Avg_Mark,-1)

plot(density(mydata$Avg_Class_Size))
mydata$Avg_Class_Size<-round(mydata$Avg_Class_Size,-2)
mydata$Avg_Class_Size<-ifelse(mydata$Avg_Class_Size > 1000,1000,mydata$Avg_Class_Size)

plot(density(mydata$stu2ormoreall))
mydata$stu2ormoreall<-round(mydata$stu2ormoreall,1)
mydata$stu2ormoreall<-ifelse(mydata$stu2ormoreall > 0.8,0.8,mydata$stu2ormoreall)

plot(density(mydata$stu2_others))
mydata$stu2_others<-round(mydata$stu2_others,-3)
mydata$stu2_others<-ifelse(mydata$stu2_others > 10000,10000,mydata$stu2_others)

datacols<-c(2:11)
response<-12
mycols<-c(datacols,response)




mform<-as.formula(paste("QOESAT ~ ",paste(colnames(mydata)[datacols],collapse = "+")))
#check it
mform
mydatamodel<-mydata[mycols]
head(mydatamodel)
colnames(mydatamodel)
# RESPONSE MUST BE NUMERIC!!!!
# DATA FRAME CAN ONLY HAVE VARIABLES THAT ARE USED IN THE FONRMULA!!!!!
mygbm<-gbm(
  mform,
  #readmitl30 ~ age+race+max_glu_serum,
  # data = mydata[sample(1:length(mydata[,1]),5000),],
  data = mydatamodel,
  distribution="bernoulli",
  n.trees = 1000,
  shrinkage = 0.05,
  interaction.depth = 3,
  bag.fraction = 0.5,
  train.fraction = 0.8,
  cv.folds = 3,
  verbose = FALSE
)


best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="test")
best.iter



best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="OOB")
best.iter
#Determine predictions for the ScoringData
best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="cv")
best.iter

summary(mygbm, n.trees = best.iter)

prob=predict.gbm(mygbm,mydatamodel,best.iter,"response")
mydata$prob=prob
pred<-prediction(prob,mydata$QOESAT)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line  

i<-8
# partial dependence plots
plot(mygbm, i.var =i, n.trees = best.iter,continuous.resolution = 100, 
     return.grid = FALSE, type = c("response"), level.plot = TRUE, contour = FALSE, number = 4,
     overlap = 0.1, main = colnames(mydatamodel)[i])


# PvO plot

#use the quantile plot I've already used in the wifi code!!!

#par(mfrow = c(1, 2))
#summary(mygbm, n.trees = 2)          # using first tree
summary(mygbm, n.trees = best.iter, pch = 0.5)  # using estimated best number of trees
#dev.off()
temp <- summary(mygbm,n.trees=best.iter)
temp

#Display the partial dependence charts, starting with the most important
num <- 1

a = plot.gbm(mygbm,i.var=as.vector(summary.gbm(mygbm,n.trees=best.iter)[5,1]))
a
num <- num+1

# Construct univariate partial dependence plots
p1 <- plot(mygbm, i.var = 1, n.trees = best.iter)
p2 <- plot(mygbm, i.var = 2, n.trees = best.iter)
p3 <- plot(mygbm, i.var = "time_in_hospital", n.trees = best.iter)  # can use index or name
grid.arrange(p1, p2, p3, nrow = 2)

# Construct bivariate partial dependence plots
plot(mygbm, i.var = 1:2, n.trees = best.iter)
plot(mygbm, i.var = c("number_diagnoses", "A1Cresult"), n.trees = best.iter)
plot(mygbm, i.var = 5:4, n.trees = best.iter)

# Construct trivariate partial dependence plots
plot(mygbm, i.var = c(1, 2, 6), n.trees = best.iter, 
     continuous.resolution = 20)
plot(mygbm, i.var = 1:3, n.trees = best.iter)

# test the interactions
interact.gbm(mygbm,mydatamodel,i.var = 1:2,n.trees = best.iter)

#Determine predictions for the ScoringData
FreqPredTrain <- predict.gbm(mygbm,mydatamodel,best.iter,"response")
plot(sort(FreqPredTrain))






































# plot some examples of wifi usage
png(filename = "Examples of usage.png",width = 900, height = 600)
n<-c(4,15,29,31,66,72)
matplot(t(as.matrix(wifidata[n,datacols])),type = "l",lty = 1,xaxt='n', ylab = "Wifi Usage (number of sessions)", 
        main = "Examples of wifi usage by individual students")
axis(1,at=1:length(wifidata[,datacols]),colnames(wifidata[,datacols]))
dev.off()


# check the data 
a1<-NULL
a1<-aggregate(wifidata[,datacols],by=list(wifidata$yearaccessed, 
                                          wifidata$semiyear),
              mean)
png(filename = "average wifi by semester.png",width = 900, height = 600)
matplot(t(as.matrix(a1[,3:length(a1[1,])])),lty = 1, type = "l",col = colours,xaxt='n', ylab = "Average Usage")
axis(1,at=1:length(a1[,3:length(a1[1,])]),colnames(a1[,3:length(a1[1,])]))
legend("topleft", legend=paste0(a1$Group.1," s",a1$Group.2), col = colours,
         lty=1, cex=0.8)
dev.off()

#fix the bad parts of the data from 2017
w <- wifidata[sample(nrow(wifidata),10000),]
checks<-as.matrix(w[,datacols])

matplot(t(checks[w$yearaccessed=="2016" & w$semiyear==1,]), type = "l", main = "2016, s1")
lines(colMeans(checks[w$yearaccessed=="2016" & w$semiyear==1,])*10, lwd=5 )

matplot(t(checks[w$yearaccessed=="2016" & w$semiyear==2,]), type = "l", main = "2016, s2")
lines(colMeans(checks[w$yearaccessed=="2016" & w$semiyear==2,])*10, lwd=5 )
#week 9 issue
matplot(t(checks[w$yearaccessed=="2017" & w$semiyear==1,]), type = "l", main = "2017, s1")
lines(colMeans(checks[w$yearaccessed=="2017" & w$semiyear==1,])*10, lwd=5 )
# weeks 3 and 4 issue
matplot(t(checks[w$yearaccessed=="2017" & w$semiyear==2,]), type = "l", main = "2017, s2")
lines(colMeans(checks[w$yearaccessed=="2017" & w$semiyear==2,])*10, lwd=5 )
#weeks 9 and 10 issue

matplot(t(checks[w$yearaccessed=="2018" & w$semiyear==1,]), type = "l", main = "2018, s1")
lines(colMeans(checks[w$yearaccessed=="2018" & w$semiyear==1,])*10, lwd=5 )
#weeks 9 and 10 issue


# problems with weeks 3 and 4 from 2017 and weeks 9 and 10 from 2016, 2017

# wifidata<-wifidata[wifidata$yearaccessed=="2016" & wifidata$semiyear==1,]
# double cheked the results against this - makes no difference to the final output




# so lets re-define the data columns as the ones without issues:
datacols<-c(4,5,8,9,10,11,14,15,16)


# check the quantiles agains the satisfied score
sums<-apply(wifidata[,datacols], 1,sum  )
wifidata$sums<-sums
brks <- with(wifidata, quantile(sums, seq(0,1,0.1)))
wifidata <- within(wifidata, quantile <- cut(sums, breaks = brks,
                                             labels = 1:length(brks[-1]),
                                     include.lowest = TRUE))

aggregate(wifidata$qoedu_numeric,by=list(wifidata$quantile),FUN=mean, 
          na.rm=TRUE)
aggregate(wifidata$qoedu_numeric,by=list(wifidata$quantile),FUN=sd, 
          na.rm=TRUE)


# pca<-princomp(wifidata[sums>=q1[2] & sums<=q1[10],datacols],cor = FALSE)

# PCA
# log transform
# w1<-log(wifidata[,datacols]+10)
# w2<-apply(w1, 2, function(i) i/sum(i))

# normalisation and log transform make no difference

pca<-princomp(wifidata[,datacols],cor = FALSE)
summary(pca)
eigs <- pca$sdev^2
pcares<-rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))
write.csv(pcares,file = "pcaresults.csv")

plot(pca$scores[,1], pca$scores[,2])
#text(pca$scores[,1], pca$scores[,2])
# 
# data2016s1[7885,]

cols2<-rainbow(5)




max1<-max(pca$loadings[,1:5])
png(filename = "wifi_PCA_loadings.png",width = 900, height = 600)
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(fig = c(0,1,0,1), new = F)
plot(pca$loadings[,1], type = "l", col = cols2[1],ylim = c(-max1,max1),xaxt='n'
     ,ylab = "PCA loadings")
axis(1,at=1:length(wifidata[,datacols]),colnames(wifidata[,datacols]), cex = 0.5)
lines(pca$loadings[,2], col =  cols2[2])
lines(pca$loadings[,3], col =  cols2[3])
lines(pca$loadings[,4], col =  cols2[4])
lines(pca$loadings[,5], col =  cols2[5])
legend("topleft", legend=c("PC1","PC2","PC3","PC4","PC5"),
       col=cols2,  lty=1, cex=0.8)
dev.off()

library(pls)
# PLS
y = as.matrix(wifidata$sums)
X=as.matrix(wifidata[,datacols])
modelpls<-plsr(y ~ X,ncomp = 1 ,validation = "CV", segments = 3 )
summary(modelpls)
X1<-tcrossprod(modelpls$scores,(modelpls$loadings))
Xnew<-(X-X1)

n<-1100
plot(X[n,],type = "l",ylim = c(-60,60))
lines(X1[n,], col="blue")
lines(Xnew[n,], col="red")



# BEGIN K-MEANS

n1<-NULL
for (i in 1:10){kmmodel<-kmeans(wifidata[,datacols],i,
                                iter.max = 2000, algorithm="MacQueen")
set.seed(1)
n1[i]<-kmmodel$betweenss / kmmodel$totss
}
png(filename = "kmeans_wifidata_nplot.png")
plot(n1, type = "l", ylim = c(-0.3,0.8), xlab = "k"
     ,ylab = "Between SS as prop of Total SS"
     ,main = "K-means on wifi usage data")
lines(diff(n1), col="blue")
lines(diff(diff(n1)), col="red")
legend("topleft", legend=c("n","n'","n''"), col = c("black","blue","red"),lty=1, cex=1.5)
dev.off()





n1<-NULL
for (i in 1:10){kmmodel<-kmeans(pca$scores[,c(1:8)],i,
                                iter.max = 2000, algorithm="MacQueen")
set.seed(1)
n1[i]<-kmmodel$betweenss / kmmodel$totss
}
png(filename = "kmeans_allPCs.png")
plot(n1, type = "l", ylim = c(-0.3,0.8), xlab = "k"
     ,ylab = "Between SS as prop of Total SS"
     ,main = "K-means on PCA scores 1-8")
lines(diff(n1), col="blue")
lines(diff(diff(n1)), col="red")
legend("topleft", legend=c("n","n'","n''"), col = c("black","blue","red"),lty=1, cex=1.5)
dev.off()

n1<-NULL
for (i in 1:10){kmmodel<-kmeans(pca$scores[,c(2:8)],i,
                                iter.max = 2000, algorithm="MacQueen")
set.seed(1)
n1[i]<-kmmodel$betweenss / kmmodel$totss
}
png(filename = "kmeans_PC2_8.png")
plot(n1, type = "l", ylim = c(-0.1,0.5), xlab = "k"
     ,ylab = "Between SS as prop of Total SS"
     ,main = "K-means on PCA scores 2-8")
lines(diff(n1), col="blue")
lines(diff(diff(n1)), col="red")
legend("topleft", legend=c("n","n'","n''"), col = c("black","blue","red"),lty=1, cex=1.5)
dev.off()



n1<-NULL
for (i in 1:9){kmmodel<-kmeans(Xnew,i,
                                iter.max = 1000, algorithm="MacQueen")
set.seed(1)
n1[i]<-kmmodel$betweenss / kmmodel$totss
Sys.sleep(0.01)
print(i)
}
png(filename = "kmeans_Xnew.png")
plot(n1, type = "l", ylim = c(-0.1,0.4), xlab = "k"
     ,ylab = "Between SS as prop of Total SS"
     ,main = "K-means on Xnew")
lines(diff(n1), col="blue")
lines(diff(diff(n1)), col="red")
legend("topleft", legend=c("n","n'","n''"), col = c("black","blue","red"),lty=1, cex=1.5)
dev.off()




# 3 looks pretty good
numclust<-4
set.seed(1)
kmmodel<-kmeans(wifidata[,datacols],4,
               iter.max = 2000, algorithm="MacQueen")
wifidata$cluster<- kmmodel$cluster

wifidata$cluster<-kmmodel$cluster
a1<-NULL
a1<-aggregate(wifidata[,datacols],by=list(wifidata$cluster),mean)
a1
colnames(a1)
maxa1<-max(a1)
aggregate(wifidata[,datacols],by=list(wifidata$cluster),length)
# matplot(t(as.matrix(a1)))

col1<-rainbow(numclust)


# png(filename = "average wifi by semester.png",width = 900, height = 600)
# matplot(t(as.matrix(a1[,3:length(a1[1,])])),lty = 1, type = "l",col = colours,xaxt='n', ylab = "Average Usage")
# axis(1,at=1:length(a1[,3:length(a1[1,])]),colnames(a1[,3:length(a1[1,])]))
# legend("topleft", legend=paste0(a1$Group.1," s",a1$Group.2), col = colours,
#        lty=1, cex=0.8)
# dev.off()

png(filename = "Wifi cluster usage using Wifi data.png",width = 900, height = 600)

par(mar=c(5.1, 4.1, 4.1, 2.1))
par(fig = c(0,1,0,1), new = F)
plot(as.numeric(a1[1,2:10]), type ="l",ylim = c(0,maxa1), col=col1[1],xaxt='n', ylab = "Average usage for cluster",
     main = "Average Wifi usage from clusters defined using actual wifi data")
axis(1,at=1:9,labels = colnames(a1)[2:10],cex.axis=1)
for (i in 1:length(a1[,1])){
  lines(as.numeric(a1[i,2:10]),col=col1[i])
}
par(mar=c(1,1,1,1))
par(fig = c(0.5,0.9, 0.85, 0.95), new = T)  
barplot(rep(1,length(col1)),col=col1, axes = FALSE, names.arg=(c(1:length(col1))))

dev.off()

# aggregate(wifidata$qoedu_numeric,by=list(wifidata$cluster),FUN=mean, 
#           na.rm=TRUE)
# aggregate(wifidata$qoedu_numeric,by=list(wifidata$cluster),FUN=sd, 
#           na.rm=TRUE)


#RE-DO KMEANS ON THE pls CLEANED DATA


# 3 looks pretty good
numclust<-4
set.seed(1)
kmmodel<-kmeans(Xnew,numclust,iter.max = 2000, algorithm="MacQueen")

wifidata$cluster<- kmmodel$cluster

a1<-NULL
a1<-aggregate(wifidata[,datacols],by=list(wifidata$cluster),mean)
a1
colnames(a1)
maxa1<-max(a1)
a2<-aggregate(wifidata[,datacols],by=list(wifidata$cluster),length)
nums<-a2[,2]
cbind(a1,a2[,2])
write.csv(cbind(a1,a2[,2]), file = "numbers in each group.csv")

# matplot(t(as.matrix(a1)))

col1<-rainbow(numclust)


# png(filename = "average wifi by semester.png",width = 900, height = 600)
# matplot(t(as.matrix(a1[,3:length(a1[1,])])),lty = 1, type = "l",col = colours,xaxt='n', ylab = "Average Usage")
# axis(1,at=1:length(a1[,3:length(a1[1,])]),colnames(a1[,3:length(a1[1,])]))
# legend("topleft", legend=paste0(a1$Group.1," s",a1$Group.2), col = colours,
#        lty=1, cex=0.8)
# dev.off()
png(filename = "Wifi cluster usage using PLS treated data.png",width = 900, height = 600)

 par(mar=c(5.1, 4.1, 4.1, 2.1))
 par(fig = c(0,1,0,1), new = F)
plot(as.numeric(a1[1,2:10]), type ="l",ylim = c(0,maxa1), col=col1[1],xaxt='n', ylab = "Average usage for cluster",
     main = "Average Wifi usage from clusters defined using Xnew")
axis(1,at=1:9,labels = colnames(a1)[2:10],cex.axis=1)
for (i in 1:length(a1[,1])){
  lines(as.numeric(a1[i,2:10]),col=col1[i])
}
par(mar=c(1,1,1,1))
par(fig = c(0.5,0.9, 0.85, 0.95), new = T)  
barplot(rep(1,length(col1)),col=col1, axes = FALSE, names.arg=(c(1:length(col1))))
dev.off()
# aggregate(wifidata$qoedu_numeric,by=list(wifidata$cluster),FUN=mean, 
#           na.rm=TRUE)
# aggregate(wifidata$qoedu_numeric,by=list(wifidata$cluster),FUN=sd, 
#           na.rm=TRUE)



write.csv(wifidata,file = 
            "C:/Users/Techlab/The University of Sydney (Staff)/Learning Analytics 2019 - General/Learning Analytics/sys_wifi_cluster.csv")
write.csv(wifidata,file = 
            "sys_wifi_cluster.csv")


# dev.off()
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(fig = c(0,1,0,1), new = F)
# plot(pca$scores[,2], pca$scores[,3], col = wifidata$cluster, xlim = c(-1500,1500), ylim = c(-500,500))
# 


# datasample<- data[sample(nrow(data),1000),]
# 
# plot(as.numeric(datasample[1,1:13]), type ="l", col="black",ylim = c(0,200))
# matplot(t(as.matrix(datasample[datasample$cluster==1,1:13])), type = "l", col = "red")
# matplot(t(as.matrix(datasample[datasample$cluster==2,1:13])), type = "l", col = "blue")
# matplot(t(as.matrix(datasample[datasample$cluster==3,1:13])), type = "l", col = "green")

# ok! good






































# 
# 
# 
# 
# 
# 
# 
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(fig = c(0,1,0,1), new = F)
# 
# # redo kmeans on raw data
# 
# for (i in 1:6){kmmodel<-kmeans(wifidata[,datacols],i,
#                                 iter.max = 20000)
# set.seed(1)
# n1[i]<-kmmodel$betweenss / kmmodel$totss
# }
# plot(n1)
# 
# 
# 
# #  looks pretty good
# numclust<-4
# set.seed(1)
# kmmodel<-kmeans(wifidata[,datacols],numclust,
#                 iter.max = 2000)
# 
# wifidata$cluster<-kmmodel$cluster
# 
# a1<-aggregate(wifidata[,datacols],by=list(wifidata$cluster),mean)
# a1
# maxa1<-max(a1)
# aggregate(wifidata[,datacols],by=list(wifidata$cluster),length)
# # matplot(t(as.matrix(a1)))
# 
# col1<-rainbow(numclust)
# 
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(fig = c(0,1,0,1), new = F)
# plot(as.numeric(a1[1,2:14]), type ="l",ylim = c(0,maxa1), col=col1[1])
# for (i in 1:length(a1[,1])){
#   lines(as.numeric(a1[i,2:14]),col=col1[i])
# }
# par(mar=c(1,1,1,1))
# par(fig = c(0.5,0.9, 0.85, 0.95), new = T)  
# barplot(rep(1,length(col1)),col=col1, axes = FALSE, names.arg=(c(1:length(col1))))
# 
# aggregate(wifidata$qoedu_numeric,by=list(wifidata$cluster),FUN=mean, 
#           na.rm=TRUE)
# aggregate(wifidata$qoedu_numeric,by=list(wifidata$cluster),FUN=sd, 
#           na.rm=TRUE)
# 
# 
