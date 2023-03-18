# install.packages("klaR")
library(klaR)
library(ROCR)
library(ggplot2)
library(gbm)


#set the working directory
#setwd("C:/Users/anthonym/OneDrive - Quantium/R stuff")
setwd("/Users/anthonymaher/Documents/")

#  import the data
mydata<-NULL
mydata<-read.csv("/Users/anthonymaher/Documents/diabetic_data.csv")

#check the first 5 rows of the data
head(mydata)

#CHECK the classes of each column
sapply(mydata,class)

  # note that some of the  "id" columns have been imported as integers - convert these to factors:
mydata$admission_type_id<-as.factor(mydata$admission_type_id)
mydata$discharge_disposition_id<-as.factor(mydata$discharge_disposition_id)
mydata$admission_source_id<-as.factor(mydata$admission_source_id)

# extract list of variables that are factors
listfactors<-names(Filter(is.factor, mydata))
# extract list of variables that are numeric / integers
listnum<-names(Filter(is.numeric, mydata))
#remove the first 2 since they are ids
listnum1<-listnum[-c(1,2)]

# Have a quick look at the summaries of all the variables
summary(mydata)

# cn<-colnames(mydata)
# i<-which(cn=="weight")
i<-1
#generate bar plots for factor variables
for (i in 1:length(listfactors)){
  png(file = paste0("cplot_fact_",listfactors[i],".png"))
  barplot(summary(mydata[,which(colnames(mydata)==listfactors[i])]),main = listfactors[i], las=2)
  dev.off()
}

#generate density plots for integer variables

for (i in 1:length(listnum1)){
  # Sys.sleep(0.01)
  # print(listnum1[i])
  png(file = paste0("cplot_num_",listnum1[i],".png"))
  
  plot(hist(
    mydata[,which(colnames(mydata)==listnum1[i])]
  ),
  main = listnum1[i], las=2)
  dev.off()
}

boxplot(mydata[,listnum1], las=2)


# Summarise the data to understand variations in aLoS
aggregate.data.frame(mydata$time_in_hospital,by=list(mydata$admission_type_id), FUN = mean)
# not much variation in admission type

a2<-aggregate.data.frame(mydata$time_in_hospital,by=list(mydata$age), FUN = mean)
barplot(as.vector(a2$x),names.arg =  a2$Group.1,las=2, ylab = "Average length of stay")
# older patients stay longer

# check stat significance

flagFor60=ifelse(as.numeric(substr(as.vector(mydata$age),2,2))<6,"under 60","over 60")
mydata<-cbind(mydata,flagFor60)

#check it
aggregate.data.frame(mydata$time_in_hospital,by=list(mydata$age,mydata$flagFor60), FUN = length)

#GET ALOS UNDER AND OVER 60
a3<-aggregate.data.frame(mydata$time_in_hospital,by=list(mydata$flagFor60), FUN = mean)
barplot(as.vector(a3$x),names.arg =  a3$Group.1,las=2, ylab = "Average length of stay")
plot(density(mydata[mydata$flagFor60=="under 60",which(colnames(mydata)=="time_in_hospital")],adjust = 2),col="blue")
lines(density(mydata[mydata$flagFor60=="over 60",which(colnames(mydata)=="time_in_hospital")],adjust = 2),col = "red")
#test statistical significance of this
x = mydata[mydata$flagFor60=="under 60",which(colnames(mydata)=="time_in_hospital")]
y = mydata[mydata$flagFor60=="over 60",which(colnames(mydata)=="time_in_hospital")]
t.test(x,y)

#according to ttest the means are significantly different

a2<-aggregate.data.frame(mydata$time_in_hospital,by=list(mydata$gender), FUN = mean)
barplot(as.vector(a2$x),names.arg =  a2$Group.1,las=2, ylab = "Average length of stay")
# not much difference in gender los

head(mydata)

aggregate.data.frame(mydata$time_in_hospital,by=list(mydata$diabetesMed), FUN = mean)

drugcols<-c(25:47)

a1<-aggregate.data.frame(mydata$time_in_hospital,by=mydata[,drugcols], FUN = length)
a1[order(a1$x, decreasing = TRUE),]


# create flags for drugs
for (i in 1:length(drugcols)){
  mydata[,paste0("f_",colnames(mydata[drugcols[i]]))]<-ifelse(mydata[colnames(mydata[drugcols[i]])]!="No",1,0)
}
colnames(mydata)

newdrugcols<-c(52:74)
a1<-NULL
a1<-aggregate.data.frame(mydata$time_in_hospital,by=mydata[,newdrugcols], FUN = length)
a1[order(a1$x, decreasing = TRUE),]

# k-modes clustering
cluster.results<-kmeans(mydata[,newdrugcols],5,iter.max = 10)
cluster.results$withindiff



# create new variables to classify diabetes meds
mydata$insulins<-mydata$f_insulin
mydata$biguandines<-mydata$f_metformin
mydata$combotherapy<-rowSums(mydata[,70:74])
mydata$glinides<-rowSums(mydata[,c(53,54)])
mydata$sulfonides<-rowSums(mydata[,c(55:60)])
mydata$aginhibitors<-rowSums(mydata[,c(63,64)])
mydata$thiazols<-rowSums(mydata[,c(61,62,65,66)])
mydata$otherdrugs<-rowSums(mydata[,c(67,68)])


head(mydata)

# k-means clustering on grouped data
expl<-NULL
for (i in 1:20) {
  kmeansmodel<-kmeans(mydata[,75:82],i)
  expl[i]<- kmeansmodel$betweenss / kmeansmodel$totss
}
png(file = paste0("kmeans_variance_plot",".png"))
plot(expl, type = "l",main = "Between SS as a proportion of total SS", las=2)
lines(diff(expl),col = "blue")
lines(diff(diff(expl)),col = "red")
dev.off()

c<-c(seq(0,0.5,0.1),seq(0.55,1,0.05))
plot(c,type = "l")
lines(abs(diff(c)),col="blue")
lines(abs(diff(diff(c))),col="red")

# 9 groups looks like a go-er
mymodel<-kmeans(mydata[,75:82],9)
mydata$cluster<-mymodel$cluster
aggregate(mydata[,75:82],by=mydata["cluster"],FUN = mean)
aggregate(mydata[,75:82],by=mydata["cluster"],FUN = length)

r1<-aggregate(mydata[,75:82],by=mydata["cluster"],FUN = mean)
r2<-aggregate(mydata[,1],by=mydata["cluster"],FUN = length)
write.csv(cbind(r1,r2), file = "clusterresult.csv")

# modelling
#  can we predict readmission rates using these data

#build response variable - just look at <30 day readmissions first
mydata$readmitl30<-as.numeric(ifelse(mydata$readmitted=="<30",1,0))

#generate csv file to label with inclusion and exclusion
# write.csv(colnames(mydata),file = "predictorcols.csv") #comment this out so it can't be accidentally overwritten

# NOTE: BEFORE GOING ANY FURTHER: 
# open up the csv YOU JUST SAVED and lable the cols you want to use as predictors
# by creating a new column called "toinclude" and put a 1 against the variables
# you want to include in the model, then save it as a .csv



mydata$cluster<-as.factor(mydata$cluster)

# get ris of unknown gender
mydata<-mydata[!(mydata$gender=="Unknown/Invalid"),]


#read that file back in
predictorcols<-read.csv("predictorcols.csv")

#generate a formula for the model
mform<-as.formula(paste("readmitl30 ~ ",paste(colnames(mydata)[predictorcols$toinclude==1],collapse = "+")))
#check it
mform

summary(mydata[,predictorcols$toinclude==1])

#try a glm first
myglm <- glm(mform, data = mydata, family = binomial(link = "logit"))
summary(myglm)

prob=predict(myglm,type=c("response"))
mydata$prob=prob
pred<-prediction(prob,mydata$readmitl30)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line  


cn<-colnames(mydata)
i<-1
for (i in 1:length(cn)){
l1<-length(levels(mydata[,i]))
l2<-length(unique(levels(mydata[,i])))
x<-l1==l2
Sys.sleep(0.01)
ifelse(l1==l2,print(paste0(cn[i]," is fine")),print(paste0(cn[i]," is not happy")))
}
#run gbm
plot(mform, data = mydata)

mydata <- mydata[sample(nrow(mydata)),]
randomr<-ifelse(runif(length(mydata[,1]))>0.3,1,0)
mydata$randomr<-randomr
dev.off()
mycols<-c("readmitl30",colnames(mydata)[predictorcols$toinclude==1])
#generate a formula for the model
mform<-as.formula(paste("readmitl30 ~ ",paste(colnames(mydata)[predictorcols$toinclude==1],collapse = "+")))
#check it
mform
mydatamodel<-mydata[mycols]
colnames(mydatamodel)
# RESPONSE MUST BE NUMERIC!!!!
# DATA FRAME CAN ONLY HAVE VARIABLES THAT ARE USED IN THE FONRMULA!!!!!
mygbm<-gbm(
  mform,
  #readmitl30 ~ age+race+max_glu_serum,
  # data = mydata[sample(1:length(mydata[,1]),5000),],
  data = mydatamodel,
  distribution="bernoulli",
  n.trees = 500,
  shrinkage = 0.05,
  interaction.depth = 2,
  bag.fraction = 0.5,
 train.fraction = 0.8,
  cv.folds = 3,
  verbose = FALSE
)

best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="test")
best.iter
#Determine predictions for the ScoringData

prob=predict.gbm(mygbm,mydatamodel,best.iter,"response")
mydata$prob=prob
pred<-prediction(prob,mydata$readmitl30)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line  

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
