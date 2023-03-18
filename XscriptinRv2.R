
library(ggplot2)
library(gbm)

# remove all objects
rm(list = ls(all.names = TRUE))


#set the working directory
#!!!- each user will need to change this for themselves to the directory that contains their data!!!
setwd("/Users/anthonymaher/Documents/WooliesXtest")
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")

#  import the data
mydata<-NULL
mydata<-read.csv("DataScience_Test.csv")

#create unique customer id
mydata$customerid<-c(1:length(mydata[,1]))
#check it
head(mydata)


#CHECK the classes of each column
sapply(mydata,class)

# note that some of the  "id" columns have been imported as integers - convert these to factors:
mydata$campaign<-as.factor(mydata$campaign)

# creare numeric response var
r1<-ifelse(mydata$y=="yes",1,0)
mydata$r1<-r1

# extract list of variables that are factors
# listfactors<-names(Filter(is.factor, mydata))
# extract list of variables that are numeric / integers
# listnum<-names(Filter(is.numeric, mydata))


# Have a quick look at the summaries of all the variables
summary(mydata)



#consolidate the type of contact
mydata$quarter<-as.factor(ifelse(mydata$month=="jan", "q1"
                                 ,ifelse(mydata$month=="feb", "q1"
                                         ,ifelse(mydata$month=="mar", "q1"
                                                 ,ifelse(mydata$month=="apr", "q2"
                                                         ,ifelse(mydata$month=="may", "q2"
                                                                 ,ifelse(mydata$month=="jun", "q2"
                                                                         ,ifelse(mydata$month=="jul", "q3"
                                                                                 ,ifelse(mydata$month=="aug", "q3"
                                                                                         ,ifelse(mydata$month=="sep", "q3"
                                                                                                 ,ifelse(mydata$month=="oct", "q4"
                                                                                                         ,ifelse(mydata$month=="nov", "q4"
                                                                                                                 ,ifelse(mydata$month=="dec", "q4","none")))))))))))))

# check call stats
calllengths<-mydata$duration[mydata$campaign == 1]
summary(calllengths)
#median is 184 - use this a benchmark

mydata$dur1<-as.factor(ifelse(mydata$duration==0,"NoContact",
                              ifelse(mydata$duration<=180,"short","long")))

#mydata$contacttype<-as.factor(paste0(mydata$contact,"_",mydata$quarter,"_",mydata$dur1))
mydata$contacttype<-as.factor(paste0(mydata$contact,"_"
                                     #,mydata$quarter,"_"
                                     ,mydata$dur1))


summary(mydata)
# remove the rows where customer was in defauls as n=3 for yes
mydata<-mydata[mydata$default!="yes",]

# data categorisation and feature creation
#after checking, we see that those in the labour force have different response rates
# to those not in the labor force
#but students will be qualitatively different to retirees
#so lets group up those in the labor force
mydata$job2<-as.factor(ifelse(mydata$job=="unemployed", "unemployed"
                    ,ifelse(mydata$job=="retired", "retired"
                            ,ifelse(mydata$job=="student", "student"
                            ,"inlaborforce"))))
mydata$job3<-as.factor(ifelse(mydata$job=="retired", "NILF"
                                      ,ifelse(mydata$job=="student", "NILF"
                                              ,"inlaborforce")))

mydata$age1<-as.factor(ifelse(mydata$age<=25,"a.lt 25",
                              ifelse(mydata$age<=60,"b. 25-60","c. over 60")))

mydata$edu1<-as.factor(ifelse(mydata$education=="professional.course","tertiary",
                              ifelse(mydata$education=="university.degree","tertiary","lt.tertiary")))

str(mydata)

#decide which cols to include in the model
cn<-colnames(mydata)
write.csv(cn,"cn.csv")


mydata_old<-mydata

#split the data 
set.seed(1223)
holdoutrows<-sample(c(1:length(mydata[,1])),length(mydata[,1])*0.2)
keeprows<-setdiff(c(1:length(mydata[,1])),holdoutrows)
length(holdoutrows)+length(keeprows)
intersect(holdoutrows,keeprows)



cnnew<-read.csv("cnnew2.csv")
trainset<-mydata[keeprows,cnnew$model==1]
testset<-mydata[holdoutrows,cnnew$model==1]

trainset<-cbind(mydata$r1[keeprows],trainset)
testset<-cbind(mydata$r1[holdoutrows],testset)

#which(colnames(trainset)=="r1")
names(trainset)[1]<-"r1"
names(testset)[1]<-"r1"
#generate a formula for the model
mform<-as.formula(paste("r1 ~ ",paste(colnames(trainset)[c(2:length(colnames(trainset)))],collapse = "+")
                        #,"+housing*marital"
                        #,"+month*duration"
                        #,"+age*contact"
                        ))
#check it
mform

trainset$contacttype<-relevel(trainset$contacttype, ref = "NA_NA_NoContact")
#try a glm first
myglm <- glm(mform, data = trainset, family = binomial(link = "logit"))
summary(myglm)

mygbm<-gbm(
  mform,
  data = trainset,
  distribution="bernoulli",
  n.trees = 1000,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  interaction.depth = 3,
  bag.fraction = 0.5,
  train.fraction = 0.8,
  cv.folds =3,
  verbose = TRUE
)
summary(mygbm)
best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="test")
best.iter
best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="cv")
best.iter

q1<-summary(mygbm, n.trees = best.iter
            ,method = relative.influence
            #,method = permutation.test.gbm
)
q1

library(ROCR)
#prediction
prob=predict(mygbm,newdata = testset,type=c("response"))
testset$prob=prob
pred<-prediction(prob,testset$r1)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line 



percentiles<-quantile(testset$prob, probs = seq(0, 1, by= 0.01)) # decile
plot(percentiles)
myp<-ecdf(percentiles)(testset$prob)
testset$myp<-myp
plot(testset$myp,testset$prob)
aprob<-aggregate(testset$r1,list(testset$myp),FUN=mean, na.action= "ignore")

# png("pvo for hyp1.png")
plot(percentiles, ylim = c(0,1))
lines(aprob$x)
# dev.off()

#check the partial dependence plots
plot(mygbm, i.var = 1, lwd = 2, col = "blue", main = "", type = "response")
plot(mygbm, i.var = 8, lwd = 2, col = "blue", main = "", type = "response")
plot(mygbm, i.var = 9, lwd = 2, col = "blue", main = "", type = "response")
plot(mygbm, i.var = 6, lwd = 2, col = "blue", main = "", type = "response")

#write.csv(q1,"gbmresults_all20190722.csv")

# simulation - set 17 new ones up with new type of contact
# generate new data set for scoring
con1<-as.list(levels(mydata$contacttype))

groupB<-mydata[mydata$campaign==0,cnnew$model==1]
groupB[1,]
groupB$contacttype[1]<-con1[[1]]
groupB$contacttype[1]
predict(mygbm,newdata = groupB[1,],type=c("response"))

 i<-1
for (i in 1:length(levels(mydata$contacttype))){
groupB$contacttype<-as.factor(con1[[i]])
#now predict new values
prob=predict(mygbm,newdata = groupB,type=c("response"))
groupB$prob=prob
Sys.sleep(0.1)
print(groupB$contacttype[1])
names(groupB)[length(names(groupB))]<-paste0("prob_",con1[[i]])
}

 colnames(groupB)
for (i in 1:length(groupB[,])){
groupB$maxs3[i]<-names(which.max(groupB[i,c(10:14)]))
}
table(groupB$maxs3)
matplot(t(as.vector(groupB[,c(10:14)])), type = 'l')

# which is biggest uplift from col 21?
groupB$uplift_c_long<-groupB$prob_cellular_long-groupB$prob_NA_NoContact
groupB$uplift_c_short<-groupB$prob_cellular_short-groupB$prob_NA_NoContact
groupB$uplift_t_long<-groupB$prob_telephone_long-groupB$prob_NA_NoContact
groupB$uplift_t_short<-groupB$prob_telephone_short-groupB$prob_NA_NoContact
# do this for each one then compute max uplift



#which is greatest uplift?
x<-NULL
for (i in 1:length(groupB[,1])){
  x[i]<-which.max(groupB[i,10:14])
}
plot(x)




