
library(ggplot2)
library(gbm)

# remove all objects
rm(list = ls(all.names = TRUE))
mydata <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
head(mydata)
str(mydata)
summary(mydata)
hist(mydata$cases,100)

#compute death rate
mydata$deathrate<-mydata$deaths / mydata$cases
hist(mydata$deathrate,100)

#CHECK the classes of each column
sapply(mydata,class)

# note that some of the integer columns have been imported as integers - convert these to factors:
#mydata$fips<-as.factor(mydata$fips)



# get the population data
mypop<-read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/asrh/cc-est2018-alldata.csv")
# The key for AGEGRP is as follows: 0 = Total
# 1 = Age 0 to 4 years
# 2 = Age 5 to 9 years
# 3 = Age 10 to 14 years
# 4 = Age 15 to 19 years
# 5 = Age 20 to 24 years
# 6 = Age 25 to 29 years
# 7 = Age 30 to 34 years
# 8 = Age 35 to 39 years
# 9 = Age 40 to 44 years
# 10 = Age 45 to 49 years 
# 11 = Age 50 to 54 years 
# 12 = Age 55 to 59 years 
# 13 = Age 60 to 64 years 
# 14 = Age 65 to 69 years 
# 15 = Age 70 to 74 years 
# 16 = Age 75 to 79 years 
# 17 = Age 80 to 84 years 
# 18 = Age 85 years or older
# 
# The key for the YEAR variable is as follows: 1 = 4/1/2010 Census population
# 2 = 4/1/2010 population estimates base 3 = 7/1/2010 population estimate
# 4 = 7/1/2011 population estimate 5 = 7/1/2012 population estimate 6 = 7/1/2013 population estimate 7 = 7/1/2014 population estimate 8 = 7/1/2015 population estimate 9 = 7/1/2016 population estimate
# 10 = 7/1/2017 population estimate 11 = 7/1/2018 population estimate

colnames(mypop)

# only need to keep the first 10 cols
mypop<-mypop[,1:10]

#and only need to keep the rows corresponding to the most recent population estimate 
# (i.e. year  = 11)
mypop<-mypop[mypop$YEAR==11,]
#create a hybrid state - county name
mypop$statecounty<-paste0(mypop$STNAME,"_",mypop$CTYNAME)
colnames(mypop)
#now need to pivot out the male and female columns
mypop_males<-reshape(mypop[,c(11,7,9)], idvar = c("statecounty"), timevar = "AGEGRP", direction = "wide")
mypop_females<-reshape(mypop[,c(11,7,10)], idvar = c("statecounty"), timevar = "AGEGRP", direction = "wide")

# rejoin these back onto the mypop data
head(mypop)
mypopspine = unique(mypop[,c(1,2,3,4,5,11)])
q1<-merge(mypopspine,mypop_males,by.x = "statecounty", by.y = "statecounty")
colnames(q1)
# compute age group proprtions as prop of totals

q1$TOT_MALE.1p<-q1$TOT_MALE.1 / q1$TOT_MALE.0
q1$TOT_MALE.2p<-q1$TOT_MALE.2 / q1$TOT_MALE.0
q1$TOT_MALE.3p<-q1$TOT_MALE.3 / q1$TOT_MALE.0
q1$TOT_MALE.4p<-q1$TOT_MALE.4 / q1$TOT_MALE.0
q1$TOT_MALE.5p<-q1$TOT_MALE.5 / q1$TOT_MALE.0
q1$TOT_MALE.6p<-q1$TOT_MALE.6 / q1$TOT_MALE.0
q1$TOT_MALE.7p<-q1$TOT_MALE.7 / q1$TOT_MALE.0
q1$TOT_MALE.8p<-q1$TOT_MALE.8 / q1$TOT_MALE.0
q1$TOT_MALE.9p<-q1$TOT_MALE.9 / q1$TOT_MALE.0
q1$TOT_MALE.10p<-q1$TOT_MALE.10 / q1$TOT_MALE.0
q1$TOT_MALE.11p<-q1$TOT_MALE.11 / q1$TOT_MALE.0
q1$TOT_MALE.12p<-q1$TOT_MALE.12 / q1$TOT_MALE.0
q1$TOT_MALE.13p<-q1$TOT_MALE.13 / q1$TOT_MALE.0
q1$TOT_MALE.14p<-q1$TOT_MALE.14 / q1$TOT_MALE.0
q1$TOT_MALE.15p<-q1$TOT_MALE.15 / q1$TOT_MALE.0
q1$TOT_MALE.16p<-q1$TOT_MALE.16 / q1$TOT_MALE.0
q1$TOT_MALE.17p<-q1$TOT_MALE.17 / q1$TOT_MALE.0
q1$TOT_MALE.18p<-q1$TOT_MALE.18 / q1$TOT_MALE.0

q2<-merge(q1,mypop_females,by.x = "statecounty", by.y = "statecounty")
q2$TOT_FEMALE.1p<-q2$TOT_FEMALE.1 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.2p<-q2$TOT_FEMALE.2 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.3p<-q2$TOT_FEMALE.3 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.4p<-q2$TOT_FEMALE.4 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.5p<-q2$TOT_FEMALE.5 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.6p<-q2$TOT_FEMALE.6 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.7p<-q2$TOT_FEMALE.7 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.8p<-q2$TOT_FEMALE.8 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.9p<-q2$TOT_FEMALE.9 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.10p<-q2$TOT_FEMALE.10 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.11p<-q2$TOT_FEMALE.11 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.12p<-q2$TOT_FEMALE.12 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.13p<-q2$TOT_FEMALE.13 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.14p<-q2$TOT_FEMALE.14 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.15p<-q2$TOT_FEMALE.15 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.16p<-q2$TOT_FEMALE.16 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.17p<-q2$TOT_FEMALE.17 / q2$TOT_FEMALE.0
q2$TOT_FEMALE.18p<-q2$TOT_FEMALE.18 / q2$TOT_FEMALE.0

# keep only the cols with "p" in their name, and the first 5
mypop2<-q2[,c(1,2,3,4,5,6,grep("p", colnames(q2)))]
# create fips code for mypop2 to join on
mypop2$fips<-mypop2$STATE*1000 + mypop2$COUNTY
# join onto the coronavirus response data
mypop3<-merge(mydata,mypop2,by = "fips", all.x = TRUE)

# Get the diabetes data per county
diabdata<-read.csv("https://raw.githubusercontent.com/dranthonymaher/handycode/master/diabetesbycounty.csv")
# merge
mypop4<-merge(mypop3,diabdata,by.x = "fips", by.y = "FIPS",all.x = TRUE)

#need to add hypertension data later
# get data for latest date
mydata2<-mypop4[mypop4$date=="2020-03-27",]

plot(mydata2$Prevalence..2012..Males,mydata2$cases)
myglm





# extract list of variables that are factors
# listfactors<-names(Filter(is.factor, mydata))
# extract list of variables that are numeric / integers
# listnum<-names(Filter(is.numeric, mydata))


# Have a quick look at the summaries of all the variables
summary(mydata)

source("/Users/anthonymaher/Documents/R stuff/plotfactor.R")
plotfactor(1,mydata)
plotfactor(2,mydata)
plotfactor(3,mydata)
plotfactor(4,mydata)
plotfactor(5,mydata)
plotfactor(6,mydata)
plotfactor(7,mydata)
plotfactor(8,mydata)
plotfactor(9,mydata)
plotfactor(10,mydata)
plotfactor(11,mydata)
plotfactor(12,mydata)
plotfactor(13,mydata)
plotfactor(14,mydata)
plotfactor(15,mydata)
plotfactor(16,mydata)


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

mydata$contacttype<-as.factor(paste0(mydata$contact,"_",mydata$quarter,"_",mydata$dur1))


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
plotfactor(20,mydata)
plotfactor(21,mydata)
plotfactor(22,mydata)
plotfactor(23,mydata)

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
mform<-as.formula(paste("r1 ~ ",paste(colnames(trainset)[c(2:13)],collapse = "+")
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
  n.minobsinnode = 50,
  interaction.depth = 3,
  bag.fraction = 0.5,
  train.fraction = 0.8,
  cv.folds = 7,
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
prob=predict(myglm,newdata = testset,type=c("response"))
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
aprob<-aggregate(testset$r1,list(testset$myp),FUN=mean, na.rm = TRUE)

# png("pvo for hyp1.png")
plot(percentiles, ylim = c(0,1))
lines(aprob$x)
# dev.off()

#check the partial dependence plots
plot(mygbm, i.var = 1, lwd = 2, col = "blue", main = "")
plot(mygbm, i.var = 10, lwd = 2, col = "blue", main = "", type = "response")
plot(mygbm, i.var = 11, lwd = 2, col = "blue", main = "", type = "response")

#write.csv(q1,"gbmresults_all20190722.csv")

# simulation - set 17 new ones up with new type of contact
# generate new data set for scoring
con1<-as.list(levels(mydata$contacttype))

groupB<-mydata[mydata$campaign==0,cnnew$model==1]

 #i<-10
for (i in 1:length(levels(mydata$contacttype))){
groupB$contacttype<-con1[[i]]
#now predict new values
prob=predict(myglm,newdata = groupB,type=c("response"))
groupB$prob=prob
Sys.sleep(0.1)
print(groupB$contacttype[1])
names(groupB)[length(names(groupB))]<-paste0("prob_",con1[[i]])
}

#which is greatest?
colnames(groupB)
for (i in 1:length(groupB[,])){
groupB$maxs3[i]<-names(which.max(groupB[i,c(13:29)]))
}
table(groupB$maxs3)
matplot(t(as.vector(groupB[1000:2000,c(13:29)])), type = 'l')

# which is biggest uplift from col 21?
groupB$uplift_c_q1_long<-groupB$prob_cellular_q1_long-groupB$prob_NA_NA_NoContact








