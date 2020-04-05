
library(ggplot2)
library(gbm)
setwd("Documents/USAdiabetesbycounty.csv/")
# remove all objects
rm(list = ls(all.names = TRUE))
# read the live data
casedata <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
head(casedata)
str(casedata)
# fix the date column
casedata$date<- as.Date(casedata$date)
summary(casedata)
str(casedata)
hist(casedata$cases,100)

#compute death rate
casedata$deathrate<-casedata$deaths / casedata$cases
#check it
hist(casedata$deathrate,100)
# create state - county compund variable
casedata$statecounty<-paste0(casedata$state, "_", casedata$county)

# work on understanding the course of the cases for each county:

#1. order the dataset by county and by date
casedata<-casedata[order(casedata$statecounty,casedata$date),]
#2. get the fist date for each one using "by()"
firstdaterows<-by(casedata, casedata$statecounty, head, n=1)
#3. make it a data frame
firstdf<-do.call("rbind", as.list(firstdaterows))
# keep only the statecounty adn the dateoffirstcase cols
firstdf<-firstdf[,c(1,8)]
#4. Keep only the statecounty and firstdatecols
firstdf1<-as.data.frame(cbind(firstdf$statecounty ,firstdf$date))
names(firstdf1)<-c("statecounty","dateoffirstcase")
str(firstdf1)
firstdf1$dateoffirstcase<-as.Date(as.numeric(firstdf1$dateoffirstcase))
#now left join that on
casedata3<-merge(casedata,firstdf,by = "statecounty",all.x = TRUE)
# now can compute the dayse since first case
str(casedata3)
casedata3$dayssince1case<-as.integer(casedata3$date.x - casedata3$date.y)
hist(casedata3$dayssince1case)
#create a data frame with stats for 7 and 14 days after first case only
c7<-casedata3[casedata3$dayssince1case==7,]
c10<-casedata3[casedata3$dayssince1case==10,]
c14<-casedata3[casedata3$dayssince1case==14,]

# rename the colnames for uniqueness
colnames(c7)
names(c7)[c(6,7)]<-c("cases.at.7.days","deaths.at.7.days")
colnames(c10)
names(c10)[c(6,7)]<-c("cases.at.10.days","deaths.at.10.days")
colnames(c14)
names(c14)[c(6,7)]<-c("cases.at.14.days","deaths.at.14.days")

# merge the two together
ctemp<-merge(c7[,c(1,3,4,5,6,7)],c10[,c(1,6,7)],by = "statecounty",all.x = TRUE)
c7.14<-merge(ctemp,c14[,c(1,6,7)],by = "statecounty",all.x = TRUE)
c7.14$c10overc7<-c7.14$cases.at.10.days / c7.14$cases.at.7.days
hist(c7.14$c10overc7,50)



# get the population data from US census
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
head(mypop)
# only need to keep the first 10 cols - only interested in the total male and female pops for now
mypop<-mypop[,1:10]

#and only need to keep the rows corresponding to the most recent population estimate 
# (i.e. year  = 11)
mypop<-mypop[mypop$YEAR==11,]
#create a hybrid state - county name to assist with the pivot
mypop$statecounty<-paste0(mypop$STNAME,"_",mypop$CTYNAME)


# create a new variable to calculate the proportion of those over 60 in each county
#start by assigning each age group to one of those two buckes
mypop$under60<-ifelse(mypop$AGEGRP<13,"<60",">=60")
# aggregate over that col
a1<-aggregate(mypop$TOT_POP,list(mypop$statecounty,mypop$under60),FUN = sum)
# name the variables
names(a1)<-c("statecounty", "indicator", "peopleover60")
#keep only the data for the number of people over 60
a1<-a1[a1$indicator==">=60",]
# Get the total pops rows only so we can join on and compute proportions
totpop<-mypop[mypop$AGEGRP==0,]
a2<-merge(a1,totpop[,c("statecounty","TOT_POP")],by =  "statecounty", all.x = TRUE)
a2$propover60 = a2$peopleover60 / a2$TOT_POP
hist(a2$propover60)

# now join that data back onto the main mypop table
mypop<-merge(mypop, a2[,c(1,3,5)], by = "statecounty", all.x = TRUE)


colnames(mypop)
#now need to pivot out the male and female columns
mypop_males<-reshape(mypop[,c(1,8,10)], idvar = c("statecounty"), timevar = "AGEGRP", direction = "wide")
mypop_females<-reshape(mypop[,c(1,8,11)], idvar = c("statecounty"), timevar = "AGEGRP", direction = "wide")

# rejoin these back onto the mypop data
head(mypop)
colnames(mypop)
# get the data "spine" - only keep the unique rows for the selected colums
mypopspine = unique(mypop[,c(1,3,4,5,13,14)])
# merge males population data onto that
q1<-merge(mypopspine,mypop_males,by.x = "statecounty", by.y = "statecounty")
colnames(q1)
# compute age group proprtions as prop of totals for MALES
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

#do the same for the sheilas
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

# build a PCA model on these data
# model<- princomp(mypop2[,7:42], cor = TRUE)
# summary(model) # first 4 components account for 80% of the variation
# barplot(model$loadings[,1], xlab = names(mypop2[,7:42]), las =2,cex.names  = 0.7)
# barplot(model$loadings[,2])
# barplot(model$loadings[,3])
# barplot(model$loadings[,4])
# The output from the PCA model shows we 
# really only need age groups 14 and 6 - i.e. 25-29 and 60-65 props
# as these explain most of the variation

colnames(mypop2)
#define columns to keep
colstokeep = c("fips","peopleover60","propover60","TOT_MALE.6p","TOT_MALE.14p","TOT_FEMALE.6p","TOT_FEMALE.14p")

# join onto the coronavirus response data
mypop3<-merge(c7.14,mypop2[,colstokeep],by = "fips", all.x = TRUE)
head(mypop3)

# Get the diabetes data per county
diabdata<-read.csv("https://raw.githubusercontent.com/dranthonymaher/handycode/master/diabetesbycounty.csv")
# merge
mypop4<-merge(mypop3,diabdata,by.x = "fips", by.y = "FIPS",all.x = TRUE)

#need to add hypertension data later
hypert.data<-read.csv("hypertensionbycounty.csv")
head(hypert.data)
mypop5<-merge(mypop4,hypert.data[,c("FIPS","Total.male.hypert","total.female.hypert")],by.x = "fips", by.y = "FIPS",all.x = TRUE)


# MODELLING ---------------------------------------------------------------
mydata<-mypop5
  
#decide which cols to include in the model
cn<-colnames(mydata)
cn
write.csv(cn,"cn.csv")


colstopred = c(
  #"fips",
  #"statecounty",
  #"county",
  #"state",
  "cases.at.7.days",
  "deaths.at.7.days",
  "cases.at.10.days",
  "deaths.at.10.days",
  #"cases.at.14.days",
  #"deaths.at.14.days",
  "c10overc7",
  #"peopleover60",
  "propover60",
  #"TOT_MALE.6p",
  "TOT_MALE.14p",
  #"TOT_FEMALE.6p",
  "TOT_FEMALE.14p",
  #"Location",
  "Prevalence..2012..Both.Sexes",
  "Prevalence..2012..Females",
  "Prevalence..2012..Males",
  "Total.male.hypert"
  , "total.female.hypert"
  
)
# go to the folder where you just created "cn.csv". Add a column called "toinclude". 
# St to 1 all the cols to include, and 0 the rest.

#split the data into training and test set
#set.seed(1223)
holdoutrows<-sample(c(1:length(mydata[,1])),length(mydata[,1])*0.2)
keeprows<-setdiff(c(1:length(mydata[,1])),holdoutrows)
length(holdoutrows)+length(keeprows)
intersect(holdoutrows,keeprows) # this should be empty

# read in the metadata columns to include in the model
trainset<-mydata[keeprows,]
testset<-mydata[holdoutrows,]

# add the response
# define response:
resp<-"cases.at.14.days"
#trainresp<-mydata[keeprows,which(colnames(mydata)==resp)]
#testresp<-mydata[holdoutrows,which(colnames(mydata)==resp)]

#trainset<-as.data.frame(cbind(trainresp,trainset))
#testset<-as.data.frame(cbind(testresp,testset))

#which(colnames(trainset)=="r1")
#names(trainset)[1]<-"r1"
#names(testset)[1]<-"r1"
trainset<-trainset[complete.cases(cbind(trainset[,resp],trainset[,colstopred])),]
testset<-testset[complete.cases(cbind(testset[,resp],testset[,colstopred])),]


#generate a formula for the model
mform<-as.formula(paste(resp,"~",paste(colstopred,collapse = "+")
))
#check it
mform

#try a glm first
myglm <- glm(mform, data = trainset, family = poisson(link = "log"))
summary(myglm)


# PREDICTION --------------------------------------------------------------
# generate response vector for test set and compare to actuals
pred=predict(myglm,newdata = testset,type=c("response"))
testset$pred=pred
testset$diffs = testset$pred - testset[,resp]
s1<-sum(testset$diffs^2) / length(testset$diffs)
plot(testset[,resp],testset$pred, main = paste0("sumsq = ",s1),xlim = c(0,100),ylim = c(0,100))
abline(0,1)




















mygbm<-gbm(
  mform,
  data = trainset,
  distribution="poisson",
  n.trees = 3000,
  shrinkage = 0.01,
  n.minobsinnode = 50,
  interaction.depth = 3,
  bag.fraction = 0.5,
  train.fraction = 0.8,
  cv.folds = 7,
  verbose = TRUE
)
best.iter <- gbm.perf(mygbm, plot.it = TRUE , oobag.curve = FALSE,method="cv")
best.iter

q1<-summary(mygbm, n.trees = best.iter
            ,method = relative.influence
            #,method = permutation.test.gbm
)
q1

#prediction
predgbm=predict(mygbm,newdata = testset,type=c("response"))
testset$predgbm=predgbm
plot(testset$r1,testset$predgbm) #,xlim = c(0,100),ylim = c(0,100))
abline(0,1)
