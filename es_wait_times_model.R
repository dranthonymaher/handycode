#import the required packages
library(gbm)
library(ROCR)
library(randomForest)
library(ggplot2)

mydata<- read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/MYH-ES_sol.csv")
# check it
head(mydata)
# check the structure of all variables that they have been appropriately defined
str(mydata)

#need to shift out the median wait times by one year as the
# idea of the model is to predict the following year's wait times
# using this years data (e.g. use 2014 data to predict 2015 wait times)

#create new variable shifting back the wait time by 1 year
mydata$rep_end_dte_shift<- mydata$reporting_end_date-1

# check a random sample of them
mydata[sample(1:length(mydata[,1]),10),] # looks ok

colnames(mydata)
#extract the response vars
newresp<-mydata[, c(which(colnames(mydata)=="reporting_unit_name")
                    ,which(colnames(mydata)=="rep_end_dte_shift")
                    ,which(colnames(mydata)=="Median_wait_time.days."))]
names(newresp)<-c("reporting_unit_name","reporting_end_date","next_years_wait_time")
colnames(newresp)
#join back onto teh original data
mydata2<-merge(mydata,newresp,by=c("reporting_unit_name","reporting_end_date"), all.x = TRUE)

# looks good - now remove the columns we no longer need
colnames(mydata2)
mydata<-mydata2[,c("reporting_unit_name","reporting_end_date","peer_group_name","surgeon_speciality_count_x", "procs_count_x",
"Surgeon_speciality_avg","Procs_count_avg","surgeon_ratio" ,"procs_ratio","next_years_wait_time")]

head(mydata)

source("plotfactor.R")
plotfactor(2, mydata, "next_years_wait_time")
plotfactor(3, mydata, "next_years_wait_time")
plotfactor(5, mydata, "next_years_wait_time")
plotfactor(6, mydata, "next_years_wait_time")
plotfactor(7, mydata, "next_years_wait_time")
plotfactor(8, mydata, "next_years_wait_time")
plotfactor(9, mydata, "next_years_wait_time")



# summarise all variables
summary(mydata)

# Define the training, test and hold out sets
xtrain<-mydata[mydata$reporting_end_date<2018,]
xtest<-mydata[mydata$reporting_end_date==2018,]
xholdout<-mydata[mydata$reporting_end_date>2018,]


# Reporting end date has come through as interger - convert to factor
xtrain$reporting_end_date<-as.factor(xtrain$reporting_end_date)

# Noticed there are NAs in the Median_wait_time variable - remove these
xtrain<-xtrain[is.na(xtrain$next_years_wait_time)==FALSE,]

# print the list of colnames
colnames(xtrain)

# define the formula
myformula <- as.formula(next_years_wait_time~ 
                          #reporting_end_date
                        +peer_group_name
                        +surgeon_speciality_count_x
                        +procs_count_x
                        #+Surgeon_speciality_avg
                        #+Procs_count_avg
                        #+surgeon_ratio
                        #+procs_ratio
                        )
myformula

# fit the model
model<-glm(myformula,data = xtrain, family = "gaussian")
#generate model summary output
summary(model)

# check the R^2
with(summary(model), 1 - deviance/null.deviance)
#  0.1687129

# check the prediction
xtest$prd.glm<-predict.glm(model,newdata = xtest, type = "response")

# plot it to check
plot(xtest$next_years_wait_time,xtest$prd.glm)
abline(0, 1) #add a 45 degree line


