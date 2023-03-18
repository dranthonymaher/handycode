# clear all objects from environment
rm(list = ls())
# set the working dir where the data is saved
mywd<-setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")
#read in the data
myfile<-"sepsisv2"
mydata<-read.csv(paste0(mywd,"/",myfile,".csv"))

#check the data
head(mydata)
summary(mydata)
str(mydata)
colnames(mydata)
# convert the dates to proper format
mydata$admission_date<-as.Date(mydata$admission_date,format="%d-%b-%y")
mydata$separation_date<-as.Date(mydata$admission_date,format="%d-%b-%y")

# fix some more formats
mydata$period<-as.character(mydata$period)

#check the structures
str(mydata)

# tabulate a few things and generate some barplots

# get a list of the column names
cns<-colnames(mydata)

i<-10
myvar<-cns[i]
myvar

#GENERATE TABLE
result<-table(mydata[myvar])
result

#plot barplot
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="category",ylab = "Number of seps",names.arg = ""
            ,main =  myvar,col = 'gray')
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue')
text(b1,0.0*result,names(result),cex =1,srt = 90,pos=4,col='black')

mydata$rand<-runif(length(mydata[,1]))






