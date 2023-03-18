# clear all objects from environment
rm(list = ls())
# set the working dir where the data is
mywd<-setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")
#read in the data
mydata<-read.csv(paste0(mywd,"/sepsisseps.csv"))

#check the data
head(mydata)
summary(mydata)
str(mydata)

# convert the dates to proper format
mydata$Admission.date<-as.Date(mydata$Admission.date,format="%d%b%y")
mydata$Separation.date<-as.Date(mydata$Separation.date,format="%d%b%y")

# fix some formats
mydata$Financial.year<-as.character(mydata$Financial.year)

#check the structures
str(mydata)

# tabulate a few things and generate some barplots
table(mydata$Hospital)
barplot(table(mydata$Hospital))
barplot(table(mydata$Indigenous.status))

# get a list of the column names
cns<-colnames(mydata)
i<-1
myvar<-cns[i]
myvar

#GENERATE TABLE
result<-table(mydata[myvar])
result

#plot barplot
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            ,main =  myvar,col = 'gray')
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue')
text(b1,0.0*result,names(result),cex =1,srt = 90,pos=4,col='black')






