

setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")

mydata<-read.csv("Training_dataset_Original.csv")
head(mydata)

sapply(mydata,class)
library(missForest)

# Check the data
summary(mydata)
# fix up missing values by telling R which are NAs
mydata[mydata=="na"]<-NA
mydata[mydata=="N/A"]<-NA
mydata[mydata=="missing"]<-NA

numericcols<-2:48
# convert the factor cols to numnerics
mydata[numericcols]<-sapply(mydata[numericcols], as.numeric)

# Check the data
summary(mydata)

# 0 imputation of NAs
mydata0<-mydata
mydata0[is.na(mydata0)]<-0

summary(mydata0)

# median imputation
mydata.med<-mydata

for (i in 1:length(colnames(mydata.med))){
mydata.med[,i][is.na(mydata.med[,i])]<-median(mydata.med[,i],na.rm = TRUE)
}
summary(mydata.med)

summary(mydata)

# import the column names
cns<-as.data.frame(read.csv("Data_Dictionary.csv"))
# abbreviate them
cns$abbrevs<-as.vector(abbreviate(cns$Definition))
# rename the cols with the abbreviations
names(mydata0)<-cns$abbrevs

s1<-strsplit(as.character(cns$Definition[2]), split = " ")
s1
substr(s1,1,33)

# generate a series of histograms of the datasets
dev.off()
for (i in 2:length(colnames(mydata))){
  Sys.sleep(0.01)
  print(paste0(i,"_",cns$Name[i],"_" , cns$Definition[i]))
  png(filename=paste0("myhist",i, ".png"))
  plot(hist(mydata[,i],20),main=strwrap(paste0(cns$Name[i],"  " , cns$Definition[i]), width = 70))
  dev.off()
}
# there are problems with Annual income
# estimated market value of property
# Problem with maximum tenure of education loans - looks like it should be months not days
#problem with duration of says at current address (goes up to 600 years!)
#
# Create a histogram for default rate by the Credit worthiness score, what trend do you observe and why? 
h1<-hist(mydata$mvar1)
c1<-cut(mydata$mvar1,breaks = h1$breaks)
a1<-aggregate(mydata$default_ind,list(c1), FUN = mean,na.rm=TRUE)
plot(h1, main = "aggregate probability of default by credit score")
lines(h1$mids,a1$x*5000, col="red")
text(h1$mids,a1$x*5000,round(a1$x*100), col="blue")







