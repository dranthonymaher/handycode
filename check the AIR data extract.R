# clear all objects from environment
rm(list = ls())
# set the working dir where the data is saved

mywd<-setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")

#mywd<-setwd("//ausydfsr12/_ADV/Tech/FY21/Services Australia - Managed Services/07 - AIR Strategic Paper")
#read in the data
myfile<-"August Pend Data"
mydata<-read.csv(paste0(mywd,"/",myfile,".csv"))



#check the data
head(mydata)
summary(mydata)
str(mydata)
colnames(mydata)

# get a list of the provider types
provtypes<-unique(mydata$PROVIDER_TYPE_CDE)
provtypes

#try some scatter plots
plot(mydata$Transmissions, mydata$Percent.Pended)
i<-2
for (i in 1:length(provtypes)){
png(filename = paste0("provscatterplot_",substr(provtypes[i],1,20),".png"),width = 1.0*800, height = 1.0*450)
temp.df<-mydata[mydata$PROVIDER_TYPE_CDE==provtypes[i],]
l1<-length(temp.df[,1])
plot(temp.df$Transmissions, temp.df$Percent.Pended,main = paste0(provtypes[i]," total sites=", l1)
     ,xlab = "Transmissions",ylab = "Percent pended", pch = 19,cex =1.5)
dev.off()
}
