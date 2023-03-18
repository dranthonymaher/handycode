rm(list=ls())

# read in all opportunities
mydata<- read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/crmoutput.csv")
# check it
head(mydata)

table(mydata$Primary.Service.Type)

# read in the analytics opportunities
manalyt<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/crmoutput_analytics.csv")
# check it
manalyt<-as.data.frame(manalyt[,c("Opportunity.ID","Referred.By")])
head(manalyt)

manalyt$isanalytics<-1
manalyt<-manalyt[,c("Opportunity.ID","isanalytics")]

mydata<-manalyt<-merge(mydata,manalyt, by="Opportunity.ID", all.x = TRUE)
mydata$isanalytics[is.na(mydata$isanalytics)]<-0

# read in the user data
myusers<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/crmusers.csv")

# join on the user info
colnames(myusers)
myusers<-myusers[,c("Full.Name","User.Name","Employee.Status","Job.Title","Division.Name",
                    "Cost.Centre","Staff.Classification","State.Responsible")]
# get a list of users with the same name as others in there
dupusers<-myusers[duplicated(myusers$Full.Name),]
dupusers<-as.data.frame(unique(dupusers$Full.Name))
dupusers$dupuser<-1
names(dupusers)<-c("Full.Name","dupuser")
#join back onto myusers
myusers<-merge(myusers,dupusers,by = "Full.Name", all.x=TRUE)

myusers$dupuser[is.na(myusers$dupuser)]<-0

myusersd<-myusers[myusers$dupuser==1,]
myusersnod<-myusers[myusers$dupuser==0,]

# export nad clean
#write.csv(myusersd,"myusersdups.csv")
# import the cleaned dups
cleanedsups<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/myusersdups-cleaned.csv")

# rebuild user tables
myusersfixed<-rbind(myusersnod,cleanedsups)

# now join on the users to the data
colnames(myusersfixed)
#newnames.oppo<-lapply(colnames(myusersfixed), paste0, ".OppO")
newnames.oppo<-lapply("OppO.", paste0, colnames(myusersfixed))
myu.oppo<-myusersfixed
names(myu.oppo)<-unlist(newnames.oppo)
head(myu.oppo)
mydata<-merge(mydata,myu.oppo, by.x = "Opportunity.Owner",by.y = "OppO.Full.Name", all.x = TRUE)
head(mydata)
mydata$Total.Fees..AUD.[is.na(mydata$Total.Fees..AUD.)]<-0
mydataanalyt<-mydata[mydata$isanalytics==1,]

colnames(mydataanalyt)
# do some aggregations
aggregate(cbind(mydataanalyt$Total.Fees..AUD.)
          ,by=list(mydataanalyt$OppO.Division.Name,mydataanalyt$Opportunity.Status)
          ,FUN = sum
          ,na.action=NULL)

write.csv(mydataanalyt,"analyticscrm.csv")











