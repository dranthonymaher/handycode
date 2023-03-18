
#Initial Setup - some packages which are used or may be used in future


library(chron)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(ggthemes)
library(readxl)
library(grid)
library(gridBase)
library(writexl)
rm(list = ls())

#=========================================================================================================================
#Import Data

#enter your KPMG username here - make sure you have sync'd the Teams folder 
kpmgusername<-"amaher2"
# define parent directory - this is where the data sets are located
pd<-paste0("C:/Users/",kpmgusername,"/KPMG/AU - ACSQHC - Sepsis - 4 Pilot study/Data/")
#Note the above file path will change depending on where the file was saved
pd

mydata <- as.data.frame(read_excel(paste0(pd,"p240556823845_sunny.tan_33049635.xlsx")))
mysample <- as.data.frame(read_excel(paste0(pd,"Sample Dataset.xlsx"),sheet = "Sepsis - Sample 3"))
blindcoding_labels<-as.data.frame(read_excel(paste0(pd,"p240556823845_sunny.tan_33049635_icd10am.xlsx")))
myquestions <-as.data.frame(read_excel(paste0(pd,"Survey Questions.xlsx")))

# read in the lists of implicit and explicit sepsis codes
implicit_sepsis_codes <- as.data.frame(read_excel(paste0(pd,"implicitsepsiscodes.xlsx")))
explicit_sepsis_codes <- as.data.frame(read_excel(paste0(pd,"explicit.sepsis.codes.xlsx"),col_names = FALSE))
colnames(explicit_sepsis_codes)<-"Code"



# define destination dir for figures
dfigs<-"C:/Users/amaher2/KPMG/AU - ACSQHC - Sepsis - 4 Pilot study/Data/Figures/"

#=========================================================================================================================
#Initial Data Cleanse

#Data formatting
mydata$admissionDate <-as.Date(mydata$admissionDate)
mydata$separationDate <- as.Date(mydata$separationDate)
mydata$reviewType<-as.character(mydata$reviewType)

mysample$admission_date <- as.Date(mysample$admission_date)
mysample$separation_date <- as.Date(mysample$separation_date)
mydata$site <- toupper(mydata$site)
mysample$hospital <- toupper(mysample$hospital)



#Data renaming
mydata <- rename(mydata, admission_date = admissionDate)
mydata <- rename(mydata, separation_date = separationDate)
mydata <- rename(mydata, hospital = site)
mydata$hospital[which(mydata$hospital == "GREENSLOPES HOSPITAL")] <- "GREENSLOPES PRIVATE HOSPITAL"

#=========================================================================================================================
#common: Hospital, Admission Date, Separation Date,
unique(mydata$hospital)
unique(mysample$hospital)
intersect(unique(mydata$hospital),unique(mysample$hospital))
# looks good



# New section - Anthony ---------------------------------------------------
# new section
# use "mysample" as the "spine" and join on
myspine<-mysample[,c(1:7,9:12)]
colnames(myspine)
colnames(mydata)
unique(mydata$reviewType)

# join on the medical reviews
mydatasm<-merge(myspine,mydata[mydata$reviewType=="Medical review",],by.x = c("hospital","admission_date","separation_date")
                ,by.y = c("hospital","admission_date","separation_date"), all.x = TRUE)
# join on code reviews
mydatasm2<-merge(mydatasm,
                 mydata[mydata$reviewType=="Code review",c("hospital","admission_date","separation_date","cr1labels","responseid","respid")],by.x = c("hospital","admission_date","separation_date")
                ,by.y = c("hospital","admission_date","separation_date"), all.x = TRUE)

# remove the out of scope end of lifers
mydatasm2<-mydatasm2[mydatasm2$mra1_1=="No",]
mydatasm3<-mydatasm2[!is.na(mydatasm2$hospital),]
# Combine answers to qSOFA question
mydatasm3$qSOFA<-ifelse(is.na(mydatasm3$mra1_12),mydatasm3$mra1_17,mydatasm3$mra1_12)


# check blind coding review full list - which are expicit sepsis? - start by aggregating the data
explseplist<-aggregate(blindcoding_labels$ex.sep,by=list(blindcoding_labels$responseid,blindcoding_labels$respid),FUN = max)
# no re-name the columns to align with the col names in the main data
colnames(explseplist)<-c("responseid.y","respid.y","expl.sep.blindcode")

# join on explicit sepsis code list
mydatasm3<-merge(mydatasm3,
            explseplist
                 ,by = c("responseid.y","respid.y"), all.x = TRUE)

# join on implicit code list
implseplist.a<-aggregate(blindcoding_labels$`Part A`,by=list(blindcoding_labels$responseid,blindcoding_labels$respid),FUN = max)
implseplist.b<-aggregate(blindcoding_labels$`Part B`,by=list(blindcoding_labels$responseid,blindcoding_labels$respid),FUN = max)

# no re-name the columns to align with the col names in the main data
colnames(implseplist.a)<-c("responseid.y","respid.y","impl.codeA")
colnames(implseplist.b)<-c("responseid.y","respid.y","impl.codeB")
impl.code.calc<-cbind(implseplist.a,implseplist.b$impl.codeB)
colnames(impl.code.calc)<-c("responseid.y","respid.y","codeA","codeB")
impl.code.calc$impl.sep.blindcode<-impl.code.calc$codeA*impl.code.calc$codeB
impllist<-impl.code.calc[,c(1,2,5)]

# join on implicit sepsis code list
mydatasm3<-merge(mydatasm3,
                 impllist
                 ,by = c("responseid.y","respid.y"), all.x = TRUE)

str(mydatasm3$impl.sep.blindcode)
# combined sepsis
mydatasm3$blind.combo<-ifelse(mydatasm3$expl.sep.blindcode=="1" & mydatasm3$impl.sep.blindcode=="1","Expl and Impl",
                              ifelse(mydatasm3$expl.sep.blindcode=="1" & mydatasm3$impl.sep.blindcode=="0","Expl only",
                                     ifelse(mydatasm3$expl.sep.blindcode=="0" & mydatasm3$impl.sep.blindcode=="1","Impl ony",
                                            "No sepsis codes"
                              )))
mydatasm3$original.combo<-ifelse(mydatasm3$explicit=="1" & mydatasm3$implicit=="1","Expl and Impl",
                              ifelse(mydatasm3$explicit=="1" & mydatasm3$implicit=="0","Expl only",
                                     ifelse(mydatasm3$explicit=="0" & mydatasm3$implicit=="1","Impl ony",
                                            "No sepsis codes"
                                     )))
table(mydatasm3$original.combo)
write.csv(mydatasm3, "checkyimplexpl.csv")


# build the demographic table
# define age groups
table(mydatasm3$ageyears)
mydatasm3$agegroup<-ifelse(mydatasm3$ageyears<40,"18-39",
                           ifelse(mydatasm3$ageyears<60,"40-59",
                                  ifelse(mydatasm3$ageyears<80,"60-79","80+")))
table(mydatasm3$agegroup)
table(mydatasm3$sex)
table(mydatasm3$indigenous_status)

mydatasm3$LoS<-as.numeric(mydatasm3$separation_date - mydatasm3$admission_date)
base::mean(mydatasm3$LoS)
sd(mydatasm3$LoS)

table(mydatasm3$hospital)

table(mydatasm3$mra1_4)

##************************************************************************
## Data prep complete  ################################
##************************************************************************


#check a few things
table(mydatasm3$explicit,mydatasm3$implicit)
table(mydatasm3$explicit)
table(mydatasm3$expl.sep.blindcode, useNA = "always")
table(mydatasm3$implicit)
table(mydatasm3$impl.sep.blindcode)

table(mydatasm3$expl.sep.blindcode)
table(mydatasm3$implicit,mydatasm3$impl.sep.blindcode)

table(mydatasm3$implicit,mydatasm3$blind.combo,useNA = "always")
table(mydatasm3$blind.combo,useNA = "always")

table(mydatasm3$blind.combo)

table(mydatasm3$mra1_23,mydatasm3$blind.combo)



# check the answer to the question "was this sepsis"
table(mydatasm3$mra1_23) #140,19,51
table(mydatasm3$mra1_23[mydatasm3$explicit==1])
table(mydatasm3$mra1_23[mydatasm3$explicit==0])
table(mydatasm3$mra1_23[mydatasm3$implicit==1])
table(mydatasm3$mra1_23[mydatasm3$implicit==0])

##**********************************************************************
# 15/3/22 - make reviewer opinion the anchor

# reviewer v blind coder:
table(mydatasm3$mra1_23,mydatasm3$blind.combo)


# reviewer v "sepsis" in medical record
table(mydatasm3$mra1_23,mydatasm3$mra1_2)

#patients that were documented as sepsis, given they met qSOFA
table(mydatasm3$qSOFA,mydatasm3$mra1_2,mydatasm3$mra1_4)

# "sepsis" in medical record, explicit codes
table(mydatasm3$mra1_2,mydatasm3$expl.sep.blindcode, useNA = "always")


table(mydatasm3$mra1_2,mydatasm3$expl.sep.blindcode, mydatasm3$qSOFA)

# 7/3/22 - compare sepsis in clinical notes v qSOFA v Clin reviewer
table(mydatasm3$mra1_2,mydatasm3$qSOFA)
table(mydatasm3$mra1_2,mydatasm3$qSOFA,mydatasm3$mra1_23)

# THOUGHT: 
# do a barplot of factors that influence whetehr teh clincial reviewer thought it was sepsis


# next section
barplot(table(mydatasm3$mra1_23,mydatasm3$explicit),beside = TRUE)

barplot(table(mydatasm3$mra1_23,mydatasm3$explicit),beside = TRUE)



table(mydatasm3$qSOFA)
table(mydatasm3$qSOFA[mydatasm3$explicit==1])
table(mydatasm3$qSOFA[mydatasm3$implicit==1])
result<-table(mydatasm3$qSOFA[mydatasm3$implicit==1 & mydatasm3$explicit==0])

myvar<-"mra1_23"
result<-table(mydatasm3[myvar])

commpallet<-c("#0a4a63","#00a8dc","#1278a2")

dev.off()
par(mar = c(2, 3, 0, 0))
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            
)
grid(NA,NULL, lwd=0.5,lty = 1, col = "gray") 
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            ,col = commpallet
             ,xaxt = "n" , yaxt = "n"
            ,add=TRUE
            )
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue')
text(b1,-0.2,names(result),cex =1,srt = 0,pos=1,col='black', xpd=NA)
dev.copy(png,paste0(dfigs,myvar,".png"),width = 1.1*450, height = 0.9*350);dev.off()



# cross over of implicit and explicit
(table(mydatasm3$explicit,mydatasm3$implicit))

# combine the explicit and implicit with clinical judgement
result<-table(mydatasm3$mra1_23,mydatasm3$explicit)
b1<-barplot(result,beside = TRUE,col = commpallet)
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue', xpd=NA)
text(b1,-0.2,rownames(result),cex =1,srt = 0,pos=1,col='black', xpd=NA)
dev.copy(png,paste0(dfigs,"explicitvclinjudge",".png"));dev.off()


# how did the combo of clinical judegemtn v qSOFA vary by reviewer or by hospital
#first need to com,bine answers to 17 adn 12 (already done above)
result<-table(mydatasm3$qSOFA,mydatasm3$mra1_23)
# barplot(table(mydatasm3$mra1_23,mydatasm3$qSOFA),beside = TRUE,col = commpallet)
dev.off()
b1<-barplot(result,beside = TRUE,col = commpallet)
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue', xpd=NA)
text(b1,-0.2,c("unmet qSOFA","met qSOFA"),cex =0.75,srt = 0,pos=1,col='black', xpd=NA)
dev.copy(png,paste0(dfigs,"qSOFAvclinjudge",".png"));dev.off()

# what does the cut of clinical judegemtnt v qSOFA look like by hospital?
table(mydatasm3$qSOFA,mydatasm3$mra1_23,mydatasm3$hospital)
# nothing too interesting there
dev.off()
u1<-unique(mydatasm3$hospital)
par(mfrow=c(2,3))
i<-1
u1<-unique(mydatasm3$hospital)
for (i in 1:length(u1)){
barplot(table(mydatasm3$qSOFA[mydatasm3$hospital==u1[i]],mydatasm3$mra1_23[mydatasm3$hospital==u1[i]]),beside = TRUE,col = commpallet,main = u1[i])
  # legend(3,20,c("qSOFA unmet","qSOFA met"))
}





# what does the cut of clinical judgement v qSOFA look like by reviewer?
table(mydatasm3$qSOFA,mydatasm3$mra1_23,mydatasm3$reviewer)
# nothing too interesting there
dev.off()
u1<-unique(mydatasm3$reviewer)
par(mfrow=c(2,4))
i<-1
for (i in 1:length(u1)){
  barplot(table(mydatasm3$qSOFA[mydatasm3$reviewer==u1[i]]
                ,mydatasm3$mra1_23[mydatasm3$reviewer==u1[i]]),beside = TRUE,col = commpallet,main = u1[i])
  # legend(3,20,c("qSOFA unmet","qSOFA met"))
}
dev.off()

# align questions
# cn<-colnames(mydatasm3)
# myquestions
# colonspots<-unlist(gregexpr(":",as.data.frame(myquestions)[,]))
# qshort<-trimws(substr(as.data.frame(myquestions)[20,1],1,colonspots[20]))
# 
# dfqs<-cbind(myquestions,colonspots)

# construct lactate qSOFA for adults this wass "mra4_7"
#check it
hist(mydatasm3$mra4_7,30)
table(mydatasm3$mra4_7,useNA = "always")
sum(table(mydatasm3$mra4_7))
#identify ones over 2 mM
mydatasm3$highlac<-ifelse(mydatasm3$mra4_7>=2,1,0)
table(mydatasm3$highlac, useNA = "always")
prop.table(table(mydatasm3$highlac))
#mydatasm3$lqSOFA<-ifelse(mydatasm3$mra4_7>=2 & mydatasm3$qSOFA=="Yes" ,1,0)
mydatasm3$lqSOFA2<-ifelse(is.na(mydatasm3$highlac)==TRUE,NA,ifelse(mydatasm3$mra4_7>=2 & mydatasm3$qSOFA=="Yes" ,"Yes","No"))
table(mydatasm3$lqSOFA2)
# fix this up!!!


table(mydatasm3$qSOFA,mydatasm3$highlac, useNA = "always")
table(mydatasm3$lqSOFA);prop.table(table(mydatasm3$lqSOFA));
table(mydatasm3$lqSOFA,mydatasm3$qSOFA)

# lqsofa, given considered as sepsis
table(mydatasm3$lqSOFA2,mydatasm3$mra1_23)
table(mydatasm3$lqSOFA)


# lqsofa, given sepsis in the clinical notes
table(mydatasm3$lqSOFA,mydatasm3$mra1_2, useNA = "always")
prop.table(table(mydatasm3$lqSOFA,mydatasm3$mra1_2))


#identify ones over 4 mM
mydatasm3$highlac4<-ifelse(mydatasm3$mra4_7>=4,1,0)
table(mydatasm3$highlac4, useNA = "always")
prop.table(table(mydatasm3$highlac4))


# POST REVIEW: "sepsis" in clincial notes v coding
table(mydatasm3$mra1_2,mydatasm3$blind.combo)

table(mydatasm3$original.combo,mydatasm3$blind.combo)



#QUESTION 2 ------------------------------------------------------------------------------------------------------
#REVIEW Q2.1 SENIOR CLINICIAN-------------------------------------------------------------------------------------
#mra3_2 is escalated to senior reviewer 

# 7/3/22 - roll up "n/a" with "yes"******************************
q222<-unique(mydatasm3$mra3_2)

mydatasm3$escalation<-ifelse(mydatasm3$mra3_2==q222[1],"Not escalated","Escalated or already in care of senior clinician")
table(mydatasm3$escalation)

cn<-colnames(mydatasm3)
myquestions<-as.data.frame(myquestions)
myquestions
result<-table(mydatasm3$escalation)
prop.table(result)
dev.off()
par(mar = c(2, 3, 0.5, 0.5))
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            
)
grid(NA,NULL, lwd=0.5,lty = 1, col = "gray") 
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            ,col = commpallet
            ,xaxt = "n" , yaxt = "n"
            ,add=TRUE
)
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue')
text(b1,-0.2,names(result),cex =0.85,srt = 0,pos=1,col='black', xpd=NA)
dev.copy(png,paste0(dfigs,"senesc",".png"),width = 450, height = 350);dev.off()

# cases that were escalated, given clin reviewer judgement
result<-table(mydatasm3$escalation,mydatasm3$mra1_23)
result
prop.table(result)

# cases that were escalated, given qSOFA met
result<-table(mydatasm3$escalation,mydatasm3$qSOFA)
result
prop.table(result)

# cases that were escalated, given "sepsis" in clin notes
result<-table(mydatasm3$escalation,mydatasm3$mra1_2)
table(mydatasm3$escalation,useNA = "always")
result
prop.table(result)


# measure average lactate levels by escalation status
aggregate(mydatasm3$mra4_7,by=list(mydatasm3$escalation),FUN=mean, na.rm=TRUE, na.action=NULL)

aggregate(mydatasm3$mra4_7,by=list(mydatasm3$escalation,mydatasm3$mra1_23)
          ,FUN=mean, na.rm=TRUE, na.action=NULL)

# cases that were escalated, by high lactate status
table(mydatasm3$escalation,mydatasm3$highlac)
table(mydatasm3$escalation,mydatasm3$highlac4)


#export the data
write.csv(mydatasm3,"checky22")


######
# convert date and time cols to datetimes for "sepsis' questionm
unique(mydatasm3$mra1_3_c)
#convert "c"s to times
junk<-ifelse(mydatasm3$mra1_3_c=="Evening 6pm-12am","21:00"
             ,ifelse(mydatasm3$mra1_3_c=="Morning 6am-12pm","09:00"
             ,ifelse(mydatasm3$mra1_3_c=="Afternoon 12pm-6pm","15:00","03:00")))
junk2<-ifelse(is.na(mydatasm3$mra1_3_b),junk,mydatasm3$mra1_3_b)

d1<-NULL
for (i in 1:length(mydatasm3[,1])){
# print(i)
d1[i]<-(ifelse(is.na(mydatasm3$mra1_3_a[i])
               ,NA,as.POSIXct(paste(mydatasm3$mra1_3_a[i],junk2[i])
                              ,origin = "1970-01-01")) )
}
mydatasm3$word.sep.first.app<-as.POSIXct(d1,origin = "1970-01-01")


######
# convert date and time cols to datetimes for "sepsis' questionm

#convert "c"s to times
junk<-ifelse(mydatasm3$mra5_1b_c=="Evening 6pm-12am","21:00"
             ,ifelse(mydatasm3$mra5_1b_c=="Morning 6am-12pm","09:00"
                     ,ifelse(mydatasm3$mra5_1b_c=="Afternoon 12pm-6pm","15:00","03:00")))
junk2<-ifelse(is.na(mydatasm3$mra5_1b_b),junk,mydatasm3$mra5_1b_b)

d1<-NULL
for (i in 1:length(mydatasm3[,1])){
  # print(i)
  d1[i]<-(ifelse(is.na(mydatasm3$mra5_1b_a[i])
                 ,NA,as.POSIXct(paste(mydatasm3$mra5_1b_a[i],junk2[i])
                                ,origin = "1970-01-01")) )
}
mydatasm3$first.antimicrob<-as.POSIXct(d1,origin = "1970-01-01")

#cal;culate dif between sepssi appearing and antimicrobials
diff<-mydatasm3$first.antimicrob-mydatasm3$word.sep.first.app
hist(as.numeric(diff/(60*24)),50)

# Blood cultures taken (sepsis v not)

#1. Did they have blood cultures taken?
table(mydatasm3$mra4_1, useNA = "always")

#2. Blood cultures by "sepsis" and by qSOFA and by clin judge
# "sepsis" in med rec
table(mydatasm3$mra4_1, mydatasm3$mra1_2, useNA = "always")
# qSOFA
table(mydatasm3$mra4_1, mydatasm3$qSOFA, useNA = "always")
# clin judge
table(mydatasm3$mra4_1, mydatasm3$mra1_23, useNA = "always")

table(mydatasm3$mra1_23, useNA = "always")
table(mydatasm3$mra1_23,mydatasm3$mra4_1, useNA = "always")

# do the same for lactate
table(mydatasm3$mra4_5, useNA = "always")
table(mydatasm3$mra4_5, mydatasm3$mra4_1,useNA = "always")
table(mydatasm3$mra4_5, mydatasm3$mra1_2, useNA = "always")
# qSOFA
table(mydatasm3$mra4_5, mydatasm3$qSOFA, useNA = "always")
# clin judge
table(mydatasm3$mra4_5, mydatasm3$mra1_23, useNA = "always")

table(mydatasm3$mra1_23, useNA = "always")
table(mydatasm3$mra1_23,mydatasm3$mra4_5, useNA = "always")
table(mydatasm3$mra1_23,mydatasm3$highlac, useNA = "always")

plot(density(mydatasm3$mra4_7[mydatasm3$mra1_2=="Yes"], na.rm = TRUE),col='blue')
lines(density(mydatasm3$mra4_7[mydatasm3$mra1_2=="No"], na.rm = TRUE),col='red')

#
#





























#=========================================================================================================================
#Match Data to Sample - this is consolidating the Data from the sample and questionnaire
mydata_all <- full_join(mysample, mydata)
mydata_all <- relocate(mydata_all, c(userid, reviewer, reviewType, medicalReviewSubType, mrn, verification_mrn, verification_adm, verification_sep), .after = implicit)
mydata_all <- relocate(mydata_all, c(responseid, respid, interview_start, interview_end, status))

#=========================================================================================================================
#Checks on combined dataset



hospitals <- unique(mydata_all$hospital) #vectors of all the hospitals
hospitals

nrow(filter(mydata_all, hospital == hospitals[1])) - nrow(filter(mysample, hospital == hospitals[1]))

nrow(filter(mydata_all, hospital == hospitals[2])) - nrow(filter(mysample, hospital == hospitals[2]))



nrow(filter(mydata_all, hospital == hospitals[3])) - nrow(filter(mysample, hospital == hospitals[3]))



nrow(filter(mydata_all, hospital == hospitals[4])) - nrow(filter(mysample, hospital == hospitals[4]))



nrow(filter(mydata_all, hospital == hospitals[5])) - nrow(filter(mysample, hospital == hospitals[5]))



nrow(filter(mydata_all, hospital == hospitals[6]))


#We expected each patient to have a medical review and a code review (i.e. 2 rows of responses).
#This means that each patient from the sample should have 2 rows in total.


#=========================================================================================================================

attach(mydata_all)

analysis<- data.frame(hospital,reviewType,medicalReviewSubType,status)
analysis2<- data.frame(hospital,reviewType,status,mra1_23)
analysis2

analysis3<- data.frame(hospital,reviewType,status,explicit,implicit,mra1_23)
analysis3

hospital_count<-table(mydata_all$hospital)

hospital_count

par(mar=c(5,4,4,4))
barplot_count<-barplot(hospital_count, xlab="Hospital",ylab="Total Number of Responses", 
        main="Number of Responses", names.arg=c("Ambulance Vic","Greenslopes","Hedland","Manning","NW Regiol","Sunshine"), las=2, cex.names=0.7,
        space=c(0.5,0.5,0.5,0.5,0.5),col=c("dodgerblue", "dodgerblue1","dodgerblue2"," dodgerblue3" ,"dodgerblue4" ,"deepskyblue"),ylim=c(0,160))
text(barplot_count, hospital_count, round(hospital_count, 1),pos=3, col = 'blue')


#=========================================================================================================================
review<- as.matrix(table(mydata_all$reviewType,mydata_all$hospital,useNA="always"))
review<- review[,-7]
review
b1<-barplot(review, bes = F, main="Distribution of Review Type", Xlab = "Hospital", ylab="Number of Reviews", col = c("lightblue", "lightpink", "lightgreen"),
        names.arg=c("Ambulance Vic","Greenslopes","Hedland","Manning","NW Regiol","Sunshine"), las=2, cex.names=0.7)
legend(x = "top", legend = c("Code Review","Medical  Review","No Data"), fill = c("lightblue", "lightpink", "lightgreen"), bty = "n", y.intersp = 2)

table(mydata_all$hospital, mydata_all$reviewType,useNA="always")

#=========================================================================================================================

#filter from mydata for medical reviews only 

mydata_medical <- mydata %>% filter (mydata$reviewType =="Medical review" ) 
mydata_medical
# check for dups by hospital, adm date and sep date
mdup<-mydata_medical[duplicated(cbind(mydata_medical$hospital,mydata_medical$admission_date,mydata_medical$separation_date)),]
# there are thee dups but all for AV
# should really just pull out AV and make its own dataset because there is no sample for it (AM)


mydata_all_medical <- full_join(mysample, mydata_medical)
# the only reason this join works is because there is nothing for AV to join on to!

mydata_all_medical <- relocate(mydata_all_medical, c(userid, reviewer, reviewType, medicalReviewSubType, mrn, verification_mrn, verification_adm, verification_sep), .after = implicit)
mydata_all_medical <- relocate(mydata_all_medical, c(responseid, respid, interview_start, interview_end, status))

table(mydata_all_medical$reviewType,mydata_all_medical$hospital)

review_medical_prop<- as.matrix(prop.table(table(mydata_all_medical$reviewType,mydata_all_medical$hospital,useNA="always"),2))
review_medical_prop<- review_medical_prop[,-7]
review_medical_prop
b2<-barplot(review_medical_prop, bes = F, main="Distribution of Medical Review", Xlab = "Hospital", ylab="Percentage %", col = c("lightblue", "lightpink"),
            names.arg=c("Ambulance Vic","Greenslopes","Hedland","Manning","NW Regiol","Sunshine"), las=2, cex.names=0.7, xlim=c(0,10))
legend(x = "right", legend = c("Medical Review", "No Data"), fill = c("lightblue", "lightpink"), bty = "y", y.intersp = 2)

review_medical_count<- as.matrix(table(mydata_all_medical$reviewType,mydata_all_medical$hospital,useNA="always"))
review_medical_count<- review_medical_count[,-7]
review_medical_count

b4<-barplot(review_medical_count, bes = F, main="Distribution of Medical Review", Xlab = "Hospital", ylab="Number", col = c("lightblue", "lightpink"),
            names.arg=c("Ambulance Vic","Greenslopes","Hedland","Manning","NW Regiol","Sunshine"), las=2, cex.names=0.7, xlim=c(0,10))
legend(x = "right", legend = c("Medical Review", "No Data"), fill = c("lightblue", "lightpink"), bty = "y", y.intersp = 2)


#filter from mydata for code review only

mydata_code <- mydata %>% filter (mydata$reviewType =="Code review" ) 
mydata_code

mydata_all_code <- full_join(mysample, mydata_code)
mydata_all_code <- relocate(mydata_all_code, c(userid, reviewer, reviewType, medicalReviewSubType, mrn, verification_mrn, verification_adm, verification_sep), .after = implicit)
mydata_all_code <- relocate(mydata_all_code, c(responseid, respid, interview_start, interview_end, status))

table(mydata_all_code$reviewType,mydata_all_code$hospital)


review_code_prop<- prop.table(table(mydata_all_code$reviewType,mydata_all_code$hospital,useNA="always"),2)
review_code_prop<- review_code_prop[,-6]

b3<-barplot(review_code_prop, bes = F, main="Distribution of Code Review", ylab= "Percentage %",
            col = c("lightgreen", "lightpink"), names.arg=c("Greenslopes","Hedland","Manning","NW Regiol","Sunshine"),
            las=2, cex.names=0.7,xlim=c(0,9))
legend(x = "right", legend = c("Code Review", "No Data"), fill = c("lightgreen", "lightpink"), bty = "y", y.intersp = 2)

            
review_code_count<- table(mydata_all_code$reviewType,mydata_all_code$hospital,useNA="always")
review_code_count<- review_code_count[,-6]
review_code_count

b5<-barplot(review_code_count, bes = F, main="Distribution of Code Review", ylab= "Number",
            col = c("lightgreen", "lightpink"), names.arg=c("Greenslopes","Hedland","Manning","NW Regiol","Sunshine"),
            las=2, cex.names=0.7,xlim=c(0,9),ylim=c(0,80))
legend(x = "right", legend = c("Code Review", "No Data"), fill = c("lightgreen", "lightpink"), bty = "y", y.intersp = 2)




clinical_sepsis_count<- table(mydata_all_medical$mra1_23,mydata_all_medical$hospital,useNA="always")
clinical_sepsis_count<- clinical_sepsis_count[,-c(1,7)]
clinical_sepsis_count

b6<-barplot(clinical_sepsis_count, bes = F, main="Distribution of Clinical Sepsis Judgement", ylab= "Number",
            col = c("pink","orange","lightblue","lightgreen"), names.arg=c( "Greenslopes","Hedland","Manning","NW Regiol","Sunshine"),
            las=2, cex.names=0.7,xlim=c(0,9))

legend(x = "right", legend = c("No","Unable to be Determined","Yes","No Data"), fill = c("pink","orange","lightblue","lightgreen"), bty = "y",)


clinical_sepsis_prop<- prop.table(table(mydata_all_medical$mra1_23,mydata_all_medical$hospital,useNA="always"),2)
clinical_sepsis_prop<- clinical_sepsis_prop[,-c(1,7)]
clinical_sepsis_prop

b7<-barplot(clinical_sepsis_prop, bes = F, main="Distribution of Clinical Sepsis Judgement", ylab= "Percentage %",
            col = c("pink","orange","lightblue","lightgreen"), names.arg=c( "Greenslopes","Hedland","Manning","NW Regiol","Sunshine"),
            las=2, cex.names=0.7,xlim=c(0,9))

legend(x = "right", legend = c("No","Unable to be Determined","Yes","No Data"), fill = c("pink","orange","lightblue","lightgreen"), bty = "y")


explicit_sepsis_count<- table(mydata_all$explicit,mydata_all$hospital,useNA="always")
explicit_sepsis_count<-explicit_sepsis_count[,-c(1,7)]
explicit_sepsis_count

b8<-barplot(explicit_sepsis_count, bes = F,main="Distribution of Explicit Sepsis", ylab= "Number",
            col = c("mediumorchid1","mediumseagreen","mediumslateblue"), names.arg=c("Greenslopes","Hedland","Manning","NW Regiol","Sunshine"),
            las=2, cex.names=0.7,xlim=c(0,9))
legend(x = "right", legend = c("Implicit Sepsis", "Explicit Sepsis","NA"), fill = c("mediumorchid1","mediumseagreen","mediumslateblue"), bty = "y")


explicit_sepsis_prop<- prop.table(table(mydata_all$explicit,mydata_all$hospital,useNA="always"),2)
explicit_sepsis_prop<-explicit_sepsis_prop[,-c(1,7)]
explicit_sepsis_prop

b9<-barplot(explicit_sepsis_prop, bes = F,main="Distribution of Explicit Sepsis", ylab= "Percent %",
            col = c("mediumorchid1","mediumseagreen","mediumslateblue"), names.arg=c("Greenslopes","Hedland","Manning","NW Regiol","Sunshine"),
            las=2, cex.names=0.7,xlim=c(0,9))
legend(x = "right", legend = c("Implicit Sepsis", "Explicit Sepsis","NA"), fill = c("mediumorchid1","mediumseagreen","mediumslateblue"), bty = "y")



analysis4<- data.frame(mydata_all_medical$hospital,mydata_all_medical$reviewType,mydata_all_medical$explicit,mydata_all_medical$implicit,mydata_all_medical$mra1_23)

#=========================================================================================================================
# creating graphs for analysis 

a<-table(analysis4$mydata_all_medical.mra1_23[analysis4$mydata_all_medical.explicit==1])
b10<-barplot(table(analysis4$mydata_all_medical.mra1_23[analysis4$mydata_all_medical.explicit==1]),main="Clinical Judgment as Sepsis, Given Coded as Explicit Sepsis",
        col = c("slategray1","slateblue2","slateblue4"))
text(b10, a, round(a, 1),pos=3, col = 'blue')

b<-table(analysis4$mydata_all_medical.mra1_23[analysis4$mydata_all_medical.implicit==1])
b11<-barplot(table(analysis4$mydata_all_medical.mra1_23[analysis4$mydata_all_medical.implicit==1]),main="Clinical Judgment as Sepsis, Given Coded as Implicit Sepsis",
             col = c("orchid1","orchid2","orchid4"))
text(b11, b, round(b, 1),pos=3, col = 'blue')


c<-prop.table(table(analysis4$mydata_all_medical.explicit[analysis4$mydata_all_medical.mra1_23== "Yes"]))*100
b12<-barplot(c,main="Coded as Explicit sepsis,Given clinical judgment as sepsis",names.arg=c("No","Yes"),
        col = c("slategray1","slateblue2"),ylab="Percentage %")
text(b12, c, labels = round(c, digits = 3),pos=3)

d<-prop.table(table(analysis4$mydata_all_medical.implicit[analysis4$mydata_all_medical.mra1_23== "Yes"]))*100
d
b13<-barplot(d,main="Coded as Implicit sepsis, Given clinical judgment as sepsis, ",names.arg=c("No","Yes"),
             col = c("slategray1","slateblue2"),ylab="Percentage %")
text(b13, d, labels = round(d, digits = 3),pos=3)

#=========================================================================================================================
# creating graphs for every survey question in the dataset 

questions<-t(myquestions%>% slice(5:383)) #creating vector of all the survey questions used for the title of the graphs later

mydata_all_filtered<-mydata_all[mydata_all$mra1_1 != "Yes", ] #filtering out end of life patients into new dataset 

#selecting manually which columns to create a graph out of 

v1<-c(128,132,133,137,138,140,142:145,150,151,152,156,158,159,163,164,171,172)
v2<-c(174:194,200,205:216,222,227:230,235,240,241,243,245:248,253,259:264,268,
      270,276,277,284,286,287,294,,307,315:336,339,361,365,377,382,385,387:390,
      395,401,403:405,410,413:415,419,420,424,426,432,437:439,443,448,455,457,
      459:479,490,491,493:502,504)
  
#im using the filtered data up until mra1_23 because after that, there should be no responses for those who are end of life 
#using the filtered dataset breaks the code after column 270 IDK why so i split it up and it works

colnames(mydata_all)[1] 

for (i in 1:length(v1)){
  png(filename=paste0(as.character(colnames(mydata_all_filtered)[v1[i]]),".png"))
  par(mar=c(4,4,4,4))
  x<-barplot(table(mydata_all_filtered[,v1[i]]),
          main= as.character(questions[v1[i]-125]),cex.main=0.7,
          col=c("#00a8dc","#1178a2","#005470","skyblue3",
                "skyblue4","slateblue","slateblue1","slateblue2",
                "slateblue3","slateblue","slategray","slategray1",
                "slategray2", "slategray3", "slategray4", "slategrey"),
          ylab="Number", ylim=c(0,1.1*max(table(mydata_all_filtered[,v1[i]]))),
          las=0, cex.names=0.7)
  dev.off()
}

for (i in 1:length(v2)){
  png(filename=paste0(as.character(colnames(mydata_all)[v2[i]]),".png"))
  par(mar=c(4,4,4,4))
  x<-barplot(table(mydata_all[,v2[i]]),
             main= as.character(questions[v2[i]-125]),cex.main=0.7,
             col=c("#00a8dc","#1178a2","#005470","skyblue3",
                   "skyblue4","slateblue","slateblue1","slateblue2",
                   "slateblue3","slateblue","slategray","slategray1",
                   "slategray2", "slategray3", "slategray4", "slategrey"),
             ylab="Number", ylim=c(0,1.2*max(table(mydata_all[,v2[i]]))),
             las=0, cex.names=0.7)
  dev.off()
}


# some graphs need to be made individually because of the axes sizes and names, so i just did them here 

e<-table(mydata_all$mra1_1)

b14<-barplot(table(mydata_all$mra1_1),
        main= as.character(questions[1]),cex.main=0.7,
        col=c("#00a8dc","#1178a2"),
        ylab="Number",ylim=c(0,250))
text(b14, e, labels = round(e, digits = 3),pos=3)

table(mydata_all$mrh1_7)

barplot(table(mydata_all$mrh1_5),las=2,cex.names = 0.7, ylim=c(0,1.2*max(table(mydata_all$mrh1_5))),
        ylab="Number", names.arg = ("Chest","Other","No","Pneumonia","Urinary Tract","Throat","")
        col=c("#00a8dc"))

#counting all the risk factors
risk.factors.adult<-mydata_all[,c(174:186,190:194)]

length(risk.factors.adult)
v2<-c(1:18)
length(v2)
as.vector(table(risk.factors.adult[1]))
table(risk.factors.adult[1])

tb1<-matrix(NA,2,18)

for (i in 1:length(v2)){
  tb1[,i]<-as.matrix(table(risk.factors.adult[i]))
}

#counting where the care was escalated
v3<-c(205:216)

length(v3)

tb2<-matrix(NA,2,12)

for (i in 1:length(v3)){
  tb2[,i]<-as.matrix(table(mydata_all[,v3[i]]))
}


#=========================================================================================================================
# time difference analyses 

v6<-c(1:nrow(mydata_all))
anti<-data.frame(mydata_all_filtered$mra1_3_a,mydata_all_filtered$mra1_3_b,mydata_all_filtered$mra1_3_c,
                 mydata_all_filtered$mra5_1b_a,mydata_all_filtered$mra5_1b_b,mydata_all_filtered$mra5_1b_c)

anti$mydata_all.mra1_3_sum <- paste(anti$mydata_all.mra1_3_a,anti$mydata_all.mra1_3_b,anti$mydata_all.mra1_3_c)
anti$mydata_all.mra5_1b_sum <- paste(anti$mydata_all.mra5_1b_a,anti$mydata_all.mra5_1b_b,anti$mydata_all.mra5_1b_c)


time_obj_1 <- as.data.frame(strptime(anti$mydata_all.mra1_3_sum, format = "%Y-%m-%d %H:%M"))
time_obj_1
time_obj_2 <- as.data.frame(strptime(anti$mydata_all.mra5_1b_sum, format = "%Y-%m-%d %H:%M"))
time_obj_2

comparison_time<-as.data.frame(c(time_obj_1,time_obj_2))
colnames(comparison_time) <- c("Sepsis Diagnosis", "First Dose of Antimicrobial")
comparison_time$diff.time <- difftime(comparison_time$`First Dose of Antimicrobial`, comparison_time$`Sepsis Diagnosis`,units="mins")
comparison_time$diff.time<-as.integer(comparison_time$diff.time)
comparison_time$discussion <-matrix(NA,nrow(anti),1)

#
hist(comparison_time$diff.time,col="slateblue", breaks=20, xlab = "Time difference in Minutes",
     main="Difference in time between antimicrobial Admission and first record of sepsis")


anti1<-data.frame(mydata_all$mra1_5_a,mydata_all$mra1_5_b,mydata_all$mra1_5_c,
                 mydata_all$mra5_1b_a,mydata_all$mra5_1b_b,mydata_all$mra5_1b_c)

anti1$mydata_all.mra1_5_sum <- paste(anti1$mydata_all.mra1_5_a,anti1$mydata_all.mra1_5_b,anti1$mydata_all.mra1_5_c)
anti1$mydata_all.mra5_1b_sum <- paste(anti1$mydata_all.mra5_1b_a,anti1$mydata_all.mra5_1b_b,anti1$mydata_all.mra5_1b_c)


time_obj_3 <- as.data.frame(strptime(anti1$mydata_all.mra1_5_sum, format = "%Y-%m-%d %H:%M"))
time_obj_3
time_obj_4 <- as.data.frame(strptime(anti1$mydata_all.mra5_1b_sum, format = "%Y-%m-%d %H:%M"))
time_obj_4

comparison_time1<-as.data.frame(c(time_obj_3,time_obj_4))
colnames(comparison_time1) <- c("Triage Time", "First Dose of Antimicrobial")
comparison_time1$diff.time <- difftime(comparison_time1$`First Dose of Antimicrobial`, comparison_time1$`Triage Time`,units="mins")
comparison_time1$diff.time<-as.integer(comparison_time1$diff.time)
comparison_time1$discussion <-matrix(NA,nrow(anti1),1)

hist(comparison_time1$diff.time,col="#005470", xlab = "Time difference in Minutes",breaks=20,
     main="Difference in time between antimicrobial Admission and triage time")

for(i in 1:length(v6)){
if (is.na(comparison_time1$diff.time[i])== TRUE){comparison_time1$discussion[i] <-""}
  else if (comparison_time1$diff.time[i] <= 0){comparison_time1$discussion[i] <-"NO"}
  else if (comparison_time1$diff.time[i] <= 60){comparison_time1$discussion[i] <-"YES"}
  else {comparison_time1$discussion[i]<-"NO"}
}


barplot(prop.table(table(comparison_time1$discussion[comparison_time1$discussion != ""])),
        main="Was antimicrobial was administered within 60 mins of suspected sepsis?",
        ylim=c(0,1),col=c("#00a8dc","#1178a2","#005470"))
#=========================================================================================================================
# creating bar-charts for time interval columns 

v6<-c(1:nrow(mydata_all))
nrow(mydata_all)

Q4<-data.frame(mydata_all$mrh3_2_a,mydata_all$mrh3_2_b,mydata_all$mrh3_2_c)
colnames(Q4)<-c("DATE","TIME","TIME INTERVAL")
Q4$DATE<-as.data.frame(strptime(Q4$DATE, format = "%Y-%m-%d"))
Q4$TIME<-as.data.frame(strptime(Q4$TIME, format = "%H:%M"))
Q4$DATE<-format(Q4$DATE,"%m")
Q4$TIME<-format(Q4$TIME,"%H:%M")
Q4$`TIME INTERVAL`[is.na(Q4$`TIME INTERVAL`)] <- ""
Q4$NEW<-c(1:nrow(mydata_all))


for(i in 1:length(v6)){
  if (is.na(Q4$TIME[i,])== TRUE){Q4$NEW[i] <-""}
  else if (Q4$TIME[i,] < "12:00" & Q4$TIME[i,]> "06:00"){Q4$NEW[i] <-"Morning 6am-12pm"}
  else if (Q4$TIME[i,] < "18:00" & Q4$TIME[i,]> "12:00"){Q4$NEW[i] <-"Afternoon 12pm-6pm"}
  else if (Q4$TIME[i,]< "24:00" & Q4$TIME[i,]> "18:00"){Q4$NEW[i] <-"Evening 6pm-12am"}
  else {Q4$NEW[i] <-"Overnight 12am-6am"}
}

Q4$Combined <- paste(Q4$`TIME INTERVAL`,Q4$NEW,sep="")
table(Q4$Combined[Q4$Combined!=""])

barplot(table(Q4$Combined[Q4$Combined!=""]),las=0,ylim=c(0,1.1*max(table(Q4$Combined[Q4$Combined!=""]))),cex.names=0.7,
        col=c("#00a8dc"),ylab = "Number")

table(mydata_all$mra4_1)
barplot(table(Q4$DATE[Q4$DATE!=""]),  col=c("#00a8dc"),ylim=c(0,1.1*max(table(Q4$DATE[Q4$DATE!=""]))),
        names.arg = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"),ylab="Number")


#REVIEW QUESTION 1.1 "WHAT PROP MET THE STUDY CRITERIA FOR SEPSIS"- if the patient met qSOFA criteria
table(mydata_all_filtered$mra1_17)

qSOFA.ED.prop<-as.matrix(prop.table(table(mydata_all_filtered$mra1_12)))#emergency department
qSOFA.inpatient.prop<-as.matrix(prop.table(table(mydata_all_filtered$mra1_17))) #inpatient
qSOFA.prop<-data.frame(qSOFA.ED.prop,qSOFA.inpatient.prop)
b14<-barplot(as.matrix(qSOFA.prop),bes=T,main="Did the patient meet two of the three qSOFA criteria? ",names.arg=c("Emergency Department","Inpatient"),
             col = c("#00a8dc","#1178a2"),ylab="Proportion",legend=T,ylim=c(0,1))

qSOFA.ED.num<-as.matrix(table(mydata_all_filtered$mra1_12))#emergency department
qSOFA.inpatient.num<-as.matrix(table(mydata_all_filtered$mra1_17)) #inpatient
qSOFA.num<-data.frame(qSOFA.ED.num,qSOFA.inpatient.num)
qSOFA.2way<-qSOFA.num%>% mutate(total = rowSums(across(where(is.numeric))))
qSOFA.2way

#REVIEW QUESTION 1.2 What proportion of cases in the sample that met the study's criteria for sepsis were coded with an ICD-10-AM code for sepsis? 
table(mydata_all_filtered$explicit[mydata_all_filtered$mra1_12 == "Yes"|mydata_all_filtered$mra1_17 == "Yes"])

barplot(prop.table(table(mydata_all_filtered$explicit[mydata_all_filtered$mra1_12 == "Yes"|mydata_all_filtered$mra1_17 == "Yes"])),main="Coded with Explicit Sepsis, Given met criteria?",
        col = c("#00a8dc","#1178a2"),ylab="Proportion",ylim=c(0,1),names.arg = c("No","Yes"))

#REVIEW QUESTION 1.2 "WHAT PROP MET THE STUDY CRITERIA FOR SEPSIS ALSO IDENTIFIED BY CLINICAL TEAM"


review1.2<-as.matrix(data.frame(as.matrix(prop.table(table(mydata_all_filtered$mra1_2[mydata_all_filtered$mra1_12=="Yes"]))),
as.matrix(prop.table(table(mydata_all_filtered$mra1_2[mydata_all_filtered$mra1_17=="Yes"])))))

barplot(review1.2,bes=T,main="",names.arg=c("Emergency Department","Inpatient"),
        col = c("#00a8dc","#1178a2"),ylab="Proportion",ylim=c(0,1))
legend("topright", 
       legend = c("No","Yes"), 
       col = c("#00a8dc","#1178a2"),
       pch = c(15,15,15), 
       bty = "y", 
       pt.cex = 2, 
       cex =0.7, 
       text.col = "black", 
       horiz = F )

#1.3 What proportion of cases in the sample that were identified by the treating clinical team as having sepsis were coded with an ICD-10-AM code for sepsis?
barplot(prop.table(table(mydata_all_filtered$explicit[mydata_all_filtered$mra1_2 == "Yes"])),
        main = "Explicit Sepsis, given clinician judged as sepsis?",
        names.arg=c("No","Yes"),
        col = c("#00a8dc","#1178a2"),ylab="Proportion",ylim=c(0,1))

#REVIEW Q1.5 QUESTION CODE IMPLICIT SEPSIS


implicit.sepsis<-table(mydata_all_filtered$implicit[mydata_all_filtered$mra1_12 == "Yes"|mydata_all_filtered$mra1_17 == "Yes"])


barplot(prop.table(implicit.sepsis),bes=T,main="Was it implicit sepsis, if it met the criteria for sepsis?",names.arg=c("No","Yes"),
        col = c("#00a8dc","#1178a2"),ylab="Proportion",ylim=c(0,1))
#REVIEW Q2.1 SENIOR CLINICIAN

barplot(prop.table(table(mydata_all$mra3_2)),col = c("#00a8dc","#1178a2","#005470"),
        ylab="Proportion",ylim=c(0,1),legend=T,legend.text = c("No","Not Applicable","Yes"),main="Was patient care escalated to senior clinician?")

#REVIEW Q2.2 BLOOD CULTURES 

barplot(prop.table(table(mydata_all$mra4_1)),col = c("#00a8dc","#1178a2","#005470"),
        ylab="Proportion",ylim=c(0,1),main="Blood Cultures Taken?")

# Q2.3 SERUM LACTATE

barplot(prop.table(table(mydata_all$mra4_5)),col = c("#00a8dc","#1178a2","#005470"),
        ylab="Proportion",ylim=c(0,1),main="Serum Lactate Taken?")


hist(mydata_all$mra4_7,breaks=20,main="Distribution of Highest Serum Lactate Measured in 24hrs",col = "#1178a2",
     xlab = "Serum Lactate (mmol/L)")

#REVIEW Q2.4 IV FLUIDS for FLUID RESUSCITATION

barplot(prop.table(table(mydata_all$mra5_8)),col = c("#00a8dc","#1178a2","#005470"),
        ylab="Proportion",ylim=c(0,1),main="IV Fluids administered?")

boxplot(mydata_all$mra5_12,main="Distribution of total volume of fluid in first 24 hours since sepsis was first suspected?",col = "#01A1CB",
     ylab="IV Fluids (L)",outline = FALSE)

outliers <- boxplot(mydata_all$mra5_12, plot=FALSE)$out
sd(mydata_all$mra5_12[- which(mydata_all$mra5_12 %in% outliers)],na.rm = TRUE)

#REVIEW Q2.5 receive supplementary oxygen if desaturating?
barplot(prop.table(table(mydata_all$mra5_13)),
        main="Was patient desaturating?",
        col = c("#00a8dc","#1178a2"),ylab="Proportion",ylim=c(0,1))
barplot(prop.table(table(mydata_all$mra5_14)),
        main="Was Supplementary Oxygen provided if desaturating?",
        col = c("#00a8dc","#1178a2"),ylab="Proportion",ylim=c(0,1))

#REVIEW Q2.6 adequate antimicrobial coverage 

barplot(prop.table(table(mydata_all$mra5_4)),col = c("#00a8dc","#1178a2","#005470"),las=0,cex.names = 1,
        ylab="Proportion",ylim=c(0,1),names.arg = c("No","Unable to Assess","Yes"),
        main="Did the antibiotics provide adequate coverage for the provisional diagnosis??")

# Q2.7 antimicrobials reviewed after they were initially prescribed? 

barplot(prop.table(table(mydata_all$mra5_6)),col = c("#00a8dc","#1178a2","#005470"),
        ylab="Proportion",ylim=c(0,1),main="Evidence of plan to review antimicrobials?")
#review occurred?
barplot(prop.table(table(mydata_all$mra5_7)),col = c("#00a8dc","#1178a2","#005470"),
        ylab="Proportion",ylim=c(0,1),main="Reviewed antimicrobials?")


#Review 1.6, What proportion of cases recieved ICD - 10AM codes by a blind coding reviewer

implicit_sepsis_codes <- read_excel("implicitsepsiscodes.xlsx")
explicit_sepsis_codes <- read_excel("explicit.sepsis.codes.xlsx",col_names = FALSE)
colnames(explicit_sepsis_codes)<-"Code"


blindcoding_labels$icd10am<-sub(" -.*", "", blindcoding_labels$icd10am)   


explicit_sepsis_codes<-gsub( " .*$", "", explicit_sepsis_codes$Code)
implicit_sepsis_codes$`Implicit sepsis codes (part A)`<-gsub( " .*$", "", implicit_sepsis_codes$`Implicit sepsis codes (part A)`)
implicit_sepsis_codes$`Implicit sepsis codes (part B)`<-gsub( " .*$", "", implicit_sepsis_codes$`Implicit sepsis codes (part B)`)

blindcoding_labels$explicitsepsiscode<-c(1:nrow(blindcoding_labels))
blindcoding_labels$explicitsepsiscode<- blindcoding_labels$icd10am %in% unlist(explicit_sepsis_codes)

my_vector <- vector(mode="numeric")

for(i in 1:nrow(blindcoding_labels)){
  if (blindcoding_labels$explicitsepsiscode[i] == TRUE){
   my_vector<-append(my_vector,blindcoding_labels$responseid[i])}
}
responseid_withblindcode<-unique(my_vector)

mydata_all_code$blindcode<-c(1:nrow(mydata_all_code))


blindcoding_labels$implicitsepsiscode_partA<-c(1:nrow(blindcoding_labels))
blindcoding_labels$implicitsepsiscode_partA<- blindcoding_labels$icd10am %in% unlist(implicit_sepsis_codes$`Implicit sepsis codes (part A)`)
blindcoding_labels$implicitsepsiscode_partB<-c(1:nrow(blindcoding_labels))
blindcoding_labels$implicitsepsiscode_partB<- blindcoding_labels$icd10am %in% unlist(implicit_sepsis_codes$`Implicit sepsis codes (part B)`)


my_vector1 <- vector(mode="numeric")

for(i in 1:nrow(blindcoding_labels)){
  if (blindcoding_labels$implicitsepsiscode_partA[i] == TRUE){
    my_vector1<-append(my_vector1,blindcoding_labels$responseid[i])}
}
my_vector1

my_vector2 <- vector(mode="numeric")

for(i in 1:nrow(blindcoding_labels)){
  if (blindcoding_labels$implicitsepsiscode_partB[i] == TRUE){
    my_vector2<-append(my_vector2,blindcoding_labels$responseid[i])}
}
my_vector2

vector_implicit_responseid<-intersect(my_vector1,my_vector2)
is.na(mydata_all_code$responseid[1])

mydata_all_code$blindcode<-c(1:nrow(mydata_all_code))
for(i in 1:nrow(mydata_all_code)){
  if (is.na(mydata_all_code$responseid[i])){mydata_all_code$blindcode[i]= NA}
  else if(mydata_all_code$responseid[i] %in% responseid_withblindcode){mydata_all_code$blindcode[i]= "Explicit"}
  else if(mydata_all_code$responseid[i] %in% vector_implicit_responseid){mydata_all_code$blindcode[i]= "Implicit"}
  else{mydata_all_code$blindcode[i]= "No Sepsis Code"}}

barplot(prop.table(table(mydata_all_code$blindcode)),ylim=c(0,1),
        main="Blind Coding Review ", col=c("#00a8dc","#1178a2","#005470"))

table(mydata_all_code$blindcode)
# QSOFA comparison 

#.	How many met qSOFA given coded as explicit sepsis
comparisonq1<-table(mydata_all_filtered$mra1_12[mydata_all_filtered$explicit == 1])+table(mydata_all_filtered$mra1_17[mydata_all_filtered$explicit == 1])
barplot(prop.table(comparisonq1),ylim=c(0,1),col=c("#00a8dc","#1178a2"))
#.	How many met qSOFA given coded as implicit sepsis
compq2<-table(mydata_all_filtered$mra1_12[mydata_all_filtered$implicit == 1])+table(mydata_all_filtered$mra1_17[mydata_all_filtered$implicit == 1])
barplot(prop.table(compq2),ylim=c(0,1),col=c("#00a8dc","#1178a2"))

#.	How many met qSOFA given considered sepsis by clinical reviewer
compq3<-table(mydata_all_filtered$mra1_12[mydata_all_filtered$mra1_23 == "Yes"])+table(mydata_all_filtered$mra1_17[mydata_all_filtered$mra1_23 == "Yes"])
barplot(prop.table(compq3),ylim=c(0,1),col=c("#00a8dc","#1178a2"))

#.	How many met qSOFA given considered sepsis by clinical treating team
compq4<-table(mydata_all_filtered$mra1_12[mydata_all_filtered$mra1_2 == "Yes"])+table(mydata_all_filtered$mra1_17[mydata_all_filtered$mra1_2 == "Yes"])
barplot(prop.table(compq4),ylim=c(0,1),col=c("#00a8dc","#1178a2"))

#.	How many met qSOFA given considered 

a<-data.frame(mydata_medical$mrn,mydata_medical$mra1_12,mydata_medical$mra1_17,mydata_medical$admission_date)
colnames(a)<-c("mrn","q12","q17","admissiondate")
length(unique(a$mrn))


b<-data.frame(mydata_all_code$mrn,mydata_all_code$blindcode,mydata_all_code$admission_date)
colnames(b)<-c("mrn","blindcode","admissiondate")
b<-b[!with(b,is.na(b$mrn)),]

a$mrn[duplicated(a$mrn)]
b$mrn[duplicated(b$mrn)]

c$mrn[with(c,is.na(c$q12)& is.na(c$q17))]

c<-merge(a,b, all.x = FALSE, all.y = TRUE)

table(b$blindcode)
table(c$blindcode)
table(a$q12)+table(a$q17)

compq5 <-table(c$q12[c$blindcode == "Explicit"|c$blindcode == "Implicit"])+table(c$q17[c$blindcode == "Explicit"|c$blindcode == "Implicit"])
compq5 
barplot(prop.table(compq5),ylim=c(0,1),col=c("#00a8dc","#1178a2"))

table(c$q12)

table(mydata_all$mrp1_1)