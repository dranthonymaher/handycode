
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")

#  import the data
mydata <- NULL
mydata <- read.csv("HouseTcpByCandidateByPollingPlaceDownload-24310 (1).csv")
head(mydata)

cn<-colnames(mydata)
cn
mydata<-mydata[,c("StateAb","DivisionID" ,"DivisionNm","PollingPlaceID" , "PollingPlace" ,  "Elected", "BallotPosition" ,
                  "PartyAb","OrdinaryVotes")]
# Pivot out the data
head(mydata)

# 
fd<-reshape(mydata, 
                              idvar = c("StateAb","DivisionID" ,"DivisionNm","PollingPlaceID" , "PollingPlace"), 
                              #varying = list(names(nsw_lga_age_tot)[3:20]),
                              #v.names = "numberofpeople",
                              timevar = "PartyAb",
                              direction = "wide")
# check it
head(nsw_lga_age_tot_long)
