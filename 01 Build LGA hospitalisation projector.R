#Anthony Maher
# script to build the hospitalisation projector spreadsheet tool for NSW

dev.off()
#remove everything from the console
rm(list = ls())
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/covid19/")

# Build the hospitalisation projector -------------------------------------

#get the cleaned data from NSW LGA Age distribution from the ABS
nsw_lga_age_tot<-read.csv("https://raw.githubusercontent.com/dranthonymaher/handycode/master/LGAtotpersonsphidu.csv")

#Clean up: remove columns that haev "X" in their names
nsw_lga_age_tot<-nsw_lga_age_tot[,-grep("X", colnames(nsw_lga_age_tot))]
nsw_lga_age_tot$tots<-nsw_lga_age_tot$Total.persons
nsw_lga_age_tot<-nsw_lga_age_tot[,-grep("Total.persons", colnames(nsw_lga_age_tot))]

#change the names of these
names(nsw_lga_age_tot)<-c("LGAcode","LGAname",paste0(rep("X.",18),seq(18)),"tots")
head(nsw_lga_age_tot)

# fix the codes being used for the old LGAs
# first, combine botany bay and rockdale to Bayside
bb<-nsw_lga_age_tot[nsw_lga_age_tot$LGAcode==11100,] # Botany Bay
rock<-nsw_lga_age_tot[nsw_lga_age_tot$LGAcode==16650,] # Rockdale
nsw_lga_age_tot<-nsw_lga_age_tot[-which(nsw_lga_age_tot$LGAcode==16650),] #removes the row corresponding to rockdale
# combine these and sum up
baysidesplit<-rbind(bb,rock)
bays<-as.data.frame(t(colSums(baysidesplit[,3:21])))

#now convert the Botany Bay row into the Bays
nsw_lga_age_tot[which(nsw_lga_age_tot$LGAcode==11100),3:21]<-bays

# now change 
nsw_lga_age_tot$LGAcode[nsw_lga_age_tot$LGAcode==11100]<-10500 #Botany Bay -> Bayside
levels(nsw_lga_age_tot$LGAname)<-c(levels(nsw_lga_age_tot$LGAname), "Bayside") # need to add a level since LGAname is in there as a factor
nsw_lga_age_tot$LGAname[nsw_lga_age_tot$LGAcode==10500]<-"Bayside" #Botany Bay -> Bayside
nsw_lga_age_tot$LGAcode[nsw_lga_age_tot$LGAcode==13510]<-12160 #Update Cootamundra

head(nsw_lga_age_tot)

# Check some LGA's age distribution
dev.off()
par(mfrow=c(1,2))
mycouncil<-"Mid-Coast (A)"
barplot(as.matrix(nsw_lga_age_tot[nsw_lga_age_tot$LGAname==mycouncil,][3:20]),
        names.arg = seq(0,85,5), cex.names = 0.7)
title(mycouncil)
mycouncil<-"Blacktown (C)"
barplot(as.matrix(nsw_lga_age_tot[nsw_lga_age_tot$LGAname==mycouncil,][3:20]),
        names.arg = seq(0,85,5), cex.names = 0.7)
title(mycouncil)
# we can see Mid Coast has a much older demographic than Blacktown


# make a "long" version of this:
nsw_lga_age_tot_long<-reshape(nsw_lga_age_tot[,c(1:20)], 
                              idvar = c("LGAcode","LGAname"), 
                              varying = list(names(nsw_lga_age_tot)[3:20]),
                              v.names = "numberofpeople",
                              timevar = "Agegropup",
                              times = seq(0,85,5),
                              direction = "long")
# check it
head(nsw_lga_age_tot_long)

# the row names are a bit weird - fix them
row.names(nsw_lga_age_tot_long)<-as.character(1:length(nsw_lga_age_tot_long[,1]))

# check it
head(nsw_lga_age_tot_long)


# export for Arwin
# convert to required format
# forG<-nsw_lga_age_tot_long
# forG$layer<-"lga"
# forG$type<-"bar"
# forG$name<-"Age Distribution"
# 
# forGv2<-as.data.frame(cbind(forG$layer, forG$type, forG$name, forG$LGAcode, forG$Agegropup, forG$numberofpeople))
# head(forGv2)
# names(forGv2)<-c("layer", "type", "name", "id", "x", "y")
# write.csv(forGv2,"Nsw_LGA_age_dist_forG.csv")

# join on totals and compute proportions
nsw_lga_totpops<-nsw_lga_age_tot[,c("LGAcode","tots")]
names(nsw_lga_totpops)<-c("LGAcode","TotalPop")
nsw_lga_age_tot_long<-merge(nsw_lga_age_tot_long,nsw_lga_totpops, by = "LGAcode",all.x = TRUE)
nsw_lga_age_tot_long$proppop<-nsw_lga_age_tot_long$numberofpeople / nsw_lga_age_tot_long$TotalPop


# Age-based hospitalisation rates -----------------------------------------
# import the data table from that gives info about hosp rates by age groups. This comes from:
# https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm#T1_down
hosp<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/covid19/hospitalisationandicubyagegroup.csv")
head(hosp,30)
# plot it
dev.off()
par(mfrow=c(1,3))
barplot(hosp$PropHospitalization, names.arg = seq(0,85,5), cex.names = 1, ylim = c(0,1)); title("Prop Hospitalised")
barplot(hosp$propICUadmission, names.arg = seq(0,85,5), cex.names = 1, ylim = c(0,1)); title("Prop ICU")
barplot(hosp$propCase.fatality, names.arg = seq(0,85,5), cex.names = 1, ylim = c(0,1)); title("Prop Deaths")

# Integrate the data
nsw_lga_hosprates<-merge(nsw_lga_age_tot_long,hosp,by.x = "Agegropup", by.y = "Age.group", all.x = TRUE)
head(nsw_lga_hosprates)
# so that's our projector - ready for cases data

# Compute the hosp factor and icu factor per case
nsw_lga_hosprates$hosp.pp = nsw_lga_hosprates$proppop * nsw_lga_hosprates$PropHospitalization
nsw_lga_hosprates$icu.pp = nsw_lga_hosprates$proppop * nsw_lga_hosprates$propICUadmission

head(nsw_lga_hosprates)
agg.hicu<-aggregate(cbind(nsw_lga_hosprates$hosp.pp,nsw_lga_hosprates$icu.pp), 
                    list(nsw_lga_hosprates$LGAcode, nsw_lga_hosprates$LGAname), FUN = sum)
names(agg.hicu)<-c("LGAcode","LGAname","hospfactor","icufactor")
head(agg.hicu)

# write the data to csv
write.csv(nsw_lga_hosprates,"C:/Users/amaher2/OneDrive - KPMG/Documents/covid19/covidLGAprojector.csv")
write.csv(agg.hicu,"C:/Users/amaher2/OneDrive - KPMG/Documents/covid19/casestohospbylga.csv")

