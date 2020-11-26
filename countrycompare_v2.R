rm(list = ls())
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/covid19/")

# 

# read in and clean up the actual data ------------------------------------


# read in the daily active case data
#actualcases<-read.csv("OxCGRT_Download_270420_055929_Full.csv")
actualcases<-read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
colnames(actualcases)
head(actualcases)
keepcols<-c("CountryName","Date", "ConfirmedCases", "ConfirmedDeaths",  "StringencyIndexForDisplay")
actualcases<-actualcases[,keepcols]
head(actualcases)
actualcases[is.na(actualcases)]<-0
str(actualcases)
actualcases$Date<-as.Date(paste(substr(as.character(actualcases$Date),1,4),
                                 substr(as.character(actualcases$Date),5,6),
                                 substr(as.character(actualcases$Date),7,8),sep = "-"))
head(actualcases)

#compute active cases as those within the last 3 weeks only
# shift the dates of the actual data by 21 days
actualdatashifted<-actualcases
actualdatashifted$Date<-actualdatashifted$Date+21
actualdatashifted<-actualdatashifted[,c("CountryName", "Date","ConfirmedCases")]
names(actualdatashifted)<-c("CountryName", "Date","cc3weeksago")
head(actualdatashifted)
actualcases2<-merge(actualcases,actualdatashifted, by = c("CountryName","Date"), all.x = TRUE)
head(actualcases2)
actualcases2$ActiveCases<-actualcases2$ConfirmedCases - actualcases2$cc3weeksago

#display list of country names
#write.csv(as.data.frame(unique(actualcases2$CountryName)),"countrylist.csv")

# set all NAs to 0s
actualcases2[is.na(actualcases2)]<-0

# remove most recent data as some are probematic
actualcases2<-actualcases2[actualcases2$Date!=max(actualcases2$Date),]
actualcases2<-actualcases2[actualcases2$Date!=max(actualcases2$Date),]
max(actualcases2$Date)

# Build a function to plot the stringency index and the cases over time
plotcasestr <- function(mycnt) {
ac_dat<-actualcases2[actualcases2$CountryName==mycnt,]
plot(ac_dat$Date,ac_dat$ConfirmedCases, type = "l",  xlab = "Date", ylab = "Cumulative cases"
     , main = paste0(mycnt," "))
lines(ac_dat$Date,ac_dat$ActiveCases, col = "Blue")
lines(ac_dat$Date,ac_dat$StringencyIndexForDisplay*max(ac_dat$ConfirmedCases)/100, ylim = c(0,100), col = "red")
endsi <- ac_dat$StringencyIndexForDisplay[ac_dat$Date == max(ac_dat$Date)] # this gives us the mose recent stringency index
text(max(ac_dat$Date),max(ac_dat$StringencyIndexForDisplay*max(ac_dat$ConfirmedCases)/100),as.character(endsi), pos = 1)
}
dev.off()
par(mfrow = c(4,3))
par(mar = c(2,1,1,1))
plotcasestr("China")
plotcasestr("South Korea")
plotcasestr("Japan")
plotcasestr("Italy")
plotcasestr("Spain")
plotcasestr("United Kingdom")
plotcasestr("New Zealand")
plotcasestr("Australia")
plotcasestr("United States")
plotcasestr("Sweden")
plotcasestr("Iran")
plotcasestr("South Africa")
dev.copy(png,"CountryCompareStrCC2sept.png"); dev.off()






# bring in the data from the world bank
totpopbycnt<-read.csv("TotalPopByCountry.csv")
head(totpopbycnt)
# get the data on the list of oecd countries
oecdcountries<-read.csv("listofoecdcountries.csv")
head(oecdcountries)
totpopbycnt<-merge(totpopbycnt, oecdcountries, by.x = "Country.Code", by.y = "Countrycode", all.x = TRUE)
totpopbycnt<-totpopbycnt[,c("Country.Name","X2018", "value1")]
names(totpopbycnt)<-c("Country.Name","TotalPop", "isOECDmember")
totpopbycnt$isOECDmember[is.na(totpopbycnt$isOECDmember)]<-0

# merge that on to get deaths per capita

actualcases3<-merge(actualcases2,totpopbycnt
                    , by.x = "CountryName", by.y = "Country.Name", all.x = TRUE)
head(actualcases3)
#oecd<-actualcases3[actualcases3$isOECDmember==1,]
# compute deaths per 1m and cases per 1m
actualcases3$deathrateper1m<-actualcases3$ConfirmedDeaths / actualcases3$TotalPop * 1000000
actualcases3$casesper1m<-actualcases3$ConfirmedCases / actualcases3$TotalPop * 1000000
head(actualcases3)


austac<-actualcases3[actualcases3$CountryName=="Iran",]

dev.off()
par(mfrow = c(3,3))
myfun<-function(cnt){
austac<-actualcases3[actualcases3$CountryName==cnt,]
plot(austac$casesper1m, austac$StringencyIndexForDisplay, main = cnt, xlim = c(0,1000), ylim = c(0,100),
     xlab = "Cases per 1m people", ylab = "Stringency Index")
}
myfun("Italy")
myfun("Spain")
myfun("China")
myfun("Iran")
myfun("Singapore")
myfun("Australia")
myfun("New Zealand")
myfun("United States")
myfun("United Kingdom")
dev.copy(png,"casesvstringency.png"); dev.off()

# get the international arrivals by country for 2018
int_arr<-read.csv("InternationalArrivalsByCountryWorldBank.csv")

head(actualcases3)
head(int_arr)
int_arr<-int_arr[,c("Country.Name","X2018")]
names(int_arr)<-c("Country.Name","arrivalstot")
actualcases4<-merge(actualcases3,int_arr
                    ,by.x = "CountryName",  by.y = "Country.Name", all.x = TRUE)

head(actualcases4)
actualcases4$arrPerPop<-actualcases4$arrivalstot / actualcases4$TotalPop

# get the GDPdata
gdpdata<-read.csv("GDPperCountry.csv")
head(actualcases4)
head(gdpdata)
actualcases5<-merge(actualcases4, gdpdata,by = "CountryName", all.x = TRUE)
actualcases5$gdppercapita<-actualcases5$gdp / actualcases5$TotalPop

# limit to oecd countries
actualcases5<-actualcases5[actualcases5$isOECDmember==1,]
actualcases5<-actualcases5[complete.cases(actualcases5),]
# install.packages("RColorBrewer")
# library(RColorBrewer)
# display.brewer.all()
unique(actualcases5$CountryName)


dev.off()
par(mfrow = c(2,2))
shortlist<-actualcases5[actualcases5$Date=="2020-02-23",]
plot(log(shortlist$arrPerPop),log(shortlist$deathrateper1m), main = "23Feb")
text(log(shortlist$arrPerPop),log(shortlist$deathrateper1m),shortlist$CountryName
     ,cex = 0.7, pos = 4)
#abline(c(0,1))
# dev.copy(png,"CountryPerformanceFeb23.png"); dev.off()


# dev.copy(png,"CountryPerformanceMar10.png"); dev.off()

shortlist<-actualcases5[actualcases5$Date=="2020-03-23",]
plot(log(shortlist$arrPerPop),log(shortlist$deathrateper1m), main = "23 Mar")
text(log(shortlist$arrPerPop),log(shortlist$deathrateper1m),shortlist$CountryName
     ,cex = 0.7, pos = 4)
#abline(c(0,1))
# dev.copy(png,"CountryPerformanceMar23.png"); dev.off()

shortlist<-actualcases5[actualcases5$Date=="2020-04-23",]
plot(log(shortlist$arrPerPop),log(shortlist$deathrateper1m), main = "23 Apr")
text(log(shortlist$arrPerPop),log(shortlist$deathrateper1m),shortlist$CountryName
     ,cex = 0.7, pos = 1)
#abline(c(0,1))

shortlist<-actualcases5[actualcases5$Date=="2020-05-10",]
plot(log(shortlist$arrPerPop),log(shortlist$deathrateper1m), main = "10 May")
text(log(shortlist$arrPerPop),log(shortlist$deathrateper1m),shortlist$CountryName
     ,cex = 0.7, pos = 4)
#abline(c(0,1))

dev.copy(png,"CountryPerformanceApr23.png"); dev.off()

# dev.off()
# shortlist<-actualcases5[actualcases5$Date=="2020-08-10",]
# plot(log(shortlist$arrPerPop),log(shortlist$deathrateper1m), main = "10 Aug")
# text(log(shortlist$arrPerPop),log(shortlist$deathrateper1m),shortlist$CountryName
#      ,cex = 0.7, pos = 4)
#abline(c(0,1))







