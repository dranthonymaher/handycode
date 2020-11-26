# READ IN CASE DATA FOR EVERY LGA IN NSW AND COMPUTE ACTIVE CASES
# run all

# read in the historical NSW cases data
nswoldcasedata<-read.csv("C:/Users/amaher2/KPMG/AU - Data Office - Data sets/NSWcovidLGAstable5dates_v3.csv")
head(nswoldcasedata)
# convert to long format
n2<-reshape(nswoldcasedata,   idvar = c("LGAname"), 
                                  varying = list(names(nswoldcasedata)[2:7]),
                                  v.names = "Cases_positive",
                                   timevar = "Date",
                                   times = names(nswoldcasedata)[2:7],
                                direction = "long")
head(n2)
# the row names are a bit weird - fix them
row.names(n2)<-as.character(1:length(n2[,1]))
str(n2)
n2$Date<-as.Date(substr(n2$Date,2,100), "%d.%m.%Y")
head(n2)

#read in daily NSW case data from Mo
nswcasedata<-read.csv("C:/Users/amaher2/KPMG/AU - Data Office - Data sets/NSW_COVID_DATA18052020.csv")
#                      ,colClasses=c("Date",NA, NA, NA))
head(nswcasedata)

# fix the date
nswcasedata$Date<-as.Date(nswcasedata$Date, "%m/%d/%Y")
head(nswcasedata)
n3<-data.frame(nswcasedata$LGA, as.Date(nswcasedata$Date), nswcasedata$Cases_positive)
names(n3)<-names(n2)
head(n2)
# combine them
rnew<-as.data.frame(rbind(n2,n3))
str(rnew)

# read in the other data set to fix up the lack of codes for LGA in this data!!
fixer<-read.csv("C:/Users/amaher2/KPMG/AU - Data Office - Data sets/LGA_outbreak_Simulator.csv")
head(fixer)
f1<- unique(fixer[,c(2,3)])
head(f1)
nswcasedata2<-merge(rnew,f1,by= "LGAname", all.x = TRUE)
head(nswcasedata2)
str(nswcasedata2)
plot(nswcasedata2$Date, nswcasedata2$Cases_positive, main = "Cumulative cases in each LGA")
# de-dupe

#compute active cases as those within the last 12 days only
# shift the dates of the actual data by sicktime days
# sicktime <-12
# datashifted<-nswcasedata2
# datashifted$Date2<-datashifted$Date+sicktime
# Bugger - aint going to work as there are gaps in the data!!
# need to interpolate

#ok first get the min date
#min(nswcasedata2$Date) # ok it's the 26th of march
# generate a list of every date available:
datelist<-seq(min(nswcasedata2$Date),max(nswcasedata2$Date), by = 1)
# generate a grid of the cartesian product of those two variables
spine<-as.data.frame(expand.grid(unique(nswcasedata2$LGAcode),datelist, KEEP.OUT.ATTRS = FALSE))
names(spine)<-c("LGAcode", "Date")
nswcasedata3<-merge(spine,nswcasedata2, by = c("LGAcode", "Date"), all.x = TRUE)
head(nswcasedata3)

# need to create a loop to do the interpolation here:
nedd4<-NULL
mycode<-unique(nswcasedata3$LGAcode)

for (i in 1:length(unique(nswcasedata3$LGAcode))){
newd1<-nswcasedata3[nswcasedata3$LGAcode == mycode[i],]
newd2<-newd1[complete.cases(newd1$LGAcode),]
newd3<-newd2[!duplicated(newd2$Date),]
print(paste0(i," ",mycode[i]))
x1<-approx(newd3$Date, newd3$Cases_positive, xout =datelist)
newd3$newc<-round(x1$y)
nedd4<-rbind(nedd4,newd3)
}

# assume that the infection is active for 12 days
sicktime <-12
datashifted<-nedd4
datashifted$Date2<-datashifted$Date+sicktime
head(datashifted)
datashifted<-datashifted[,c("LGAcode", "Date2","newc")]
names(datashifted)<-c("LGAcode", "Date","Cases_positive")
head(datashifted)
head(nedd4)
cases2<-merge(nedd4,datashifted, by = c("LGAcode","Date"), all.x = TRUE)
head(cases2, 10)
# compute active cases
cases2$ActiveCases<-cases2$newc - cases2$Cases_positive.y
head(cases2, 32)
# clean up
cases2<-cases2[,c("LGAcode","Date","newc","ActiveCases")]
names(cases2)<-c("LGAcode","Date","CumCases","ActiveCases")
plot(cases2$Date,cases2$ActiveCases)
# set NAs, -ves to 0s
cases2$ActiveCases<-ifelse(cases2$ActiveCases<0, 0, cases2$ActiveCases)
cases2$ActiveCases[is.na(cases2$ActiveCases)]<-0
cases2<-merge(cases2,unique(fixer[,c("LGAname", "LGAcode")]),by = "LGAcode", all.x = TRUE)
 # get the data for 16 April
currentActivecases<-cases2[cases2$Date==max(cases2$Date)-30,]
currentActivecases
head(fixer)
head(currentActivecases)
cac<-merge(currentActivecases,unique(fixer[,c("LGAname", "LGAcode")]),by = "LGAcode", all.x = TRUE)
head(cac)
head(cases2)
# order them from highest cum cases to lowest
cacordered<-cac[order(-cac$CumCases),]
mylga<-"Waverley"

# Build a function to plot the cases over time
plotcasestr <- function(mylga) {
  ac_dat<-cases2[cases2$LGAname==mylga,]
  plot(ac_dat$Date,ac_dat$CumCases, type = "o",  xlab = "Date", ylab = "Cumulative cases",
       ylim = c(0,1.5*max(cacordered$CumCases, na.rm = TRUE))
      , cex = 1.1,bty="n")
  title(paste0(mylga," "), line = -2)
  lines(ac_dat$Date,ac_dat$ActiveCases, col = "red")
  text(max(ac_dat$Date),
       max(ac_dat$CumCases, na.rm = TRUE),
       as.character("Cumulative Cases                    "), pos =1)
  text(max(ac_dat$Date),
       max(ac_dat$ActiveCases, na.rm = TRUE),
       as.character("Active Cases"), pos =2, col = "red")
}
dev.off()
par(mfrow = c(4,3))
par(mar = c(2,1,1,1))
cacordered<-cac[order(-cac$CumCases),]
head(cacordered)
for (i in 1:12){
  print(i)
  plotcasestr(cacordered$LGAname.x[i])  
}

