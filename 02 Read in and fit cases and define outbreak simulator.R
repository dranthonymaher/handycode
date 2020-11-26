# Read in national cases data and fit to model to get parameters

#Anthony Maher


#setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/covid19/")

# Fit case data curve and predict cases in 1 and 2 weeks -----------------------------------------------------

# read in the actual Australian data and clean and process

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
                                substr(as.character(actualcases$Date),7,8),sep = "-")) # converts the date to proper date format
head(actualcases)
dev.off()
#compute active cases as those within the last 2 weeks only
# shift the dates of the actual data by sicktime days
sicktime <-12
actualdatashifted<-actualcases
actualdatashifted$Date<-actualdatashifted$Date+sicktime
actualdatashifted<-actualdatashifted[,c("CountryName", "Date","ConfirmedCases")]
names(actualdatashifted)<-c("CountryName", "Date","cc3weeksago")
head(actualdatashifted)
actualcases2<-merge(actualcases,actualdatashifted, by = c("CountryName","Date"), all.x = TRUE)
head(actualcases2)
actualcases2$ActiveCases<-actualcases2$ConfirmedCases - actualcases2$cc3weeksago
actualcases2[is.na(actualcases2)]<-0

# extract aussie data only
astactuals<-actualcases2[actualcases2$CountryName=="Australia",]
head(astactuals)
astactuals<-astactuals[astactuals$Date>=as.Date("2020-03-20"),]
astactuals$time<-astactuals$Date - as.Date("2020-03-20")
plot(astactuals$time, astactuals$ConfirmedCases, main = "Confirmed cases in Australia")

# compute the active case to cumulative case ratio
# astactuals$actoccratio<-astactuals$ActiveCases / astactuals$ConfirmedCases
# plot(astactuals$time, astactuals$actoccratio)


# 
# Solve system of partial differential equations and fit rate parameters ------------------------------------------------------------

library(deSolve)


n=6800
covidspread<-function(times, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    dH <- -vi*H*I/(n*1) # Healthy
    dI <- vi*H*I/(n*1)- vr*I - vd*I #infected, symptomatic 
    dR <- vr*I # recovered
    dD <- vd*I # Dead
    
    # return the rate of change
    list(c(dH, dI, dR, dD))
  }) 
}


#set number of days
times <- seq(0, length(astactuals[,1]), by = 1)
# **************Optimise these rate parameters******************
parameters <- c(vi = 0.295,
                vr =0.03,
                vd = 0.01
)
ni<-700
# define initial state
state <- c(H = n-ni,
          
           I = ni,
           R = 0,
           D = 3)
# solve the ode
out1 <- ode(y = state, times = times, func = covidspread, parms = parameters)
a1<-as.data.frame(out1)
a1$totcases<-a1$I+a1$R+a1$D

#plot(a1$time,a1$H, type = "l")
plot(a1$time,a1$totcases,col = "orange",ylim = c(0,10000) , type = "l", ylab = "Cases"); text(40,7000,"Cumulative cases", col = "orange")
lines(a1$time,a1$I,col = "red" ); text(45,2000,"Number active cases", col = "red")
lines(a1$time,a1$R,col = "blue" ); text(60,5000,"Number recovered", col = "blue")
lines(a1$time,a1$D,col = "green" ); text(60,1700,"Number dead", col = "green")

points(astactuals$time,astactuals$ConfirmedCases,col = "orange")
points(astactuals$time, astactuals$ActiveCases, col = "red")



# Construct the outbreak simulator function -------------------------------

outbreaksimulator<-function(n,ni){
  # define the number of susceptibles as n
  #n=6800
  covidspread<-function(times, state, parameters) {
    with(as.list(c(state, parameters)),{
      # rate of change
      dH <- -vi*H*I/(n*1) # Healthy
      dI <- vi*H*I/(n*1)- vr*I - vd*I #infected, symptomatic 
      dR <- vr*I # recovered
      dD <- vd*I # Dead
      
      # return the rate of change
      list(c(dH, dI, dR, dD))
    }) 
  }
  
  
  #set number of days
  times <- seq(0, 200, by = 1)
  # define rate parameters
  parameters <- c(vi = 0.195,
                  vr =0.03,
                  vd = 0.01
  )
  # define the number of currently infected as ni
  
  # define initial state
  state <- c(H = n-ni,
             
             I = ni,
             R = 0,
             D = 3)
  # solve the ode
  out1 <- ode(y = state, times = times, func = covidspread, parms = parameters)
  a1<-as.data.frame(out1)
  a1$totcases<-a1$I+a1$R+a1$D
  
  
  return(a1)
  
}
