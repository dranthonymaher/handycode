


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
times <- seq(0, 200, by = 1)
# **************Optimise these rate parameters******************
parameters <- c(vi = 0.2095,
                vr =0.03,
                vd = 0.01
)
ni<-7
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


head(a1)
a2<-as.data.frame(cbind(a1$time,round(a1$I,0)))
write.csv(a2,file="outbreaksimNSW2.csv")

lhds<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/LHDlist.csv")

i<-1

x1<-NULL
# **************Optimise these rate parameters******************
viL=runif(length(lhds[,1]),0.2,0.643)
vrL=runif(length(lhds[,1]),0.02,0.055)
vdL=runif(length(lhds[,1]),0.0005,0.05)


ni<-7
# define initial state
state <- c(H = n-ni,
           
           I = ni,
           R = 0,
           D = 3)

for (i in 1:length(lhds[,1])){
# solve the ode
  parameters <- c(vi = viL[i],
                  vr =vrL[i],
                  vd = vdL[i]
  )
out1 <- ode(y = state, times = times, func = covidspread, parms = parameters)
a1<-as.data.frame(out1)

a2<-as.data.frame(cbind(a1$time,round(a1$I,0)))

a3<-cbind(rep(lhds[i,],length(a2[,1])),a2)

x1<-rbind(x1,a3)
}

#plot(a1$time,a1$H, type = "l")
plot(x1$V1,x1$V2,col = "orange", ylab = "Cases")

colnames(x1)<-c("LHD","Day","Cases")
write.csv(x1,file="outbreaksimNSW3.csv")


