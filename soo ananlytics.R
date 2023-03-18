# analyse the SOO results

# import the data
rm(list=ls())


sooresults<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/soo/sooresultlist.csv")

sooresults[is.na(sooresults)]<-0

# create cumulative results for games
sooresults$cumNSW<-cumsum(sooresults$NSWwin)
sooresults$cumqld<-cumsum(sooresults$QldWin)
# CREATE x variable
sooresults$myx<-sooresults$Year+(sooresults$GameNo-1)/3
plot(sooresults$myx,sooresults$cumqld, type = 'l',col='red',xlab = "Year",ylab = "Cumulative games won",
     main = "NSW v Qld State of Origin games won",lwd = 2)
lines(sooresults$myx,sooresults$cumNSW, type = 'l',col='blue',lwd = 2)
rect(1988,0,1992,30,col = rgb(0.5,0.5,0.5,0.1),border = NA)
text(1990,30,"Lewis, Langer era", pos=3)
rect(2009,0,2018,59,col = rgb(0.5,0.5,0.5,0.1),border = NA)
text((2009+2018)/2,59,"Lockyer, Smith, Thurston, Slater era", pos=3)

dev.copy(png,"NSWvQldgames.png", width = 800, height = 600); dev.off()

# get list of first games
firstgame<-sooresults[sooresults$GameNo==1,]
firstgame<-firstgame[,c("Year","GameNo","Game.winner")]
names(firstgame)<-c("Year","GameNo","firstGameWinner")

# get a list of series winners

a1<-aggregate(cbind(sooresults$NSWwin, sooresults$QldWin),by=list(sooresults$Year),FUN=sum)
names(a1)<-c("Year", "NSWnumwins","QldNumWins")

a1$Serieswin<-ifelse(a1$NSWnumwins==a1$QldNumWins,"Drawn",
                     ifelse(a1$NSWnumwins>a1$QldNumWins,"New South Wales", "Queensland"))
# create win col for each state
a1$nswwin<-ifelse(a1$Serieswin=="New South Wales",1,0)
a1$qldwin<-ifelse(a1$Serieswin=="Queensland",1,0)

# create cumulative results for games
a1$cumNSW<-cumsum(a1$nswwin)
a1$cumqld<-cumsum(a1$qldwin)

#plot it
plot(a1$Year,a1$cumqld, type = 'l',col='red',xlab = "Year",ylim = c(0,max(a1$cumqld))
     ,ylab = "Cumulative games won",lwd = 2,
     main = "NSW v Qld State of Origin Series won")
lines(a1$Year,a1$cumNSW, type = 'l',col='blue',lwd=2)
rect(1988,-10,1992,30,col = rgb(0.5,0.5,0.5,0.1),border = NA)
#text(1990,30,"Lewis, Langer era", pos=3)
rect(2009,-10,2018,59,col = rgb(0.5,0.5,0.5,0.1),border = NA)
#text((2009+2018)/2,59,"Lockyer, Smith, Thurston, Slater era", pos=3)

dev.copy(png,"NSWvQldseries.png", width = 800, height = 600); dev.off()


series1<-merge(firstgame,a1,by = "Year",all.x = TRUE)

series1$firstgmwinwonseries<-ifelse(series1$firstGameWinner==series1$Serieswin,1,0)

paste0("the team that won the first game has gone on to win the series ",sum(series1$firstgmwinwonseries), " times over ", 
length(series1$firstgmwinwonseries)," series. THat's ",
round(100*sum(series1$firstgmwinwonseries) / length(series1$firstgmwinwonseries)),"% of the time")


# let's simulate a whole bunch
x1<-NULL
ntrials<-1000
for (i in 1:ntrials){
# let's simulate  38 series with each game  decided by too of a coin
sim<-as.data.frame(matrix(rbinom(38*3,1,0.5), nrow=38,byrow = TRUE))
names(sim)<-c("g1", "g2", "g3")
sim$nswgamewins<-sim$g1+sim$g2+sim$g3
sim$firstGameWinner<-ifelse(sim$g1>0,"New South Wales", "Queensland")
sim$series.winner<-ifelse(sim$nswgamewins>1.5,"New South Wales", "Queensland")

sim$firstgmwinwonseries<-ifelse(sim$firstGameWinner==sim$series.winner,1,0)
print(paste0("First winner won "
       ,sum(sim$firstgmwinwonseries), " times over ", 
       length(sim$firstgmwinwonseries)," series. That's ",
       round(100*sum(sim$firstgmwinwonseries) / length(sim$firstgmwinwonseries)),"%"))
x1[i]<-sum(sim$firstgmwinwonseries)
}
plot(x1, type = "l", ylim = c(0,38))

mean(x1)*100 / 38
sd(x1)
