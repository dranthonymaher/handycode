rm(list = ls())

#read in the data
df<- read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/nrl-2021-AUSEasternStandardTime.csv")

#make a few minor fixes and add cols for the scores 
df$home.score<-as.numeric(sub("(.*)-.*", "\\1", df$Result))
df$away.score<-as.numeric(sub(".*-(.*)", "\\1", df$Result))

#str(df)

# allteams<-unique(df$Home.Team)
# allteams
#who won?
# df$winner<-ifelse(df$home.score>df$away.score,df$Home.Team,
#                   ifelse(df$home.score<df$away.score,df$Away.Team,"Draw"))
# df$loser<-ifelse(df$home.score<df$away.score,df$Home.Team,
#                   ifelse(df$home.score>df$away.score,df$Away.Team,"Draw"))
# 
# df$pdiff<-ifelse(df$Home.Team==df$winner,df$home.score-df$away.score,df$away.score-df$home.score)
# df$pdiffneg<-ifelse(df$Home.Team==df$winner,-df$home.score+df$away.score,-df$away.score+df$home.score)

# head(df)
mypos<-NULL
outcometext<-NULL
n1<-1000
for (i in 1:n1){

#separate home v away teams
dfh<-df[,c(1,2,3,4,5,8,9)]
dfa<-df[,c(1,2,3,4,6,9,8)]

# cn.dfh<-colnames(dfh)
colnames(dfh)<-c("Match.number","round","date","location","team","points.for","points.against")
colnames(dfa)<-c("Match.number","round","date","location","team","points.for","points.against")
df1<-rbind(dfh,dfa)
rm(dfh,dfa)
# enter ladder points earned
df1$pts<-ifelse(df1$points.for>df1$points.against,2,ifelse(df1$points.for==df1$points.against,1,0))



# Part 2 ------------------------------------------------------------------

#next: simulate the scores based on teams scores from past rounds
# use the original table for this? Want to consider team and opposition

dfpast<-df1[is.na(df1$points.for)==FALSE,]
dffutu<-df1[is.na(df1$points.for)==TRUE,]

avs<-aggregate(cbind(dfpast$points.for,dfpast$points.against),list(dfpast$team)
          ,FUN =function(x) c(mean=mean(x), sd=sd(x)))
colnames(avs)<-c("team","avptsforhome","avptsagthome")
avs
# merge onto original df to build predicted score
dffutu2<-merge(df,avs,by.x = "Home.Team", by.y = "team", all.x = TRUE)

colnames(avs)<-c("team","avptsforaway","avptsagtaway")
dffutu2<-merge(dffutu2,avs,by.x = "Away.Team", by.y = "team", all.x = TRUE)

#calculate the ave score for home v away teams
dffutu2$expected.home.score <- (dffutu2$avptsforhome+dffutu2$avptsagtaway)/2
dffutu2$expected.away.score <- (dffutu2$avptsagthome+dffutu2$avptsforaway)/2

# head(dffutu2)

# simulation --------------------------------------------------------------


# simulated score
dffutu2$home.team.sim.score<-round(rnorm(length(dffutu2[,1]),dffutu2$expected.home.score,10))
dffutu2$away.team.sim.score<-round(rnorm(length(dffutu2[,1]),dffutu2$expected.away.score,10))

# split the data
dfpast<-dffutu2[is.na(dffutu2$home.score)==FALSE,]
dffutu<-dffutu2[is.na(dffutu2$home.score)==TRUE,]
# colnames(dfpast)

#separate home v away teams
dfpastp2<-dfpast[,c(1,2,3,4,5,8,9)]
dffutup2<-dffutu[,c(1,2,3,4,6,16,17)]
# colnames(dfpastp2)
# cn.dfh<-colnames(dfh)
colnames(dffutup2)<-c("Away.Team",    "Home.Team",    "Match.Number", "Round.Number", "Date", "home.score",   "away.score")
dfnew<-rbind(dfpastp2,dffutup2)
rm(dfpastp2,dffutup2)
# enter ladder points earned
#dfnew$pts<-ifelse(df1$points.for>df1$points.against,2,ifelse(df1$points.for==df1$points.against,1,0))

# now need to re-structure that into team-round level data


#separate home v away teams
dfh<-dfnew[,c(2,3,4,5,6,7)]
dfa<-dfnew[,c(1,3,4,5,7,6)]

# cn.dfh<-colnames(dfh)
colnames(dfh)<-c("team","match.number","round","date","points.for","points.against")
colnames(dfa)<-c("team","match.number","round","date","points.for","points.against")
df1<-rbind(dfh,dfa)
rm(dfh,dfa)
# enter ladder points earned
df1$pts<-ifelse(df1$points.for>df1$points.against,2,ifelse(df1$points.for==df1$points.against,1,0))

df1text<-paste(as.character(df1[df1$round>=23,]))
gettext<-dffutu2
gettext<-gettext[order(gettext$Match.Number),]
gettext$winner<-ifelse(gettext$home.team.sim.score>gettext$away.team.sim.score,gettext$Home.Team,gettext$Away.Team)
gettext$loser<-ifelse(gettext$home.team.sim.score>gettext$away.team.sim.score,gettext$Away.Team,gettext$Home.Team)
gettext$diff<-abs(gettext$home.team.sim.score - gettext$away.team.sim.score)
gettext$textiwant<-paste(gettext$winner,"beat",gettext$loser,"by",gettext$diff,"in round",gettext$Round.Number)
# squash(as.character(gettext$textiwant[gettext$Round.Number>22]))
mytext<-paste(gettext$textiwant[gettext$Round.Number>23], collapse = '; ')
mytext
# Generate table ----------------------------------------------------------


#get the table after n rounds
n<-25
d22<-df1[df1$round<=n,]
t22<-aggregate(cbind(d22$pts,d22$points.for,d22$points.against),list(d22$team),FUN = sum )
colnames(t22)<-c("Team","Points","For","Against")
t22$pd<-t22$For-t22$Against
t22<-t22[order(-t22$Points,-t22$pd),]
t22$ladder.pos<-1:length(t22[,1])
rownames(t22)<-t22$ladder.pos
t22
#

mypos[i]<-which(t22$Team=="Wests Tigers")
outcometext[i]<-mytext
}
mypos
result<-table(mypos)

b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="Final ladder position",ylab = "Number of outcomes"
            ,main =  "Wests Tigers finals hopes")
text(b1,result+max(result)*0.05,paste0(as.character(result/(n1/100)),"%"))

