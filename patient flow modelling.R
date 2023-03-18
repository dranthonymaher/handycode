

# patient flow modelling
# rep("a",15)
# 
# n<-15
# # time arrived
# timearr = rexp(n, 1) 
# 
# # time in ED
# timeinED = rexp(n, 1) +2
# 
# # time released from ED
# timeout<-timearr+timeinED
# 
# myseq<-1:n
# plot(timearr,myseq,xlim = c(0,max(timeout)))
# 
# arrows(timearr,myseq,timeout,myseq,code = 3)

# try again using geometric dist
#hist(rgeom(1000,1/5),20)

n<-150
meantime<-10
# time arrived
timearr = rgeom(n, 1/meantime) 
timearr = round(runif(n, 0,100) )

# time in ED
timeinED = rgeom(n, 1/meantime) +2

# time released from ED
timeout<-timearr+timeinED

#combine to df
df<-as.data.frame(cbind(timearr,timeinED,timeout))
colnames(df)<-c('timearr','timeinED','timeout')
#sort it
df<-df[order(timearr),]

# needs ward admission?
df$needsward<-rbinom(n,1,0.5)
myseq<-1:n
plot(df$timearr,myseq,xlim = c(0,max(timeout)))

segments(df$timearr,myseq,df$timeout,myseq)

# create time vec 
timevec<-as.data.frame(seq(0,150))
colnames(timevec)<-c("timevec")

#summarise the timearr and timeout
tadf<-as.data.frame(table(timearr))
todf<-as.data.frame(table(timeout))

#merge
df2<-merge(timevec,tadf,by.x = "timevec",by.y="timearr", all.x = TRUE)
colnames(df2)[2]<-"number.in"

df3<-merge(df2,todf,by.x = "timevec",by.y="timeout", all.x = TRUE)
colnames(df3)[3]<-"number.out"

#set all NAs to 0
df3[is.na(df3)]<-0

df3$netnumber<-cumsum(df3$number.in)-cumsum(df3$number.out)

lines(df3$timevec,df3$netnumber,type = 'l', col='blue',lwd=2)

