
days<-1:100
cases<-round(exp(-0.0182*(days-30)^2+7))
plot(days, cases)

df<-NULL

#build df - the patient admission data
for (i in 1:length(days)){

alos<-rpois(cases[i],10)
df<-rbind(df,cbind(rep(i,cases[i]),rep(cases[i],cases[i]),alos))
}
df<-as.data.frame(df)
colnames(df)<-c("day.of.outbreak","number.of.cases","los")

df$separation<-df$day.of.outbreak+df$los-1
head(df)
#asssume 10% of people go to hospital
df<-df[sample(1:length(df[,1]),round(length(df[,1])*0.1)),]

# x1<-rep(0,length(df[,1]))
# x1<-as.data.frame(x1)

x1<-NULL
for (i in 1:length(df[,1])){
# i<-2
myvec<-c(rep(0,df[i,1]),rep(1,df[i,3]),rep(0,100-df[i,1]-df[i,3]))
x1<-rbind(x1,myvec)
}
cases
hospload<-colSums(x1)
lines(days,hospload)
plot(days,(cases))
lines(days, hospload)
points(days, cases)



