mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)


d1<-aggregate(mydata["gre"],by=mydata["admit"],FUN=mean)
barplot(d1$gre,col="#FF0016")
d1
mydata["gre"]
#check the scatter plots
par(mfrow=c(2,2))
plot(mydata$gre,mydata$admit)
plot(mydata$gpa,mydata$admit)
plot(mydata$rank,mydata$admit)

rgb(255,0,22,max=255)


