
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")

m1<-read.csv("Premium comparisons by age2.csv")


v1<-m1[,1]
dim(v1)<-c(7,64)

v2<-m1[1:336,2]
v3<-m1[1:224,3]
v4<-m1[1:384,4]
v5<-m1[1:384,5]

336/7
224/7
384/8

dim(v2)<-c(7,48)
dim(v3)<-c(7,32)
dim(v4)<-c(8,48)
dim(v5)<-c(8,48)

write.csv(as.data.frame(v1),"v1.csv")

write.csv(as.data.frame(v2),"v2.csv")

write.csv(as.data.frame(v3),"v3.csv")

write.csv(as.data.frame(v4),"v4.csv")

write.csv(as.data.frame(v5),"v5.csv")

