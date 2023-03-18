library(alluvial)

tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
tit

n<-100000
sid<-seq(1,n,1)
y10<-rbinom(n,1,0.95)
y11<-rbinom(n,1,0.95)*y10
y12<-rbinom(n,1,0.95)*y11
uni<-rbinom(n,1,0.65)*y12
vet<-rbinom(n,1,0.8)*(1-uni)
comb <- uni*2+vet
employed<-ifelse(comb==2,rbinom(n,1,0.95)
                 ,ifelse(comb==1,rbinom(n,1,0.6),rbinom(n,1,0.4)))



table(comb)

df<-as.data.frame(cbind(sid,y10,y11,y12,comb,employed))

a1<-aggregate(rep(1,n),by =list(y10,y11,y12,comb,employed), FUN = length)
names(a1)<-c("y10","y11","y12","postschool","age25","Freq")
a1

mycol<-(a1$postschool+3*a1$age25)+1

a1$y10[a1$y10==0]<-"absent"
a1$y10[a1$y10==1]<-"attended"

a1$y11[a1$y11==0]<-"absent"
a1$y11[a1$y11==1]<-"attended"
a1$y12[a1$y12==0]<-"absent"
a1$y12[a1$y12==1]<-"attended"

a1$postschool[a1$postschool==0]<-"NEET"
a1$postschool[a1$postschool==2]<-"Uni"
a1$postschool[a1$postschool==1]<-"TAFE"

a1$age25[a1$age25==1]<-"b.Employed"
a1$age25[a1$age25==0]<-"a.Unemployed"

a1
alluvial(a1[,1:5], freq=a1$Freq,
         col = mycol,
        # border = ifelse(tit$Survived == "Yes", "orange", "grey"),
         hide = a1$Freq == 0,
         cex = 0.7
)
