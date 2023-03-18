
x1<-NULL
y1<-NULL
x<-0
x1[1]<-0
y1[1]<-10
for (i in 2:100){
  r1<-rbinom(2,2,0.5)-1
  x1[i]<-x1[i-1]+r1[1]
  y1[i]<-y1[i-1]+r1[2]
  if (abs(x1[i]-y1[i])<2){x1}
}

plot(x1, type = 'l',ylim = c(min(x1,y1),max(x1,y1)))
lines(y1,col='red')


