


rm(list = ls())

# really simple

x1<--100:100
y1<-exp(x1)/(1+exp(x1))

plot(x1,y1)

z1<-c(rbinom(101,1,0.1),rbinom(100,1,0.9))
points(x1,z1,col='blue')

model<-glm(z1~x1,family = 'binomial')
summary(model)












x1<-runif(200,0,10)
x2<-runif(200,0,10)
df<-as.data.frame(cbind(x1,x2))
colnames(df)<-c("x1","x2")

plot(df)

df$r1<-ifelse(df$x1>5 & df$x2>5,1,0)

#add some noise
df$x1<-df$x1+rnorm(length(df$x1))
df$x2<-df$x2+rnorm(length(df$x2))

plot(df)

plot(df[df$r1==0,1:2],col = "blue")
points(df[df$r1==1,1:2],col = "red")

model<-glm(r1~x1+x2,data = df, family = "binomial")
summary(model)




rm(list = ls())

x1<-runif(2000,0,10)
x2<-rep(c("a","b"),0,length(x1))
df<-as.data.frame(cbind(x1,x2))
colnames(df)<-c("x1","x2")

plot(df)

df$r1<-ifelse(df$x1>8 & df$x2=="a",1,
              ifelse(df$x1<8 & df$x2=="b",1,0))

str(df)
df$x1<-as.numeric(df$x1)
plot(df)

#add some noise
df$x1<-df$x1+rnorm(length(df$x1))
# df$x2<-df$x2+rnorm(length(df$x2))
aggregate(df$r1,list(df$x2),FUN=mean)


model<-glm(r1~x1+x2,data = df, family = "binomial")
summary(model)

exp(2.72)




rm(list = ls())

x1<-runif(200,0,10)
x2<-rep(c(0,1),0,length(x1))
df<-as.data.frame(cbind(x1,x2))
colnames(df)<-c("x1","x2")

plot(df)

df$r1<-ifelse(df$x1>8 & df$x2==0,1,
              ifelse(df$x1<8 & df$x2==1,1,0))

str(df)
df$x1<-as.numeric(df$x1)
plot(df)


plot(df[df$r1==0,1:2],col = "blue")
points(df[df$r1==1,1:2],col = "red")

#add some noise
# df$x1<-df$x1+rnorm(length(df$x1))
# df$x2<-df$x2+rnorm(length(df$x2))
aggregate(df$r1,list(df$x2),FUN=mean)

mysample<-sample(nrow(df),150)
s1<-df[mysample,]
holdout<-setdiff(1:nrow(df),mysample)
model<-glm(r1~x1+x2,data = s1, family = "binomial")
summary(model)
pred1<-predict(model,newdata = df[holdout,],type = "response")

plot(df$r1[holdout],pred1)

exp(2.72)
