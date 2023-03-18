

x=NULL
x = rnorm(2)
x2 = rnorm(2)
for (i in 1:1000){
  
  # step<-rbinom(2,2,0.5)-1
  step<-rnorm(2)
  step2=rnorm(2)
  x = rbind(x,step)
  x2=rbind(x2,step2)
  cx=apply(x,2,cumsum)
  cx2=apply(x2,2,cumsum)
  plot(cx, type = 'l', main = paste0(i), lwd = 2, col = 'gray')
  lines(cx2)
  points(cx[i,1],cx[i,2], pch=16, col='red')
  
  
  
  Sys.sleep(0.05)
    }
# df<-cbind(rnorm(20),rnorm(20))
# plot(df, type = 'l')
# how aboiut a population of 100 people
x<-matrix(rnorm(100*100,0,1),nrow=100,byrow = TRUE)
cx<-apply(x,2,cumsum)
y<-matrix(rnorm(100*100,0,1),nrow=100,byrow = TRUE)
cy<-apply(y,2,cumsum)

for (i in 3:100){
  plot(cx[i,],cy[i,], main = paste0(i), col = 'blue',xlim=c(-25,25),ylim=c(-25,25),pch=16)
  j=i-1
  k=j-1
  segments(cx[j,],cy[j,],cx[i,],cy[i,],col = 'pink')
  segments(cx[k,],cy[k,],cx[j,],cy[j,],col = 'pink')
  
  
   Sys.sleep(0.05)
}


# Part 2 - try introducing illness ----------------------------------------

plot(cx[50,],cy[50,])

p1<-1
mypoint<-cbind(cx[50,],cy[50,])
mydist<-NULL
for (i in 1:100){
  mydist[i]<-dist(rbind(c(0,0),mypoint[i,]))
}

# which are less that 1 unit from 0,0?
closeto0<-which(mydist<2, arr.ind = FALSE)

#tag ones with cv
haverona<-NULL
haverona<-c(closeto0,haverona)
uninfected<-setdiff(1:100,haverona)
plot(cx[50,uninfected],cy[50,uninfected],col='blue',pch=16)
points(cx[50,haverona],cy[50,haverona],col='red',pch=16)
