#PCA
nvec<-30
a<-rnorm(nvec)
b<-seq(1,3,length.out=nvec)
c<-rnorm(nvec,1,10)
d<-rnorm(nvec)

X<-data.frame(a,b,c,d)
X
as.formula(c(a,b,c))


pca<-princomp(X)
plot(pca$scores[,1],pca$scores[,2])



#PCA really simple
a<-c(-1,-.5,-.5,.5,.5,1,1,2)
b<-c(-.5,-1,.5,-.5,1,1,3,2)

X<-data.frame(a,b)
plot(a,b)

pca<-princomp(X)
plot(pca$scores[,1],pca$scores[,2])


#PCA really simple
a<-c(-1,-.5,-.5,.5,.5,1,1,2)
b<-c(0,1,0,0,1,0,1,0)
c<-c(0,0,0,0,0,1,0,0)

X<-data.frame(a,b,c)
X
plot(a,b)

pca<-princomp(X)
plot(pca$scores[,1],pca$scores[,2])










