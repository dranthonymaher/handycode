# Script to demo how to use PCA and k-means clustering in R
# Anthony Maher 13/3/20

dev.off()
# remove all objects
rm(list = ls(all.names = TRUE))


#set the working directory
#!!!- each user will need to change this for themselves to the directory that contains their data!!!
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")

#  import the data

mydata<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/Data/2016 Census GCP All Geographies for AUST/SSC/AUST/2016Census_G01_AUS_SSC_suburbname.csv")

mydata$statenum = substr(mydata$SSC_CODE_2016,4,4)
mydata2<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/Data/2016 Census GCP All Geographies for AUST/SSC/AUST/2016Census_G02_AUS_SSC.csv")
mydata2$subsurbname<-mydata$Census_Name_2016
mydata2$statenum<-mydata$statenum
mydata<-mydata2
# check the colnames - which are the ones that t?
colnames(mydata)
#table(mydata$statenum)
# so get cols  - this is our model file for PCA
modeldata<-mydata[,2:9]

# remove rows without complete cases from both data files
mydata<-mydata[complete.cases(modeldata),]
modeldata<-modeldata[complete.cases(modeldata),]


# build PCA model
model<- princomp(modeldata, cor=TRUE)
summary(model)

#output<-cbind(mydata[,c(1,2,3)],model$scores[,c(1:10)])
# write.csv(output,"absdatapcascoresSSC.csv")


# check the proportion of variance explained
PoV <- model$sdev^2/sum(model$sdev^2)
Cumprov<-cumsum(PoV)
plot(PoV, type = "l", ylim=c(0,1))
lines(Cumprov, col = 'blue')
#3 PCs capture over xx% of variation


#clean the names - remove everythging in the brackets
# mydata$Label_x<-gsub("\\s*\\([^\\)]+\\)","",mydata$Label_x)

loads<-as.data.frame(model$loadings[,2:3])
barplot(model$loadings[,3])

#plotting
library(scatterD3)

mysuburb<-"Five Dock"
refsub<-which(mydata$subsurbname==mysuburb)
i=2
j=3
scalextot<-0.01*(max(model$scores[,i])-min(model$scores[,i]))
scaleytot<-0.01*(max(model$scores[,j])-min(model$scores[,j]))
xval<-model$scores[refsub,i]
yval<-model$scores[refsub,j]

# create zoomable scatter plot
scatterD3(x=model$scores[,i],y=model$scores[,j],lab =  mydata$subsurbname ,col_var = mydata$statenum
          ,xlim=c(xval-scalextot,xval+scalextot)
          ,ylim=c(yval-scaleytot,yval+scaleytot)          
          ,point_opacity = 1)

# create zoomable loadings plot
scatterD3(x=model$loadings[,i],y=model$loadings[,j],lab =  colnames(modeldata) ,
          point_opacity = 1)



xdata<-c(model$scores[,i],model$loadings[,i]*180)
ydata<-c(model$scores[,j],model$loadings[,j]*180)
ldata<-c(paste0(mydata$state,"_", mydata$Label_x),colnames(modeldata))
scatterD3(x=xdata,y=ydata,lab = ldata ,col_var = c(mydata$state,rep("loads",456)),
          opacity_var = c(rep(1,length(modeldata[,1])),rep(0.5,length(modeldata[1,]))))




# k-means clustering
expl<-NULL
#e1<-NULL
# build for loop to identify best number of clusters
for (i in 1:20) {
  kmeansmodel<-kmeans(modeldata,i)
  #e1[i]<-kmeansmodel$betweenss
  expl[i]<- kmeansmodel$betweenss / kmeansmodel$totss
}
plot(expl, type = "l",main = "Between SS as a proportion of total SS", las=2)
lines(diff(expl),col = "blue")

# the above shows that the explained variance starts to lvel off after about 7 clusters.
kmeansmodel<-kmeans(modeldata,7)

#get the clusters and add them to the dataset
mydata$cluster<-kmeansmodel$cluster

# now aggregate the data by cluster to get the average numers of students in each year for each cluster
round(aggregate(modeldata,by=mydata["cluster"],FUN = mean))
#count how many schools are in each cluster:
table(mydata$cluster)

# create zoomable scatter plot
scatterD3(x=model$scores[,i],y=model$scores[,j],lab =  mydata$Label_x ,col_var = mydata$cluster
          ,
          point_opacity = 1)


#check the mix of schools in each cluster
table(mydata$SMMA_Typology, mydata$cluster)

# replot the pca but colour by cluster
par(xpd=TRUE)
plot(model$scores[,1],model$scores[,3], col = mydata$cluster, pch = 19)
legend(-2,4,legend = sort(unique(mydata$cluster)), fill =  c(1:6), cex=0.8)
text(model$scores[,1],model$scores[,3]+0.15,labels = mydata$SMMA_Typology, col = 'gray', cex = 0.5)
text(model$scores[,1],model$scores[,3]-0.15,labels = mydata$SMMA_Abbreviated_Name, col = 'gray', cex = 0.5)




                                             



















# The other benefit of running a PCA is that we can reconstruct our X matrix and strip out 
# the variation of any dimension we were not interested in.
#For example, if we only wanted to build a k-means on the xmatrix without the variation associated with
# PC2, we can just take the corss product of the scores and loadings and exclude PC2!
xnew<-model$scores[,c(1,3)]%*%t(model$loadings[,c(1,3)])

# now rebuild the k-means clustering
expl<-NULL
#e1<-NULL
# build for loop to identify best number of clusters
for (i in 1:20) {
  kmeansmodel<-kmeans(xnew,i)
  #e1[i]<-kmeansmodel$betweenss
  expl[i]<- kmeansmodel$betweenss / kmeansmodel$totss
}
plot(expl, type = "l",main = "Between SS as a proportion of total SS", las=2)
lines(diff(expl),col = "blue")

# the above shows that the explained variance starts to lvel off after about 5 clusters.
kmeansmodel<-kmeans(xnew,5)

#get the clusters and add them to the dataset
mydata$cluster<-kmeansmodel$cluster

# now aggregate the data by cluster to get the average numers of students in each year for each cluster
round(aggregate(modeldata,by=mydata["cluster"],FUN = mean))
#count how many schools are in each cluster:
table(mydata$cluster)

#check the mix of schools in each cluster
table(mydata$SMMA_Typology, mydata$cluster)

# replot the pca but colour by cluster
par(xpd=TRUE)
plot(model$scores[,1],model$scores[,3], col = mydata$cluster, pch = 19)
legend(-2,4,legend = sort(unique(mydata$cluster)), fill =  c(1:6), cex=0.8)


