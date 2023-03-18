# Script to demo how to use PCA and k-means clustering in R
# Anthony Maher 13/3/20

dev.off()
# remove all objects
rm(list = ls(all.names = TRUE))


#set the working directory
#!!!- each user will need to change this for themselves to the directory that contains their data!!!
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")

#  import the data
mydata<-NULL
mydata<-read.csv("abs_data.csv")

# check the colnames - which are the ones that t?
cn<-colnames(mydata)


# so get cols 28-42 - this is our model file for PCA
modeldata<-mydata[,5:460]

# remove rows without complete cases from both data files
mydata<-mydata[complete.cases(modeldata),]
modeldata<-modeldata[complete.cases(modeldata),]


# build PCA model
model<- princomp(modeldata, cor=TRUE)
summary(model)

output<-cbind(mydata[,c(1,2,3)],model$scores[,c(1:10)])
write.csv(output,"absdatapcascores.csv")


# check the proportion of variance explained
PoV <- model$sdev^2/sum(model$sdev^2)
Cumprov<-cumsum(PoV)
plot(PoV, type = "l", ylim=c(0,1))
lines(Cumprov, col = 'blue')
#3 PCs capture over xx% of variation

mydata$state.LGA<-paste0(mydata$state,"_",mydata$Label_x)

#clean the names - remove everythging in the brackets
mydata$Label_x<-gsub("\\s*\\([^\\)]+\\)","",mydata$Label_x)

loads<-as.data.frame(model$loadings[,2:3])

#plotting
library(scatterD3)


i=2
j=3
# create zoomable scatter plot
scatterD3(x=model$scores[,i],y=model$scores[,j],lab =  mydata$Label_x ,col_var = mydata$state
          ,
          point_opacity = 1)

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




#check some schools
#compare Quincy Upper with Curley Upper
mydata[mydata$SMMA_Abbreviated_Name=="Quincy Upper",28:42]
mydata[mydata$SMMA_Abbreviated_Name=="Curley Upper",28:42]
# Curley upper is labelled a K-8 but it's really a middle school
mydata[mydata$SMMA_Abbreviated_Name=="BLS",28:42] # big high schools
mydata[mydata$SMMA_Abbreviated_Name=="East Boston",28:42]
mydata[mydata$SMMA_Abbreviated_Name=="Umana",28:42] #K-* but lots of middle school enrolments
mydata[mydata$SMMA_Abbreviated_Name=="Quincy Lower",28:42] # large elementary schools
mydata[mydata$SMMA_Abbreviated_Name=="Hennigan*",28:42]

                                             




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


