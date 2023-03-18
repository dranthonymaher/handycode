# Script to demo how to use PCA and k-means clustering in R
# Anthony Maher 13/3/20

dev.off()
# remove all objects
rm(list = ls(all.names = TRUE))


#set the working directory
#!!!- each user will need to change this for themselves to the directory that contains their data!!!
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")

#  import the data

mydata<-read.csv("NESA data.csv")
#reshape
mydata2<-reshape(mydata[,c("SID","Subj2","Mark")],idvar = "SID",timevar = "Subj2",direction = "wide")

# so get cols  - this is our model file for PCA
modeldata<-mydata2[,2:5]


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
#2 PCs capture over xx% of variation


#clean the names - remove everythging in the brackets
# mydata$Label_x<-gsub("\\s*\\([^\\)]+\\)","",mydata$Label_x)

loads<-as.data.frame(model$loadings[,1:2])
barplot(model$loadings[,2])

#plotting
library(scatterD3)

# create zoomable scatter plot
scatterD3(x=model$scores[,1],y=model$scores[,2],col_var =   modeldata$Mark.Humanities1)

# create zoomable loadings plot
scatterD3(x=model$loadings[,1],y=model$loadings[,2],lab =  colnames(modeldata) ,
          point_opacity = 1)




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
kmeansmodel<-kmeans(modeldata,4)

#get the clusters and add them to the dataset
modeldata$cluster<-kmeansmodel$cluster

# now aggregate the data by cluster to get the average numers of students in each year for each cluster
round(aggregate(modeldata,by=modeldata["cluster"],FUN = mean))
#count how many schools are in each cluster:
table(mydata$cluster)

# create zoomable scatter plot
scatterD3(x=model$scores[,1],y=model$scores[,2],col_var = mydata$cluster
          ,
          point_opacity = 1)


write.csv(modeldata,"myclstr.csv")

#Read it in
m2<-read.csv("PathwaysSimulat.csv")
colnames(m2)
model1<-glm(MEDICINe~Mathematics+English+DidAH+DidMH+DidBio+DiDChem+DidPhys, data=m2)
summary(model1)
model2<-glm(LAW~Mathematics+English+DidAH+DidMH+DidBio+DiDChem+DidPhys, data=m2)
summary(model2)
model3<-glm(Engineering~Mathematics+English+DidAH+DidMH+DidBio+DiDChem+DidPhys, data=m2)
summary(model3)

hist(m2$MEDICINe)
hist(m2$LAW)
hist(m2$Engineering)

plot(m2[,c(18:20)])
