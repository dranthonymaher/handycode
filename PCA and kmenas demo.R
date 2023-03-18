# Script to demo how to use PCA and k-means clustering in R
# Anthony Maher 13/3/20

dev.off()
# remove all objects
rm(list = ls(all.names = TRUE))



#  import the data from Boston area schools data
# these data come from:
#  https://data.boston.gov/dataset/buildbps-facilities-and-educational-data-for-boston-public-schools
mydata<-NULL
mydata<-read.csv("https://raw.githubusercontent.com/dranthonymaher/handycode/master/buildbps.csv")

# check the colnames - which are the ones that tell us how many kids in each year?
cn<-colnames(mydata)
cn[1:50]
# output should be:
# [1] "SMMA_Identifier"          "SMMA_Only_For_Map"        "BPS_School_Name"          "BPS_Historical_Name"     
# [5] "SMMA_Abbreviated_Name"    "BPS_Address"              "BRA_Neighborhood"         "SMMA_latitude"           
# [9] "SMMA_longitude"           "SMMA_Typology"            "SMMA_Educational_Program" "BPS_Grades_Offered"      
# [13] "BPS_Property_Status"      "BPS_Total_GSF"            "BPS_GSF"                  "SMMA_Site_SF"            
# [17] "SMMA_Site_Acres"          "BPS_Year_Founded"         "BPS_Year_Built"           "MSBA_Year_Reno"          
# [21] "SMMA_Era"                 "BPS_Plans"                "BPS_Principal"            "BPS_Open"                
# [25] "BPS_Close"                "DOE_ID"                   "DOE_Total"                "DOE_PK"                  
# [29] "DOE_K"                    "DOE_1"                    "DOE_2"                    "DOE_3"                   
# [33] "DOE_4"                    "DOE_5"                    "DOE_6"                    "DOE_7"                   
# [37] "DOE_8"                    "DOE_9"                    "DOE_10"                   "DOE_11"                  
# [41] "DOE_12"                   "DOE_SP"                   "GSF...Student.Value"      "BPS_KWH"                 
# [45] "BPS_Electric_Bill"        "BPS_Therms"               "BPS_Gas_Bill"             "BPS_Water_ft_cubic"      
# [49] "BPS_Water_Bill"           "BPS_Total_Energy_Cost"   

# so get cols 28-42 - this is our model file for PCA
modeldata<-mydata[,28:42]

# remove rows without complete cases from both data files
mydata<-mydata[complete.cases(modeldata),]
modeldata<-modeldata[complete.cases(modeldata),]


# build PCA model
model<- princomp(modeldata, cor = TRUE)
summary(model)

# check the proportion of variance explained
PoV <- model$sdev^2/sum(model$sdev^2)
Cumprov<-cumsum(PoV)
plot(PoV, type = "l", ylim=c(0,1))
lines(Cumprov, col = 'blue')
#3 PCs capture over 80% of variation

#plot the PCA scores
plot(model$scores[,1],model$scores[,2])
#annotate with school type
text(model$scores[,1],model$scores[,2], labels = mydata$SMMA_Typology)

# the above is a bit cluttered - let's annotate with a colour code

#first check the number of levels for the school type
unique(mydata$SMMA_Typology)
# build a new variable and assign eah factor level a unique number
mydata$schooltypenum<-as.numeric(as.factor(mydata$SMMA_Typology))
table(mydata$SMMA_Typology, mydata$schooltypenum)
#re-plot the PCA
plot(model$scores[,1],model$scores[,2], col = mydata$schooltypenum)
legend(-5,8,legend = sort(unique(mydata$SMMA_Typology)), fill =  c(1:6), cex=0.8)



#check the loadings
par(mfrow=c(2,2))
par(xpd=TRUE)
barplot(model$loadings[,1], main = "PC1")
barplot(model$loadings[,2], main = "PC2")
barplot(model$loadings[,3], main = "PC3")
barplot(model$loadings[,4], main = "PC4")


# the loadings suggest the variation in PC1 and 3 is the most "interesting"
par(fig=c(0.1,0.5,0.1,0.9))
par(xpd=TRUE)
#plot the PCA scores for PC1 v PC3
plot(model$scores[,1],model$scores[,3], col = mydata$schooltypenum, pch = 19)
legend(-2,6.5,legend = sort(unique(mydata$SMMA_Typology)), fill =  c(1:6), cex=0.8)
par(fig=c(0.5,0.9,0.1,0.5), new=TRUE)
barplot(model$loadings[,3],ylab="PC3", cex.names =   0.5, las = 2)
par(fig=c(0.5,0.9,0.5,0.9), new=TRUE)
barplot(model$loadings[,1],ylab="PC1", cex.names =   0.5, las = 2)
dev.off()



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

# the above shows that the explained variance starts to lvel off after about 5 clusters.
kmeansmodel<-kmeans(modeldata,5)

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


