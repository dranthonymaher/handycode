df<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/CDO Team Clustering  (1-14).csv")

# exclude test data from upper rows
df<-df[df$Please.enter.your.name!="",]
summary(df)
# set NAs to average for that column
colnames(df)
numericdata<-df[,8:27]
colmea<-colMeans(numericdata, na.rm = TRUE)
length(colmea)
dim(numericdata)
# Clustering
# first replace NAs with averages
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
numericdata<-df[,8:27]

for (i in 1:length(numericdata[1,])){
png(filename = paste0("mypic",i,".png"),width = 1.5*800, height = 1.5*450)
h1=hist(numericdata[,i], main = colnames(numericdata)[i], xlab =colnames(numericdata)[i]
     ,col = "cyan",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlim = c(min(numericdata[,i])*1.2,max(numericdata[,i])*1.2))
text(numericdata[,i],runif(length(numericdata[,i]),0,0.5*max(h1$counts)),df[,7], srt=45, pos=4, cex = 1.5)
dev.off()
}

# Clustering
# first replace NAs with averages
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
numericdata<-df[,8:27]

# build PCA model
model<- prcomp(numericdata, scale. = TRUE)
summary(model)

model$x

#plot the PCA scores
# plot(model$x[,1],model$x[,2])
# #annotate with school type
# text(model$x[,1],model$x[,2], labels = df$Please.assign.yourself.a.nom.de.plume.for.this.survey.)
# dev.copy(png,"pcascores.png"); dev.off()
# 
# plot(model$x[,1],model$x[,2])
# #annotate with school type
# text(model$x[,1],model$x[,2], labels = df$Please.enter.your.name)
# dev.copy(png,"pcascoresrealnames.png"); dev.off()
# arrows(0,0,model$rotation[,1]*5,model$rotation[,2]*5, col = "gray")
# text(model$rotation[,1]*5,model$rotation[,2]*5, labels = colnames(numericdata),col = "gray", cex = 0.75)
# dev.copy(png,"pcascoresrealnamesandloads.png"); dev.off()




# do the kmeans on the first 3 pcs only

# k-means clustering
expl<-NULL
#e1<-NULL
# build for loop to identify best number of clusters
for (i in 1:10) {
  kmeansmodel<-kmeans(model$x[,1:2],i)
  #e1[i]<-kmeansmodel$betweenss
  expl[i]<- kmeansmodel$betweenss / kmeansmodel$totss
}
plot(expl, type = "l",main = "Between SS as a proportion of total SS", las=2)
lines(diff(expl),col = "blue")

# the above shows that the explained variance starts to lvel off after about k clusters.
k<-3
kmeansmodel<-kmeans(numericdata,k)

#get the clusters and add them to the dataset
numericdata$cluster<-kmeansmodel$cluster

# now aggregate the data by cluster to get the average numers of students in each year for each cluster
round(aggregate(numericdata,by=numericdata["cluster"],FUN = mean))
#count how many schools are in each cluster:
table(numericdata$cluster)


# now colour the PCA scores by cluster
plot(model$x[,1],model$x[,2], xlab = "PC1",ylab = "PC2",pch=16,cex=10,col = "grey")
dev.copy(png,"pcascoresraw.png",width = 1.5*800, height = 1.5*450); dev.off()

for (i in 1:k){
  print(i)
points(model$x[numericdata$cluster==i,1],model$x[numericdata$cluster==i,2],col = i+1,cex = 10, pch=16)
}
dev.copy(png,"pcascores_clustered.png",width = 1.5*800, height = 1.5*450); dev.off()

#annotate with nom de plumes
text(model$x[,1],model$x[,2], labels = df$Please.assign.yourself.a.nom.de.plume.for.this.survey., cex=1.5)
dev.copy(png,"pcascores_nomdeplumes.png",width = 1.5*800, height = 1.5*450); dev.off()



# now colour the PCA scores by cluster and name with real name
plot(model$x[,1],model$x[,2], xlab = "PC1",ylab = "PC2")
#dev.copy(png,"pcascoresraw.png"); dev.off()

for (i in 1:k){
  print(i)
  points(model$x[numericdata$cluster==i,1],model$x[numericdata$cluster==i,2],col = i+1,cex = 10, pch=16)
}
#dev.copy(png,"pcascores_clustered.png"); dev.off()

#annotate with nom de plumes
text(model$x[,1],model$x[,2], labels = df$Please.enter.your.name,cex=1.5)
dev.copy(png,"pcascores_realnamess.png",width = 1.5*800, height = 1.5*450); dev.off()

arrows(0,0,model$rotation[,1]*5,model$rotation[,2]*5, col = "gray")
text(model$rotation[,1]*5,model$rotation[,2]*5, labels = colnames(numericdata),col = "gray", cex = 0.75)
dev.copy(png,"pcascoresrealnamesandloads.png",width = 1.5*800, height = 1.5*450); dev.off()

