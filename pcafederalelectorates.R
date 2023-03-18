rm(list = ls())
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents/")

# read in the data
mydata<-read.csv("dsselectoratedatamarch2014flat.csv")

colnames(mydata)

model<-princomp(mydata[,4:31])

summary(model)

# check the proportion of variance explained
PoV <- model$sdev^2/sum(model$sdev^2)
Cumprov<-cumsum(PoV)
plot(PoV, type = "l", ylim=c(0,1))
lines(Cumprov, col = 'blue')


i=2
j=3
#plot the PCA scores
plot(model$scores[,i],model$scores[,j], col = mydata$party2,pch=19, lwd = 4)
#annotate with school type
text(model$scores[,i],model$scores[,j], labels = paste0(mydata$party,"_",mydata$commonwealth_electoral_divisio00), cex = 0.75)
#check the loadings
par(xpd=TRUE)
barplot(model$loadings[,i], main = paste0("PC",i),cex.names =  1, las =2)
barplot(model$loadings[,j], main = paste0("PC",j),cex.names =  1, las =2)
dev.off()

# make labels and margins smaller
par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
par(fig=c(0.1,0.7,0.25,0.95))
plot(model$scores[,i],model$scores[,j], xlab = paste0("PC",i),ylab = paste0("PC",j))
#annotate with school type
text(model$scores[,i],model$scores[,j], labels = mydata$commonwealth_electoral_divisio00)
# define area for the histogram
par(fig=c(0.1,0.7,0.0,0.2))
barplot(model$loadings[,i], main = paste0("PC",i),cex.names =  1, las =2)
# define area for the boxplot
par(fig=c(0.85,1,0.2,1), new=TRUE)
barplot(model$loadings[,j], main = paste0("PC",j),cex.names =  1, las =2, horiz = TRUE)
# # define area for the stripchart
# par(fig=c(0.1,0.67,0.1,0.25), new=TRUE)
# stripchart(Temperature, method="jitter")

