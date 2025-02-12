


# what this requres:
# a data frame (model file) with categorical variables (factors)
# and a response variable of 0s and 1s. This will plot chart for a given
# categorical variable "myvar", and overlay the average response "r1"
# from datafram "df"
# you just need the naems of the columns and it finds the indexes

cn<-colnames(rawdata)
cn
# set the column names of interest
myvar<-"Gender"
r1<-"QOESAT"

cvarind<-which(colnames(rawdata)==myvar)
rvarind<-which(colnames(rawdata)==r1)

result<-table(rawdata[,cvarind])

dev.off()
par(mar = c(5, 3, 0, 0))
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of claims",names.arg = ""
            
)
grid(NA,NULL, lwd=0.5,lty = 1, col = "gray") 
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of claims",names.arg = ""
            ,col = 'gray'#commpallet
            ,xaxt = "n" , yaxt = "n"
            ,add=TRUE
)
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'gray')
text(b1,-0.02*max(result),names(result),cex =1,srt = 90,pos=2,col='red', xpd=NA)

a1mean<-aggregate(rawdata[,rvarind], by=list(rawdata[,cvarind]), FUN=mean, na.rm = TRUE)
a1mean
lines(x = b1,y = 0.5*a1mean$x/max(a1mean$x)*max(result), col = 'blue', lwd = 2)
text(x = b1,y = 0.5*a1mean$x/max(a1mean$x)*max(result),round(100*a1mean$x)/100, col = 'blue',pos = 3)

#dev.copy(png,paste0("thispuic_",myvar,".png"),width = 1.1*450, height = 0.9*350);dev.off()