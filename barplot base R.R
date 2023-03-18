
# generate data

# categorical variables
ncat<-c("hosp","year","therapeuticarea")

hosp<-c("RPA","Prince of Wales","Westmead","RNS")

tarea<-c("cancer","othapedics","COPD","diabetes")
year<-c(2017:2022)
df<-as.data.frame(expand.grid(hosp,year,tarea))

# metrics
df$metric1<-rnorm(nrow(df),100,3)
df$metric2<-rpois(nrow(df),10)
df$metric3<-rnorm(nrow(df),100,3)
df$metric4<-rbinom(nrow(df),1,0.5)
plot(df)

names(df)<-c(ncat,"m1","m2","m3","m4")
# write.csv(df,"thisisdf.csv")


###### 1-way barplot
myvar<-"hosp"
result<-table(df[myvar])

commpallet<-c("#0a4a63","#00a8dc","#1278a2")

dev.off()
par(mar = c(2, 3, 0, 0))
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            
)
grid(NA,NULL, lwd=0.5,lty = 1, col = "gray") 
b1<-barplot(result, ylim = c(0,max(result)*1.1),
            xlab="",ylab = "Number of seps",names.arg = ""
            ,col = commpallet
            ,xaxt = "n" , yaxt = "n"
            ,add=TRUE
)
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue')
text(b1,-0.2,names(result),cex =1,srt = 0,pos=1,col='black', xpd=NA)
dev.copy(png,paste0("thispuic_",myvar,".png"),width = 1.1*450, height = 0.9*350);dev.off()


# 2-way barplot
# combine the explicit and implicit with clinical judgement
result<-table(mydatasm3$mra1_23,mydatasm3$explicit)
b1<-barplot(result,beside = TRUE,col = commpallet)
text(b1,result+max(result)*0.05,paste0(as.character(result)),col = 'blue', xpd=NA)
text(b1,-0.2,rownames(result),cex =1,srt = 0,pos=1,col='black', xpd=NA)
dev.copy(png,paste0(dfigs,"explicitvclinjudge",".png"));dev.off()

