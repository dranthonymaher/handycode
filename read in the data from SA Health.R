
library(xlsx)

pathofinterest<-"C:/Users/amaher2/KPMG/AU - SA Health Workforce Plan - General/03. Workforce Analysis Phase/Data/Raw data from client/SHARP data"

myfile<- "SA Health Hours by FTE Category KPMG Apr 20"


#read teh data

datafile<-read.csv(paste0(pathofinterest,"/",myfile,".csv"))

head(datafile)



