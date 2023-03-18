#import the required packages
library(gbm)
library(ROCR)
library(randomForest)
library(ggplot2)


rm(list=ls())

# read in all opportunities
mydata<- read.csv("C:/Users/amaher2/KPMG/AU - Data Office - Model Maher-Antaki/Raw files and code/model_spin3.csv")
# check it
head(mydata)

# check the structure of all variables that they have been appropriately defined
str(mydata)


# Reporting end date has come through as interger - convert to factor
mydata$Remoteness.area..code.<-as.factor(mydata$Remoteness.area..code.)

# summarise all variables
summary(mydata)

# remove the rows for which data wasn't provided about median wait times
mydata<-mydata[is.na(mydata$next_year_wait_time)==FALSE | mydata$reporting_end_date==2019,]

# Define the training, test and hold out sets
xtrain<-mydata[mydata$reporting_end_date<2018,]
xtest<-mydata[mydata$reporting_end_date==2018,]
xholdout<-mydata[mydata$reporting_end_date>2018,]



# print the list of colnames
writeClipboard(colnames(xtrain))

# define the formula
myformula <- as.formula(next_year_wait_time~ 
                          #+	reporting_unit_name
                          +	reporting_end_date
                          +	peer_group_name
                          #+	reported_measure_name
                          #+	reporting_unit_type_code
                          #+	units_display
                          #+	reported_measure_category_name_x
                          #+	reported_measure_category_two_name
                          #+	Establishment.ID
                          +	State
                          #+	Hospital.name
                        #+	Medicare.provider.no.
                        #+	Local.Hospital.Network.code
                        #+	Local.Hospital.Network
                        #+	Address.Line.1
                        #+	Address.Line.2
                        #+	Remoteness.area..code.
                        +	Remoteness.area
                        +	Number_beds_available
                        #+	Peer.group.code
                     #  +	Peer.group.name
                        #+	Supplied.Establishments.data
                        #+	Supplied.Morbidity.data
                        #+	Supplied.Emergency.department.data
                        #+	Supplied.Elective.Surgery.Waiting.times.data
                        #+	Supplied.Non.admitted.patient.aggregate.data
                        +	IHPA.funding.designation
                        +	surgeon_speciality_count
                        +	procs_count
                        #+	Surgeon_speciality_avg
                        #+	Procs_count_avg
                      #  +	surgeon_ratio
                      #  +	procs_ratio
                        
)
myformula

# fit the model
model<-glm(myformula,data = xtrain, family = "gaussian")
#generate model summary output
summary(model)

# check the R^2
with(summary(model), 1 - deviance/null.deviance)
#  0.3944866

# check the prediction
xtest$prd.glm<-predict.glm(model,newdata = xtest, type = "response")
sumsq<-sum((xtest$prd.glm - xtest$next_year_wait_time)^2)
sumsq
# plot it to check
dev.off()
plot(xtest$next_year_wait_time,xtest$prd.glm,main =paste0( "GLM (Sumsq=",round(sumsq),")"))
abline(0, 1) #add a 45 degree line


# GBM and RF --------------------------------------------------------------


# try fitting a gbm
mygbm<-gbm(data = xtrain,formula = myformula, distribution = "gaussian",
           n.trees = 2500,
           interaction.depth = 2, n.minobsinnode = 20, shrinkage = 0.05,
           bag.fraction = 0.5, train.fraction = 0.5, cv.folds = 0,
           keep.data = TRUE, verbose = TRUE)
summary(mygbm)

best.iter <-
  gbm.perf(mygbm,
           plot.it = TRUE ,
           oobag.curve = FALSE,
           method = "test")
best.iter


#Determine predictions for the ScoringData
# check the prediction
xtest$prd.gbm<-predict.gbm(mygbm,newdata = xtest, type = "response")
sumsq<-sum((xtest$prd.gbm - xtest$next_year_wait_time)^2)
sumsq
# plot it to check
plot(xtest$next_year_wait_time,xtest$prd.gbm,main =paste0( "GBM (Sumsq=",round(sumsq),")"))
abline(0, 1) #add a 45 degree line



# try a random forest
myrf = randomForest(myformula, data=xtrain, ntree=1000,  importance=TRUE, na.action = na.omit)
print(myrf)
myrfimp<-as.data.frame(round(importance(myrf),2))
names(myrfimp)<-c("IncMSE","IncNodePurity")
rfoutput<-myrfimp[with(myrfimp,order(-IncMSE)),]
write.csv(rfoutput,"rfoutput.csv")
rfoutput

# plot(mydata$Number_beds_available, mydata$next_year_wait_time)
# lm(next_year_wait_time~Number_beds_available, data = mydata)

#Determine predictions for the ScoringData
# check the prediction
xtest$prd.rf<-predict(myrf,newdata = xtest, type = "response")

# plot it to check
plot(xtest$next_year_wait_time,xtest$prd.rf,main =paste0( "RF (Sumsq=",round(sumsq),")"))
abline(0, 1) #add a 45 degree line
text(xtest$next_year_wait_time,xtest$prd.rf,xtest$reporting_unit_name,cex=0.5, pos=2)

#par(mfrow = c(2,2))
#myvar<-"prd.glm"
plotmodelpred<-function(myvar){
  
  
  sumsq<-sum((xtest[,myvar] - xtest$next_year_wait_time)^2)
  sumsq
  # plot it to check
  plot(xtest[,"next_year_wait_time"],xtest[,myvar]
       ,xlab = "Actual wait time"
       ,ylab = "predicted wait time"
       ,main =paste0( myvar," (Sumsq=",round(sumsq),")"))
  abline(0, 1) #add a 45 degree line
  dev.copy(png,paste0(myvar,"_plot.png"))
  dev.off()
}
plotmodelpred("prd.glm")
plotmodelpred("prd.gbm")
plotmodelpred("prd.rf")



i<-11
partialPlot(myrf,xtrain,colnames(xtrain)[i], main = colnames(xtrain)[i], las = 2, cex.names = 0.75)
i<-19
partialPlot(myrf,xtrain,colnames(xtrain)[i], main = colnames(xtrain)[i], las = 2, cex.names = 0.75)
i<-20
partialPlot(myrf,xtrain,colnames(xtrain)[i], main = colnames(xtrain)[i], las = 2, cex.names = 0.75)
i<-28
partialPlot(myrf,xtrain,colnames(xtrain)[i], main = colnames(xtrain)[i], las = 2, cex.names = 0.75)
i<-29
partialPlot(myrf,xtrain,colnames(xtrain)[i], main = colnames(xtrain)[i], las = 2, cex.names = 0.75)
i<-30
partialPlot(myrf,xtrain,colnames(xtrain)[i], main = colnames(xtrain)[i], las = 2, cex.names = 0.75)



# get the prediction for next year
xholdout$prd.rf<-predict(myrf,newdata = xholdout, type = "response")

source('plotfactor.R')
plotfactor(19,xholdout,"prd.rf")

# replace those values with the predictions
xholdout$next_year_wait_time<-xholdout$prd.rf

mycols<-colnames(xtrain)

newoutput<-rbind(xtrain[,mycols], xtest[,mycols], xholdout[,mycols])

#export it
write.csv(newoutput,"newoutput.csv")

