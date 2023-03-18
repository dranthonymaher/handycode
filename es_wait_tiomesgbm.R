




# try fitting a gbm
mygbm<-gbm(data = mydata,formula = myformula, distribution = "gaussian",
           n.trees = 1000,
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

mydata$prob = predict.gbm(mygbm, mydata, best.iter, "response")
# plot the result
plot(mydata$Median_wait_time.days., mydata$prob)
abline(0, 1) #add a 45 degree line


# try a random forest
myformula <- as.formula(Median_wait_time.days.~ 
                          reporting_end_date
                        +peer_group_name
                        +surgeon_speciality_count_x
                        +procs_count_x
                        +Surgeon_speciality_avg
                        +Procs_count_avg
                        +surgeon_ratio
                        +procs_ratio
)
myrf = randomForest(myformula, data=mydata, ntree=1000,  importance=TRUE, na.action = na.omit)
print(myrf)
round(importance(myrf), 2)

