setwd("/Users/anthonymaher/Documents/R stuff")
getwd()
rm(list=ls())
#import the data 
mydata <- read.csv("playgolf2.csv")
summ <- summary(mydata)

#step 1: calculate entropy of the target

#extract the target and predictors
target <- mydata[, 5]
predictors <- mydata[, -5]
len1<-length(predictors)

#count up number of levels in the target
r1 <- table(target)
#normalise r1
r2 <- r1/sum(r1)
#now entropy is just
entvec <- r2 * log(r2, 2)
ent <- sum(entvec) * -1
ent


#step 2: get the information gain from each vector
varvec <- NULL
valvarvec <- NULL

#for (j in 1:len1) {
	j<-1
	#2a. get probabilities
	ent1 <- NULL
	for (i in 1:length(predictors)) {
		p1 <- table(predictors[, i])/sum(table(predictors[, i]))

		#2b. get entropies
		t2 <- table(predictors[, i], target)
		#normalise it
		t3 <- t2/rowSums(t2)
		#get the logs
		t3logs <- log(t3, 2)
		#convert inf to 0
		t3logs[is.infinite(t3logs)] <- 0
		t4 <- -(t3 * t3logs)
		#get the row sums
		t5 <- rowSums(t4)
		#finally compute the entropy
		ent1[i] <- sum(p1 * t5)
	}

	#now compute the information gain
	igain <- ent - ent1
	igain
	# and find which is maximum
	maxi <- which.max(igain)
	# and save both to  new ordered vectors
	varvec[j] <- names(predictors)[maxi]
	#use this to define our threshold
	v1<-max(igain)
	valvarvec[j] <- v1

	#now remove that one from the data
	predictors <- predictors[, -maxi]
	j<-j+1
	
#	len <- length(predictors)
#}

