require(gbm)
require(MASS)#package with the boston housing dataset
#library(gbm)
#separating training and test data
train=sample(1:506,size=374)

Boston.boost=gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                  shrinkage = 0.001, interaction.depth = 3, verbose=TRUE)
Boston.boost

summary(Boston.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
#Plot of Response variable with lstat variable
plot(Boston.boost,i="lstat") 
#Inverse relation with lstat variable
best.iter <- gbm.perf(Boston.boost, method = "OOB")
print(best.iter)
best.iter <- gbm.perf(Boston.boost, method = "test")
print(best.iter)
par(mfrow = c(1, 2))
summary(Boston.boost, n.trees = 1)          # using first tree
summary(Boston.boost, n.trees = best.iter)  # using estimated best number of trees
print(pretty.gbm.tree(Boston.boost, i.tree = 1))
print(pretty.gbm.tree(Boston.boost, i.tree = Boston.boost$n.trees))



plot(Boston.boost,i="rm") 
#as the average number of rooms increases the the price increases

plot.gbm(Boston.boost) 

n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(Boston.boost,Boston[-train,],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(Boston[-train,],apply( (predmatrix-medv)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.err),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)

