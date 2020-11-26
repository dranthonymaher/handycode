
library(gbm)
set.seed(123)

# set number of samples, n
n<-200

# create education dataset - want to try to predict score on test
#student info
iq<-rnorm(n,100,10)
studytime<-rpois(n,10)
seifa<-rnorm(n,100,10)
score<-iq*0.6+studytime*0.8-0.6*seifa+45+rnorm(n,0,2)
mean(score)
score<- ifelse(score<median(score),0,1)
df<-as.data.frame(cbind(iq,studytime, seifa,score))
plot(df)

# build the first tree
ntrees<-1
model<-gbm(score~iq+studytime+seifa,
           data = df,   n.trees = ntrees,
           interaction.depth =2, n.minobsinnode = 5, shrinkage = 0.1,
           bag.fraction = 0.8, train.fraction = 0.8, cv.folds = 0, verbose = TRUE, keep.data=TRUE)
# Check performance using the 50% heldout test set
best.iter <- gbm.perf(model, method = "test")
print(best.iter)
gg<-pretty.gbm.tree(model, i.tree = 1)
print(pretty.gbm.tree(model, i.tree = 1))
print(pretty.gbm.tree(model, i.tree = 2))

# build trees
for (i in 1:10) {
  Yhat <- predict(model, newdata = df, n.trees =  model$n.trees, type = "response")
  # calculate least squares error
  lse<-sum((df$score - Yhat)^2)
  # plot loop
  par(mfrow = c(2,3))
  for (i in 1:3){
    plot(df[,i],df$score, main = paste0("LSE = ",as.character(round(lse))), cex =  20*(df$score - Yhat)^2, ylab = "score", xlab = colnames(df)[i])
    text(df[,i],df$score, 1:length(df$studytime))
    p1<-plot(model, i.var = i, n.trees = model$n.trees,return.grid=TRUE, type = "response")
    points(df[,i],Yhat, col = "orange", pch = 8)
    lines(p1, lwd = 3)
  }
  df$Yhat<-Yhat
  # plot the residuals 
  #
  plot(df$Yhat, ylim = c(0,1), pch = 8, main = paste0(as.character(model$n.trees)," trees"))
  points(df$score, col = "red", cex = 20*(df$score - Yhat)^2)
  text(1:length(df$score),df$score, 1:length(df$studytime))
  
  #plot(df$Yhat, df$score-df$Yhat)
  
  model <- gbm.more(model, n.new.trees = 1, verbose = FALSE)
  Sys.sleep(0.1)
}
getwd()




