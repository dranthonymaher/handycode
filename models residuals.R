
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


library(gbm)
set.seed(123)

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


for (i in 1:10) {
  Yhat <- predict(model, newdata = df, n.trees =  model$n.trees, type = "response")
  # least squares error
  lse<-sum((df$score - Yhat)^2)
  #1
  par(mfrow = c(2,3))
  for (i in 1:3){
    plot(df[,i],df$score, main = paste0("LSE = ",as.character(round(lse))), cex =  20*(df$score - Yhat)^2, ylab = "score", xlab = colnames(df)[i])
    text(df[,i],df$score, 1:length(df$studytime))
    p1<-plot(model, i.var = i, n.trees = model$n.trees,return.grid=TRUE, type = "response")
    points(df[,i],Yhat, col = "orange", pch = 8)
    lines(p1, lwd = 3)
  }
  df$Yhat<-Yhat
  #df<-df[order(df$Yhat),]
  plot(df$Yhat, ylim = c(0,1), pch = 8, main = paste0(as.character(model$n.trees)," trees"))
  points(df$score, col = "red", cex = 20*(df$score - Yhat)^2)
  text(1:length(df$score),df$score, 1:length(df$studytime))
  
  #plot(df$Yhat, df$score-df$Yhat)
  
  model <- gbm.more(model, n.new.trees = 1, verbose = FALSE)
  Sys.sleep(0.1)
}
getwd()
dev.off()
plot(df$Yhat, df$score-df$Yhat)



# introduce a corelation


x75<-rnorm()
N <- 200 # Number of random samples
set.seed(123)
# Target parameters for univariate normal distributions
rho <- -0.6
mu1 <- 1; s1 <- 2
mu2 <- 1; s2 <- 8
rbvn<-function (n, m1, s1, m2, s2, rho)
{
  X1 <- rnorm(n, mu1, s1)
  X2 <- rnorm(n, mu2 + (s2/s1) * rho *
                (X1 - mu1), sqrt((1 - rho^2)*s2^2))
  cbind(X1, X2)
}

bvn3 <- rbvn(N,mu1,s1,mu2,s2,rho)
plot(bvn3)
colnames(bvn3) <- c("bvn3_X1","bvn3_X2")
