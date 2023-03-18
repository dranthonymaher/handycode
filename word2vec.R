getwd()
setwd("C:/Users/amaher2/OneDrive - KPMG/Documents")
df<-read.csv("pruned.word2vec.csv")

mat1<-(df[,c(2:301)])
plot(as.numeric(mat1[1,]),as.numeric(mat1[2,]))

model<-princomp(mat1)

# plot(model$scores[,1],model$scores[,2])
# text(model$scores[,1],model$scores[,2],df[,1])

library("scatterD3")
x <- c(33, 33, 43, 55, 48, 37, 43, 24)
y <- c(37, 38, 42, 46, 46, 59, 41, 50)
library(lsa)
cosine(x, y)
cor(x,y)
plot(x,y)


# plot zoomable scatter plot
scatterD3(model$scores[,1],model$scores[,2],
          lab = df[,1]
          ,ylim = c(0.1,0.15)
          ,xlim = c(0.1,0.15))

summary(model)
dist(mat1[,1],mat1[,2])
cor(as.numeric(mat1[1,]),as.numeric(mat1[2,]))

wordlist<-df[,1]
corlist<-NULL
for (i in 1:43981){
corlist[i]<-cor(as.numeric(mat1[1,]),as.numeric(mat1[i,]))
}

plot(corlist)
text(corlist,wordlist[1:1000])

?dist
x <- matrix(rnorm(100), nrow = 5)
d1<-dist(mat1[,1:4])


dd <- as.dist((1 - cor(USJudgeRatings))/2)
round(1000 * dd) # (prints more nicely)
plot(hclust(dd)) # to see a dendrogram of clustered variables
