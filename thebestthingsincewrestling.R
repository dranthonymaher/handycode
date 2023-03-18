
# create the initial x variable
x1 <- rnorm(100, 15, 5)

# x2, x3, and x4 in a matrix, these will be modified to meet the criteria
x234 <- scale(matrix( rnorm(300), ncol=3 ))

# put all into 1 matrix for simplicity
x1234 <- cbind(scale(x1),x234)

# find the current correlation matrix
c1 <- var(x1234)
cor(x1234)
# cholesky decomposition to get independence
chol1 <- solve(chol(c1))

newx <-  x1234 %*% chol1 

# check that we have independence and x1 unchanged
zapsmall(cor(newx))
all.equal( x1234[,1], newx[,1] )

# create new correlation structure (zeros can be replaced with other r vals)
newc <- matrix( 
  c(1  , 0.4, 0.5, 0.6, 
    0.4, 1  , 0  , 0  ,
    0.5, 0  , 1  , 0  ,
    0.6, 0  , 0  , 1  ), ncol=4 )

# check that it is positive definite
eigen(newc)

chol2 <- chol(newc)

finalx <- newx %*% chol2 * sd(x1) + mean(x1)

# verify success
mean(x1)
colMeans(finalx)

sd(x1)
apply(finalx, 2, sd)

zapsmall(cor(finalx))
pairs(finalx)

all.equal(x1, finalx[,1])

r1<-finalx[,1]*-0.4+finalx[,2]*0.24+finalx[,3]*0.04+finalx[,4]*-1.4+rnorm(100)*0.01
head(finalx)
colnames(finalx)<-LETTERS[1:4]
finalx1<-NULL
finalx1<-cbind(as.data.frame(finalx),rnorm(100, 15, 5))
colnames(finalx1)<-LETTERS[1:5]
model<-glm(r1~A+B+C+D+E,data=finalx1)
#interesting
summary(model)
plot(r1~E,data=finalx1)
pairs(finalx1)
