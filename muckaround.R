# cubic splines












data <- replicate(100, runif(n=20))
matplot(data, type = "l")
?runif

means <- colMeans(data)

?sample

?data.frame
data.frame(LETTERS[1:3],)
beta = 0
b1=0.1
b2=0.2
L3 <- LETTERS[1:3]
n<-10
d <- data.frame(x = sample(L3,n , replace = TRUE),
                y = sample(c(1:5),n, replace = TRUE), runif(n))
response0<-beta+b1*d$x
