#as.matrix(reshape(rnorm(9),c(3,3)))



## multiple id variables
df3 <- data.frame(school = rep(1:3, each = 4), class = rep(9:10, 6),
                  time = rep(c(1,1,2,2), 3), score = rnorm(12)^2)
wide <- reshape(df3, idvar = c("school","class"), direction = "wide")

## transform back
reshape(wide)

barplot(as.matrix(wide[,3:4]))
?barplot
