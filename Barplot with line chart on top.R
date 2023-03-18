# barplot with line chart over the top

par(mfrow = c(1,2))
df <- data.frame(stolpec1 = 10 * runif(10), stolpec2 = 30 * runif(10))

#dont do it this way:
barplot(df$stolpec1)
lines(df$stolpec2/10) #implicitno x = 1:10
points(df$stolpec2/10)

# instead do it this way
df.bar <- barplot(df$stolpec1)
lines(x = df.bar, y = df$stolpec2/10)
points(x = df.bar, y = df$stolpec2/10)

