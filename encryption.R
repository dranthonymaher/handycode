strtoi(c("0xff", "077", "123"))

library(digest)

digest("The quick brown fox jumps over the lazy dog", algo = "crc32")
16^8

mylist<-as.list(round(10000000*runif(25000000)))
mylist1<-digest(mylist[1:10], algo = "crc32")


digest(c("a","b"), algo = "crc32")
