myfile<-readLines("myjson.txt")

library(jsonlite)

mytable<-fromJSON(myfile)

flatten(as.data.frame(mytable))

