wordlist<-read.delim("https://raw.githubusercontent.com/charlesreid1/five-letter-words/master/sgb-words.txt")

strsplit("react","")
character_array <- unlist(strsplit(paste(wordlist), ""))
paste(wordlist)
as.character(wordlist)

barplot(table(unlist(strsplit(paste(as.vector(t(wordlist))),""))))
barplot(sort(table(unlist(strsplit(paste(as.vector(t(wordlist))),""))), decreasing = TRUE))

mclp<-NULL

i<-5
letterpos<-substr(wordlist[,1],i,i)
barplot(table(letterpos),main = paste0("most common letter in position ",i))
mclp[i]<-names(which.max(table(letterpos)))

mclp

intersect(c("h","h"),c("h","o"))
i<-1
w1<-strsplit(paste(as.vector(t(wordlist)[i])),"")
i<-2
w2<-strsplit(paste(as.vector(t(wordlist)[i])),"")
w1
w2
intersect(unlist(w1),unlist(w2))

