
#which is greatest?
colnames(groupB)
for (i in 1:length(groupB[,])){
  groupB$maxs3[i]<-names(which.max(groupB[i,c(10:12)]))
}