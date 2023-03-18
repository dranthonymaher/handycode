

med_map$SPECIALITY_CODE<-as.numeric(med_map$SPECIALITY_CODE)

data2<-med_map[order(med_map$Specialty,med_map$SPECIALITY_CODE),]

# need to use "by" and "order"
x1<-as.vector(by(order(data2$Specialty,data2$SPECIALITY_CODE),data2$Specialty,min, simplify=TRUE))


data3<-data2[x1,]



# patient flow modelling
rep("a",100)


inter = rexp(15, 1) 
arr = cumsum(inter)
one = rep(1, times = length(arr))
plot(arr, one, yaxt = 'n', ann=FALSE)
