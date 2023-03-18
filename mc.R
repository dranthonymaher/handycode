mc<-function(a33){

mca<-t(t(a33)-colMeans(a33))

return(mca)
}