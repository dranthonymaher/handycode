uv<-function(a33){

mca<-t(t(a33)/sd(a33))

return(mca)
}