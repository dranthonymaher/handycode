



plotfactor <- function(i, mydata, r1){
  # get list of colnames to map to
  if (is.factor(mydata[,i])==TRUE){
    
    responsecol<-which(colnames(mydata)==r1)
    a1mean<-aggregate(mydata[,responsecol], list(mydata[,i]), FUN=mean, na.rm = TRUE)
    a1exp<-aggregate(mydata[,responsecol], list(mydata[,i]), FUN=length)
    mycol<-NULL
    # Group.1<-colnames(mydata)[i]
    names(a1exp)<-c("Group.1", "count1")
    names(a1mean)<-c("Group.2", "avg")
    df<-cbind(a1mean,a1exp)
    df$avg<-round(df$avg, digits = 2)
    print(ggplot(df)  + 
            geom_bar(aes(x=Group.1, y=count1),stat="identity", fill="red", colour="sienna3")+
            geom_line(aes(x=Group.1, y=avg),stat="identity",group = 1 )+
            geom_text(aes(label=avg, x=Group.1, y=avg), colour="black")+
            geom_text(aes(label=count1, x=Group.1, y=0.95*count1), colour="blue")+
            ggtitle(colnames(mydata)[i])+
          #  scale_y_continuous(sec.axis = sec_axis(~.)))+
            theme(axis.text.x = element_text(angle = 45)))
    
    # xlab(colnames(mydata)[i])
  }
  else {
    h1<-hist(mydata[,i], plot = FALSE)
    y<- list(cut(mydata[,i], breaks=h1$breaks))
    responsecol<-which(colnames(mydata)==r1)
    a1mean<-aggregate(mydata[,responsecol],y, mean, na.rm = T)
    a1exp<-aggregate(mydata[,responsecol],y, length )
    df<-NULL
    n1<-colnames(mydata)[i]
    names(a1exp)<-c("Group.1", "count1")
    names(a1mean)<-c("Group.2", "avg")
    df<-cbind(a1mean,a1exp)
    df$avg<-round(df$avg, digits = 2)
    print(ggplot(df)  + 
            geom_bar(aes(x=Group.1, y=count1),stat="identity", fill="red", colour="sienna3")+
            geom_line(aes(x=Group.1, y=avg),stat="identity",group = 1 )+
            geom_text(aes(label=avg, x=Group.1, y=avg), colour="black")+
            geom_text(aes(label=count1, x=Group.1, y=0.95*count1), colour="blue")+
            ggtitle(colnames(mydata)[i])+
            scale_y_continuous(sec.axis = sec_axis(~./max(df$count1)))+
            theme(axis.text.x = element_text(angle = 45)))
    # xlab(colnames(mydata)[i])
    
    
    # xlab(colnames(mydata)[i])
    
    
    Sys.sleep(0.01)
    print("numeric")
  }
}
