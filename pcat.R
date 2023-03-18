#function [T,P,E]=pcatones(X,pcnum);
#%function [T,P,E]=pcatones(X,pcnum)
#%computes the first pcnum PCs from X using NIPALS algorithm
#%based on Geladi (1986) Anal Chim Acta 185:1-17
#%anthony maher 24/3/09
#converted to R 18/3/11

pcat<-function(Xn,pcnum){

#step 1 take any vector t from X
tv=Xn[,2]

To<-matrix(nrow=nrow(Xn),ncol=pcnum)
P<-matrix(nrow=pcnum,ncol=ncol(Xn))


for(i in 1:pcnum){

    
#define threshold as 1
thresh<-1

while(thresh > 1e-10){
    p<-((t(Xn))%*%tv)%*%solve((t(tv))%*%tv)
    pnew<-p%*%solve(sqrt(sum((p)^2)))
    tnew<-Xn%*%pnew%*%solve((t(pnew)%*%pnew))

    thresh<-sqrt(sum((tv-tnew)^2))
    
    p<-pnew
    tv<-tnew
    print(thresh)
    }

To[,i]<-tv
P[i,]<-p

Xn<-Xn-tv%*%t(p)

E<-Xn

}


result<-list(scores=To,loadings=P)
return(result)
}