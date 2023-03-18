function [Eopls, Tortho, Portho, Wortho, Xortho,rat1] = oplstones(X,Y,pcnum);
#function [Eopls, Tortho, Portho, Wortho, Xortho] = oplstones(X,Y,pcnum);
#Anthony maher 26/3/09
#ONLY USE 1 dimensional Y


#
#check Y is a vector
#dimy=size(Y);

#if dimy(2) ~= 1
#disp('sorry mate this only works when Y is a vector')
#end

#step 1 take any vector u from Y
u=Y(:,1);



for i=1:pcnum;

#define threshold as 1
thresh=1;
t=zeros(length(X(:,1)),1);

while thresh > 1e-10
#step 2
w=X'*u/(u'*u);
#step 3
w=w/norm(w); #(normalization)
#step 4
tnew=X*w/(w'*w);
          
          #step 5
          q=Y'*tnew/(tnew'*tnew);
                     #step 6
                     q=q/norm(q);
                     #step 7 
                     u=Y*q/(q'*q);
    
    thresh=norm(t-tnew);
    #disp(num2str(thresh))
    
    t=tnew;
    
    disp(['done iteration ' num2str(i)])
    
end
    
#disp('doing this bit')
#step 9
p=X'*t/(t'*t);
# add extra steps for OPLS
wortho=p-(w'*p/(w'*w))*w;
wortho=wortho/norm(wortho);
tortho=X*wortho/(wortho'*wortho);
                 portho=X'*tortho/(tortho'*tortho);
                                   
                                   Eopls=X-tortho*portho';


Tortho(:,i)=tortho;
Portho(i,:)=portho;
Wortho(i,:)=wortho;
rat1(i)=norm(p-(w'*p/(w'*w))*w)/norm(p);

# disp('doing this bit 4')

#disp('doing this bit 5')
X=Eopls;
Xortho = Tortho*Portho;



end