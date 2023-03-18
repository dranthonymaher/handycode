
# wtp for group 1 and group2
g1wtp=40
g2wtp=30

#how many punters in each group
nwtp1=160
nwtp2=50
totpunters<-nwtp1+nwtp2
# all possible prices
p<-seq(1,100,1)

# define function for how many punters will be there for each given price
punters<-ifelse(p<=g2wtp,totpunters,ifelse(p<=g1wtp,nwtp1,0))
plot(p,punters)

# calculate total revenue per price
rev=p*punters
plot(p,rev, type = 'l')

# find maximum revenue
maxrev<-which.max(rev)

# how many tickets will be sold?
tixsold<-punters[maxrev]


# Part 2 ------------------------------------------------------------------




# all possible prices
p<-seq(1,100,1)

# define function for how many punters will be there for each given price
punters<-200-2*p
plot(p,punters)

# calculate total revenue per price
rev=p*punters
plot(p,rev, type = 'l')
# find price at which revenue is maximum
pmaxrev<-which.max(rev)
# what is total rev?
myrev<-rev[pmaxrev]
# how many tickets will be sold?
tixsold<-punters[maxrev]
plot(p,rev, type = 'l', main=paste0("price=",pmaxrev, ". rev = ",myrev,". tix sold=",tixsold))
