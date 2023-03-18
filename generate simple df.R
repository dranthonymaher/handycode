# generate data

# categorical variables
ncat<-c("hosp","year","therapeuticarea")

hosp<-c("RPA","Prince of Wales","Westmead","RNS")

tarea<-c("cancer","othapedics","COPD","diabetes")
year<-c(2017:2022)
df<-as.data.frame(expand.grid(hosp,year,tarea))

# metrics
df$metric1<-rnorm(nrow(df),100,3)
df$metric2<-rpois(nrow(df),10)
df$metric3<-rnorm(nrow(df),100,3)
df$metric4<-rbinom(nrow(df),1,0.5)
plot(df)

write.csv(df,"thisisdf.csv")
getwd()

# 
# Different extract times
# ims+ constantly evolving
# he filtered on reported  date rather than incident date
# 
# can remove all patient details
# 
# mental health facility - multiple people affected by covid outbreak in prison
# RIB has to be done for any confirmed harm score 1 - but anything can have a RIB if the CEO asks for it
