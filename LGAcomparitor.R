# LGA comparitor

library("xlsx")
# Clear console and set up directory --------------------------------------

#close all plot windows
dev.off()
#remove everything from the console
rm(list = ls())

# enter your own username here
username<-"amaher2"
# define the working directory - this is the loaction of the data sets on the Teams site
mywd<-paste0("C:/Users/",username,"/Downloads/")
#set the working directory
setwd(mywd)

df<-read
