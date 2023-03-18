

# get ABS data on industry employment by small area
nswindustrybyLGA<-read.csv(paste0(mywd,"SA2_INDUSTRY_LEVEL1_GEOtoLGA.csv"))
head(nswindustrybyLGA)
# summarise by LGA
lgainssumm<-aggregate(nswindustrybyLGA$Manufacturing, by = list(nswindustrybyLGA$LGAcode), FUN = sum)
names(lgainssumm)<-c("LGAcode","numManufact")
#attach on to the cases data
cacordered2<-merge(cacordered, lgainssumm,by = "LGAcode", all.x = TRUE )
head(cacordered2)



# Run the (uncontolled) outbreak simulator for every LGA in NSW----------------------------------------------

lgasim<-NULL
for (i in 1:length(cacordered2[, 1])) {
  myn <- cacordered2$numManufact[i]
  myi <- cacordered$ActiveCases[i]
  a1 <- outbreaksimulator(myn, myi)
  # Sys.sleep(0.01)
  # print(a1$totcases) - something strange going on with Randwick
  a1$LGAname <- cacordered$LGAname[i]
  a1$LGAcode <- cacordered$LGAcode[i]
  lgasim <- rbind(lgasim, a1)
}
head(lgasim)

# cacordered2[cacordered2$LGAname=="Randwick",]
# lgasim[lgasim$LGAname=="Randwick",]

# integrate the hosp proj data to compute hospitalisations and ICU rates
lgasim2<-merge(lgasim,agg.hicu,by = "LGAcode", all.x = TRUE)
head(lgasim2)
# compute hospitalisation and ICU rates
lgasim2$hosprate<-round(lgasim2$I*lgasim2$hospfactor)
lgasim2$icurate<-round(lgasim2$I*lgasim2$icufactor)

lgasim2$LGAname<-gsub("\\s*\\([^\\)]+\\)","",as.character(lgasim2$LGAname))



write.csv(lgasim2,"LGA_outbreak_SimResults.csv")


# mylga = "Bayside"
head(lgasim2)
# Build a function to plot the hoslitalistion over time for each LGA
plotcasestr <- function(mylga) {
  ac_dat<-lgasim2[lgasim2$LGAname==mylga,]
  plot(ac_dat$time,ac_dat$I,  xlab = "Time (days)", ylab = "Active cases",
       ylim = c(0,
                #1.5*max(ac_dat$I, na.rm = TRUE)
                600
                )
       , cex = 1.1,bty="n")
  title(paste0(mylga ," (mwkrs=", cacordered2$numManufact[cacordered2$LGAname.x==mylga],","
               ," c=", cacordered2$ActiveCases[cacordered2$LGAname.x==mylga],")")
        , line = -2)
  lines(ac_dat$time,ac_dat$hosprate, col = "Blue", lwd = 2)
  lines(ac_dat$time,ac_dat$icurate, col = "Red", lwd = 2)
  
  text(max(ac_dat$time)/4,
       max(ac_dat$hosprate, na.rm = TRUE),
       as.character("Hospitalistaion Rate"), pos =3, col = "Blue")
  text(max(ac_dat$time)/4,
       max(ac_dat$icurate, na.rm = TRUE),
       as.character("ICU Rate"), pos =1, col = "Red")
  }
dev.off()
par(mfrow = c(4,3))
par(mar = c(2,1,1,1))
cacordered3<-cacordered2[order(-cacordered2$CumCases),]
head(cacordered3,12)
for (i in 1:12){
  plotcasestr(cacordered3$LGAname.x[i])  
}

head(lgasim2)

# convert to required format
lgasim2ex<-lgasim2
lgasim2ex$layer<-"LGA"
lgasim2ex$type<-"line"
lgasim2ex$name<-"Chart Title"
lgasim2ex$id<-lgasim2$LGAcode
lgasim2ex$x<-lgasim2ex$time
lgasim2ex$y<-lgasim2ex$I
head(lgasim2ex)
lgasim2ex<-lgasim2ex[,c("layer","type","name","id","x","y")]
# export the result to .csv
write.csv(lgasim2ex,"C:/Users/amaher2/KPMG/AU - Data Office - Data sets/simulationoutputLGA.csv")



# Check individual LGAs ---------------------------------------------------



# compare Mid-Coast (old folks!) to Blacktown (younger deomgraphic!)
dev.off()
par(mfrow=c(1,2))
mylga = "Mid-Coast"
ac_dat<-lgasim2[lgasim2$LGAname==mylga,]
plot(ac_dat$time,ac_dat$I,  xlab = "Time (days)", ylab = "Active cases",
     ylim = c(0,
              1.0*max(ac_dat$I, na.rm = TRUE)
              
     )
     , cex = 1.1,bty="n")
title(paste0(mylga ," (mwkrs=", cacordered2$numManufact[cacordered2$LGAname.x==mylga],","
             ," c=", cacordered2$ActiveCases[cacordered2$LGAname.x==mylga],")")
      , line = -2)
lines(ac_dat$time,ac_dat$hosprate, col = "Blue", lwd = 2)
lines(ac_dat$time,ac_dat$icurate, col = "Red", lwd = 2)

text(max(ac_dat$time)/4,
     max(ac_dat$hosprate, na.rm = TRUE),
     as.character("Hospitalistaion Rate"), pos =3, col = "Blue")
text(max(ac_dat$time)/4,
     max(ac_dat$icurate, na.rm = TRUE),
     as.character("ICU Rate"), pos =1, col = "Red")



mylga = "Blacktown"
ac_dat<-lgasim2[lgasim2$LGAname==mylga,]
plot(ac_dat$time,ac_dat$I,  xlab = "Time (days)", ylab = "Active cases",
     ylim = c(0,
              1.0*max(ac_dat$I, na.rm = TRUE)
              
     )
     , cex = 1.1,bty="n")
title(paste0(mylga ," (mwkrs=", cacordered2$numManufact[cacordered2$LGAname.x==mylga],","
             ," c=", cacordered2$ActiveCases[cacordered2$LGAname.x==mylga],")")
      , line = -2)
lines(ac_dat$time,ac_dat$hosprate, col = "Blue", lwd = 2)
lines(ac_dat$time,ac_dat$icurate, col = "Red", lwd = 2)

text(max(ac_dat$time)/4,
     max(ac_dat$hosprate, na.rm = TRUE),
     as.character("Hospitalistaion Rate"), pos =3, col = "Blue")
text(max(ac_dat$time)/4,
     max(ac_dat$icurate, na.rm = TRUE),
     as.character("ICU Rate"), pos =1, col = "Red")

