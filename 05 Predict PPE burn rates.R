
# predict ppe burn rates --------------------------------------------------

# get the data on hospital locations
hospstockdata<-read.csv(paste0(mywd,"hospital stocks data.csv"))
head(hospstockdata)
# join on LGA
pctolgamap<-read.csv(paste0(mywd,"postcode to LGA map.csv"))
head(pctolgamap)

hospstockdata2<-merge(hospstockdata,pctolgamap,by.x = "postcode", by.y = "Postcode", all.x = TRUE)
head(hospstockdata2)

# agrgate at LGA level
hopsstockLGA<-aggregate(cbind(hospstockdata2$GownsStock,hospstockdata2$N95masks), by=list(hospstockdata2$LGA.region),
                        FUN = sum)
names(hopsstockLGA)<-c("LGAname","Gowns", "N95masks")
head(hopsstockLGA)

# get the PPE burn rate calucaltor data
ppeburnrates<-read.csv(paste0(mywd,"PPEcalculatorEbola.csv"))
ppeburnrates

# according to PPE for Ebola calculator, need 24 Gowns per patient per day and 16 N95 Masks per patient per day
# apply these to the data
head(lgasim2)

lgasim3<-merge(lgasim2,hopsstockLGA,by = "LGAname",all.x = TRUE)
head(lgasim3)
lgasim3<-lgasim3[with(lgasim3,order(LGAname,time)),]
head(lgasim3)
lgasim3$gownuse<-pmax(0,lgasim3$Gowns-24*lgasim3$totcases*lgasim3$hospfactor)
lgasim3$maskuse<-pmax(0,lgasim3$N95masks-16*lgasim3$totcases*lgasim3$hospfactor)
head(lgasim3,10)



# plot the shortfall
dev.off()
# mylga<-"Blacktown"
plotcasestr2 <- function(mylga) {
  ac_dat<-lgasim3[lgasim3$LGAname==mylga,]
  plot(ac_dat$time,ac_dat$I,  xlab = "Time (days)", ylab = "Active cases",
       ylim = c(0,
                1.5*max(ac_dat$I, na.rm = TRUE)
                
       )
       , cex = 1.1,bty="n")
  title(paste0(mylga ," (mwkrs=", cacordered2$numManufact[cacordered2$LGAname.x==mylga],","
               ," c=", cacordered2$ActiveCases[cacordered2$LGAname.x==mylga],")")
        , line = -2)
  points(ac_dat$time,ac_dat$hosprate, col = "Blue", lwd = 2)
  lines(ac_dat$time,ac_dat$gownuse*max(ac_dat$I, na.rm = TRUE)/max(ac_dat$gownuse), col = "Red", lwd = 2)
  lines(ac_dat$time,0.8*ac_dat$maskuse*max(ac_dat$I, na.rm = TRUE)/max(ac_dat$maskuse), col = "Green", lwd = 2)
  
  text(max(ac_dat$time)/4,
       max(ac_dat$hosprate, na.rm = TRUE),
       as.character("Gown Use Rate"), pos =3, col = "Red")
  text(max(ac_dat$time)/8,
       max(ac_dat$hosprate, na.rm = TRUE)*0.7,
       as.character("N95 Mask Use Rate"), pos =3, col = "Green")
}

dev.off()
par(mfrow = c(3,3))
par(mar = c(2,1,1,1))
cacordered3<-cacordered2[order(-cacordered2$CumCases),]
head(cacordered3,9)
for (i in 1:9){
  plotcasestr2(cacordered3$LGAname.x[i])  
}

