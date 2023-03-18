

expand.grid(0:1, 0:1, 0:1)

settings<-c("ED","ICU","theatre","surg wards","med wards","community")
LHNs<-c("Barossa Hills Fleurieu LHN",
        "Central Adelaide LHN",
        "Eyre and Far North LHN",
        "Flinders and Upper North LHN",
        "Limestone Coast LHN",
        "Northern Adelaide LHN",
        "Riverland Mallee Coorong LHN",
        "Southern Adelaide LHN",
        "Womens and Childrens LHN",
        "Yorke and Northern LHN")
#OccGroups<-c("Medical","Nursing","admin","hotel","allied")
HospOccGroups<-c("JMOs","Medics","NursesL1","NursesL2","NUMs")
LHNOccGroups<-c("admin","allied","hotel")

m1<-expand.grid(HospOccGroups,settings, LHNs, KEEP.OUT.ATTRS = TRUE)
m2<-expand.grid(LHNOccGroups, LHNs, KEEP.OUT.ATTRS = TRUE)


write.csv(m1,"SAhealthcombos_hosp.csv")
write.csv(m2,"SAhealthcombos_lhns.csv")


