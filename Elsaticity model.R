
# read in the "ChannelStores" table
storeinfo<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/dax/all data/ChannelStores.csv")
colnames(storeinfo)
storelist<-unique(storeinfo$address_id)

# read in the "Product List" table
product_cats<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/dax/all data/Product List.csv")

# get sales for selected number of stores only

# Note that this was the SQL query used:
# select *
# from [dbo].[qtr_retailer]
# where 
# [media_plcmt]  in ('B&G(PM)','B&G(PRR)','NAS','NonProm','NonPromRaw','PM(Lge)','PM(Sml)')
# and [address_id] in ( '12010','18974','33201','21824','451','5914','19435','11242','48084','48145','19294','19569','48113','20153','33063','813','50491','52521','50428','32697','43750','46009')
# and 
# (
#   LTRIM(RTRIM([IGA_med_base]))<>''
#   or LTRIM(RTRIM([IGA_large_base]))<>''
#   or LTRIM(RTRIM([IGA_small_base]))<>''
# )
# and prom_year_num = '2020'

# read in sales data for the following stores:
#'12010','18974','33201','21824','451','5914','19435','11242','48084','48145','19294','19569','48113','20153','33063','813','50491','52521','50428','32697','43750','46009')
# where 
# [media_plcmt]  = ('B&G(PM)','B&G(PRR)','NAS','NonProm','NonPromRaw','PM(Lge)','PM(Sml)')
# for base range products only
sales<-read.csv("C:/Users/amaher2/OneDrive - KPMG/Documents/dax/flatfileforelast2.csv")
colnames(sales)

# check
table(sales$media_plcmt)

# 
# 1. Aggregate by store / promo /  product 
a1<-aggregate(cbind(sales$sales_qty_float,sales$asp_AUD_float, sales$promo_asp_AUD_float), 
              by=list(sales$address_id,sales$media_plcmt,sales$product_cd),
              FUN = mean)
names(a1)<-c("address_id","media_plcmt","product_id","msalesq","masp","mpromoasp")

# 2. sum up the product sales (to take only prods with sufficietn sales)
sumprodsales<-aggregate(cbind(sales$sales_qty_float), 
              by=list(sales$address_id,sales$product_cd),
              FUN = sum)
names(sumprodsales)<-c("address_id","product_id","sum_salesq")

# 3. count the number of unique different promos per item (so that we can be confident it's seen a lower price)
mediacount<-aggregate(cbind(sales$media_plcmt), 
                        by=list(sales$address_id,sales$product_cd),
                      function(x) length(unique(x)))
names(mediacount)<-c("address_id","product_id","countmedia")

#4 now join those on by address_id and product_id
a2<-merge(a1,sumprodsales,by = c("address_id","product_id"), all.x = TRUE)
a2<-merge(a2,mediacount,by = c("address_id","product_id"), all.x = TRUE)

# --now join on nonPromRaw price to mytable
nonpromsales<-a2[a2$media_plcmt=="NonPromRaw",c(1,2,4,5)]
colnames(nonpromsales)
names(nonpromsales)<-c("address_id","product_id","msalesatnonprom","maspatnonprom")

a2<-merge(a2,nonpromsales,by = c("address_id","product_id"), all.x = TRUE)

# choose only ones with more than 20 sales total
a3<-a2[a2$sum_salesq>20,]

#choose only ones for which we have a nonprom raw price
a3<-a2[is.na(a2$msalesatnonprom)==FALSE,]

# calculate the % diff from shelf price to competitor price
a3$pctdifftocomp<-(a3$maspatnonprom-a3$mpromoasp)/a3$mpromoasp

# calculate new sales at comp price
a3$newslaesatcopmprice<-ifelse(a3$pctdifftocomp<0.01,a3$msalesq,NA)

# calculate ADDITIONAL sales once shifted to comp shelf price
a3$added_sales<-ifelse(a3$newslaesatcopmprice - a3$msalesatnonprom<0,0,a3$newslaesatcopmprice - a3$msalesatnonprom)

# calculate % ADDITIONAL sales once shifted to comp shelf price
a3$added_sales_pct<-ifelse(a3$newslaesatcopmprice /a3$msalesatnonprom<1,0,a3$newslaesatcopmprice / a3$msalesatnonprom-1)

#check
plot(a3$pctdifftocomp,a3$added_sales_pct, xlim = c(-1,0),ylim = c(0,10))

# calculate the average for every product
a4<-a3[is.na(a3$added_sales_pct)==FALSE,]

a5<-aggregate(cbind(a4$pctdifftocomp,a4$added_sales_pct), by=list(a4$product_id), FUN = mean)
names(a5)<-c("product_cd","pctdifftocomp","added_sales_pct")
plot(a5$pctdifftocomp,a5$added_sales_pct,xlim = c(-1,1),ylim = c(-1,1))
text(a5$pctdifftocomp,a5$added_sales_pct, a5$product_cd)

# join on product cats and aggregate
a6<-merge(a5,product_cats,by = "product_cd",all.x=TRUE)

a7<-aggregate(cbind(a6$pctdifftocomp,a6$added_sales_pct), by=list(a6$finance_department_nm), FUN = mean)
names(a7)<-c("dept","pctdifftocomp","added_sales_pct")
plot(a7$pctdifftocomp,a7$added_sales_pct)#,xlim = c(-1,1),ylim = c(-1,1))
text(a7$pctdifftocomp,a7$added_sales_pct, a7$dept)

a7<-aggregate(cbind(a6$pctdifftocomp,a6$added_sales_pct), by=list(a6$msc_section_nm), FUN = mean)
names(a7)<-c("dept","pctdifftocomp","added_sales_pct")
plot(a7$pctdifftocomp,a7$added_sales_pct)#,xlim = c(-1,1),ylim = c(-1,1))
text(a7$pctdifftocomp,a7$added_sales_pct, a7$dept)


# export the data
write.csv(a5,"C:/Users/amaher2/OneDrive - KPMG/Documents/dax/product_elasticities_v4.csv")






