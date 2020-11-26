
#############################################################
#   Channel Recommendation Engine
#   Description: Get Data for Channel Profile and Hotel Nights/Room/Day
#               
#   Author: Mickey Shi
#   Date: 22OCT2018

#############################################################

# Load libraries
library(DBI)
library(RPostgres)
library(lubridate)
library(dplyr)
library(sqldf)
library(recommenderlab)

setwd("C:/Users/Mickey Shi/Documents/Sandbox/Channel Recommendation Model")

#Connection to Data Warehouse
conn <- dbConnect(RPostgres::Postgres(), 
                  host="sm-bi.c9ffhnpgamzz.us-west-2.redshift.amazonaws.com", 
                  port="5439", 
                  dbname="staging", 
                  user="bi_mickey", 
                  password="Mickey312")


dbGetQuery(conn, "
           drop table IF EXISTS bu_growth.temp_cre1 cascade
           ")

dbGetQuery(conn, "
create table bu_growth.temp_cre1 as 
select source_created_month, siteminder_property_id, hotel_name, hotel_id, region, channel_code, no_of_rooms, hotel_country, sales_sub_region

   --earliest channel create date
   ,coalesce(channel_setup_at, hotel_created_at) as min_setup_at
   ,reservation
   ,revenue_total_aud
   ,room_night_total
   ,avg_lead_time
   ,reservation_cancelled
   ,avg_length_of_stay
   
   from dbi_bi_prd_src_tabs.cm_reservation_hist
   where channel_type = 'OTA' and outlier_account = 'false' and outlier_reservation = 'false' and source_created_month >= '2017-01-01'
           ")


dbGetQuery(conn, "
           drop table IF EXISTS bu_growth.temp_cre2 cascade
           ")

dbGetQuery(conn, "
create table bu_growth.temp_cre2 as 
select siteminder_property_id, channel_code
from bu_growth.temp_cre1
where date_part(year, min_setup_at)*12+date_part(month, min_setup_at) 
        < date_part(year, source_created_month)*12+date_part(month, source_created_month) 
group by 
")




temp$hotel_country <- as.factor(temp$hotel_country)
temp$sales_sub_region <- as.factor(temp$sales_sub_region)
temp$channel_code <- as.factor(temp$channel_code)
temp$rev_pernight <- temp$revenue_total_aud/temp$room_night_total
temp$cancellation_rate <- temp$reservation_cancelled/temp$reservation
temp$channel_ota <- as.numeric(temp$channel_ota)
temp$channel_whole_saler <- as.numeric(temp$channel_whole_saler)
temp$channel_tmc <- as.numeric(temp$channel_tmc)
temp$channel_gds <- as.numeric(temp$channel_gds)
temp$channel_crs <- as.numeric(temp$channel_crs)
temp$channel_ibe <- as.numeric(temp$channel_ibe)
temp$room_night_total <- as.numeric(temp$room_night_total)
temp$reservation <- as.numeric(temp$reservation)
temp$reservation_cancelled <- as.numeric(temp$reservation_cancelled)

add_region <- read.csv("add_region.csv")

temp1 <- sqldf("
select x.*, coalesce(x.sales_sub_region, y.sales_sub_region) as sales_sub_region_adj
from temp as x
left join
add_region as y
on x.hotel_country = y.hotel_country
         ")

#keep variables
keep_var <- c(
  "siteminder_property_id",
  "hotel_id",
  "region",
  "no_of_rooms",
  "sales_sub_region_adj",
  "channel_code",
  "rev_pernight",
  "ann_rev_per_room",
  "avg_lead_time",
  "cancellation_rate",
  "avg_length_of_stay"
  )

temp1$sales_sub_region_adj <- as.factor(temp1$sales_sub_region_adj)

temp1 <- temp1[,keep_var]

rm(temp)


#write.csv(temp1, "temp_ms.csv")

#model for each metric: rev_pernight

#########################################
#Create recommendation model

#Change to sparse matrix

temp1$spid <- as.numeric(factor(temp1$siteminder_property_id, 
                                levels=unique(temp1$siteminder_property_id)))

temp1$channel_code_id <- as.numeric(factor(temp1$channel_code, 
                                levels=unique(temp1$channel_code)))

temp_sparse <- sparseMatrix(i = temp1$spid, j=temp1$channel_code_id, x=temp1$rev_pernight,
                            dims = c(length(unique(temp1$siteminder_property_id)), length(unique(temp1$channel_code))),  
                            dimnames = list(paste("u", 1:length(unique(temp1$siteminder_property_id)), sep = ""), 
                                            paste("m", 1:length(unique(temp1$channel_code)), sep = "")))

#Partition data for modelling


temp_model <- Recommender(new("realRatingMatrix", data=temp_sparse), method="POPULAR", param=list(normalize = "center"))

temp_pred <- predict(temp_model, new("realRatingMatrix", data=temp_sparse)[1:5], type="ratings")
as(temp_pred, "matrix")[,1:5]


#Test model
set.seed(123)
e <- evaluationScheme(new("realRatingMatrix", data=temp_sparse), method="split", train=0.8, given = -1)
temp_testmodel <- Recommender(getData(e, "train"), "POPULAR")
prediction <- predict(temp_testmodel, getData(e, "known"), type="ratings")
rmse_popular <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]

##########################################

#Get scoring data
score <- 
  dbGetQuery(conn, "
             
             select siteminder_property_id, hotel_name, hotel_id, region, channel_code, no_of_rooms, hotel_country, sales_sub_region
             
             --earliest channel create date
             ,coalesce(min(channel_setup_at), min(hotel_created_at)) as min_setup_at
             
             --reservation price per room night
             ,sum(revenue_total_aud) as revenue_total_aud
             ,sum(room_night_total) as room_night_total
             
             --Channel categories
             ,count(distinct (case when channel_type = 'OTA' then channel_name end)) as channel_OTA 
             ,count(distinct (case when channel_type = 'WHOLE_SALER' then channel_name end)) as channel_WHOLE_SALER
             ,count(distinct (case when channel_type = 'TRAVEL_MANAGEMENT_COMPANY' then channel_name end)) as channel_TMC
             ,count(distinct (case when channel_type is null then channel_name end)) as channel_GDS 
             ,count(distinct (case when channel_type = 'CRS' then channel_name end)) as channel_CRS
             ,count(distinct (case when channel_type = 'IBE' then channel_name end)) as channel_IBE
             
             --Annualised Revenue per room
             ,(sum(revenue_total_aud)/count(distinct source_created_month))*12/max(no_of_rooms) as ann_rev_per_room
             
             --average lead time
             ,avg(avg_lead_time) as avg_lead_time
             
             --cancellation %
             ,sum(reservation_cancelled) as reservation_cancelled
             ,sum(reservation) as reservation
             
             --average length of stay
             ,avg(avg_length_of_stay) as avg_length_of_stay
             --select top 100 *
             from dbi_bi_prd_src_tabs.cm_reservation_hist
             where source_created_month = '2018-09-30' and channel_type = 'OTA'
             and no_of_rooms > 0 
             and avg_length_of_stay is not null and avg_lead_time is not null and room_night_total is not null
             and revenue_total_aud is not null and outlier_account = 'false' and outlier_reservation = 'false'
             group by siteminder_property_id, hotel_name, hotel_id, region, channel_code, no_of_rooms, hotel_country, sales_sub_region
             
             ")

score$hotel_country <- as.factor(score$hotel_country)
score$sales_sub_region <- as.factor(score$sales_sub_region)
score$channel_code <- as.factor(score$channel_code)
score$rev_pernight <- score$revenue_total_aud/score$room_night_total
score$cancellation_rate <- score$reservation_cancelled/score$reservation
score$channel_ota <- as.numeric(score$channel_ota)
score$channel_whole_saler <- as.numeric(score$channel_whole_saler)
score$channel_tmc <- as.numeric(score$channel_tmc)
score$channel_gds <- as.numeric(score$channel_gds)
score$channel_crs <- as.numeric(score$channel_crs)
score$channel_ibe <- as.numeric(score$channel_ibe)
score$room_night_total <- as.numeric(score$room_night_total)
score$reservation <- as.numeric(score$reservation)
score$reservation_cancelled <- as.numeric(score$reservation_cancelled)

score1 <- sqldf("
               select x.*, coalesce(x.sales_sub_region, y.sales_sub_region) as sales_sub_region_adj
               from score as x
               left join
               add_region as y
               on x.hotel_country = y.hotel_country
               ")

score1$sales_sub_region_adj <- as.factor(score1$sales_sub_region_adj)

score1 <- score1[,keep_var]

rm(score)

###########################################
#Scoring model to new data
score1$spid <- as.numeric(factor(score1$siteminder_property_id, 
                                levels=unique(score1$siteminder_property_id)))

score1$channel_code_id <- as.numeric(factor(score1$channel_code, 
                                           levels=unique(score1$channel_code)))

score_sparse <- sparseMatrix(i = score1$spid, j=score1$channel_code_id, x=score1$rev_pernight,
                            dims = c(length(unique(score1$siteminder_property_id)), length(unique(score1$channel_code))),  
                            dimnames = list(paste("u", 1:length(unique(score1$siteminder_property_id)), sep = ""), 
                                            paste("m", 1:length(unique(score1$channel_code)), sep = "")))

score_pred <- predict(temp_model, new("realRatingMatrix", data=score_sparse), type="ratings")
scored <- as.data.frame(as(score_pred, "matrix")[1:5,1:5])

