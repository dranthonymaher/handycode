####################################################################
#Project: 
#Description: 

#Owner: Mickey Shi
#Date Created: 20 May 2019
#Last Modified: 

####################################################################

# THE GOAL IS INTERPRETABILITY NOT ACCURACY

list.of.packages <- c(
  "mlogit",
  "readxl",
  "dplyr",
  "survival"
)

###Install/load required packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#Set seed
set.seed(123)

###specify path
data_path <- "C:/Users/mshi1/OneDrive - KPMG/Documents/Choice Modelling"

input_data_name <- "output_transformed_data.csv"
pilot_remove_list <- "pilot respondent removal RESIDs.csv"

setwd(data_path)
data_input <- read.csv(input_data_name)
pilot_remove_data <- read.csv(pilot_remove_list)

#Set Factors
#levels(data_input$Severity) <- c("Localised", "Widespread")
#levels(data_input$Duration) <- c("1 hour", "3 hours", "6 hours", "12 hours")
#levels(data_input$Season) <- c("Winter", "Summer")
#levels(data_input$TimeofDay) <- c("Off-Peak", "Peak")
#levels(data_input$Weekday) <- c("Weekdays", "Weekends")

data_input$Severity <- relevel(data_input$Severity, ref="Localised")
data_input$Duration <- relevel(data_input$Duration, ref="1 hour")
data_input$Season <- relevel(data_input$Season, ref="Winter")
data_input$TimeofDay <- relevel(data_input$TimeofDay, ref="Off-Peak")
data_input$Weekday <- relevel(data_input$Weekday, ref="Weekdays")
data_input$sq_outage <- relevel(data_input$sq_outage, ref="no")

#filter

#All pool
data_aemo_res_all <- data_input[data_input$Flag=="Residential 1",]
data_aer_res_all <- data_input[data_input$Flag=="Residential 2",]
data_aer_bus_all <- data_input[data_input$Flag=="Business",]
data_aer_bus_all$discount_num <- ifelse(data_aer_bus_all$Discounts=="No Change", 0,
                                        ifelse(data_aer_bus_all$Discounts=="Discount Level 1", 1,
                                               ifelse(data_aer_bus_all$Discounts=="Discount Level 2", 2,
                                                      ifelse(data_aer_bus_all$Discounts=="Discount Level 3", 3,0))))
#Exclude Hannah's definition of illogical choice
data_aemo_res_excl8BL <- data_input[data_input$Flag=="Residential 1",]
data_aemo_res_excl8BL <- subset(data_aemo_res_excl8BL, 
                                !(data_aemo_res_excl8BL$RESPID %in% pilot_remove_data$RESPID))

data_aer_res_excl8BL <- data_input[data_input$Flag=="Residential 2",]
data_aer_res_excl8BL <- subset(data_aer_res_excl8BL, 
                                !(data_aer_res_excl8BL$RESPID %in% pilot_remove_data$RESPID))

data_aer_bus_excl8BL <- data_input[data_input$Flag=="Business",]
data_aer_bus_excl8BL <- subset(data_aer_bus_excl8BL, 
                               !(data_aer_bus_excl8BL$RESPID %in% pilot_remove_data$RESPID))
data_aer_bus_excl8BL$discount_num <- ifelse(data_aer_bus_excl8BL$Discounts=="No Change", 0,
                                        ifelse(data_aer_bus_excl8BL$Discounts=="Discount Level 1", 1,
                                               ifelse(data_aer_bus_excl8BL$Discounts=="Discount Level 2", 2,
                                                      ifelse(data_aer_bus_excl8BL$Discounts=="Discount Level 3", 3,0))))

#Climate Zone
data_aer_res_cz2 <- data_aer_res_excl8BL[data_aer_res_excl8BL$Climate_Zone==2,]
data_aer_res_cz5 <- data_aer_res_excl8BL[data_aer_res_excl8BL$Climate_Zone==5,]
data_aer_res_cz6 <- data_aer_res_excl8BL[data_aer_res_excl8BL$Climate_Zone==6,]


#Exclude all 8BLs
data_input$sq_outage_ind <- ifelse(data_input$sq_outage=="yes", 1, 0)*data_input$Response
all_8bl_list <- data_input %>%
                  group_by(RESPID) %>%
                  summarise(sq_outage_res=sum(sq_outage_ind))
all_8bl_list <- all_8bl_list[all_8bl_list$sq_outage_res==8,]

data_aemo_res_exclall8BL <- data_input[data_input$Flag=="Residential 1",]
data_aemo_res_exclall8BL <- subset(data_aemo_res_exclall8BL, 
                                !(data_aemo_res_exclall8BL$RESPID %in% all_8bl_list$RESPID))


data_aer_res_exclall8BL <- data_input[data_input$Flag=="Residential 2",]
data_aer_res_exclall8BL <- subset(data_aer_res_exclall8BL, 
                                   !(data_aer_res_exclall8BL$RESPID %in% all_8bl_list$RESPID))

data_aer_bus_exclall8BL <- data_input[data_input$Flag=="Business",]
data_aer_bus_exclall8BL <- subset(data_aer_bus_exclall8BL, 
                                  !(data_aer_bus_exclall8BL$RESPID %in% all_8bl_list$RESPID))


###############################
###Fitting model
###############################

#define model function
choice_model <- function(input_data, output_data, model_name) {
  
  model_data <- mlogit.data(input_data, choice = "Response", shape = "long",
                                    alt.var = "choice",
                                    chid.var = "choiceID")
  
  model_all <- mlogit(Response ~ sq_outage+
                        Severity
                      + Duration
                      + Season
                      + TimeofDay
                      + Weekday
                      + discount_num
                      | -1
                      , reflevel = "1", 
                      data = model_data
  )
  temp_coef <- as.data.frame(summary(model_all)$CoefTable)
  temp_coef$modelname <- paste0(model_name)
  write.csv(temp_coef, paste0(model_name,".csv"))
}


#1.AEMO Residential all
choice_model(data_aemo_res_all, coef_aemo_res_all, "aemo_res_all")

#2.AER Residential all
choice_model(data_aer_res_all, coef_aer_res_all, "aer_res_all")

#3.AER Business all
choice_model(data_aer_bus_all, coef_aer_bus_all, "aer_bus_all")

#4.AEMO Residential excluding 8BL list (hannah's definition)
choice_model(data_aemo_res_excl8BL, coef_aemo_res_excl8BL, "aemo_res_excl8BL")

#5.AER Residential excluding 8BL list (hannah's definition)
choice_model(data_aer_res_excl8BL, coef_aer_res_excl8BL, "aer_res_excl8BL")

#6.AER Business excluding 8BL list (hannah's definition)
choice_model(data_aer_bus_excl8BL, coef_aer_bus_excl8BL, "aer_bus_excl8BL")

#7.Climate Zone 2 AER Residential excluding 8BL list
choice_model(data_aer_res_cz2, coef_aer_res_cz2, "aer_res_cz2")

#8.Climate Zone 5 AER Residential excluding 8BL list
choice_model(data_aer_res_cz5, coef_aer_res_cz5, "aer_res_cz5")

#9.Climate Zone 6 AER Residential excluding 8BL list
choice_model(data_aer_res_cz6, coef_aer_res_cz6, "aer_res_cz6")

#10.AEMO Residential excluding 8BL list (all 8BLs)
choice_model(data_aemo_res_exclall8BL, coef_aemo_res_exclall8BL, "aemo_res_exclall8BL")

#11.AER Residential excluding 8BL list (all 8BLs)
choice_model(data_aer_res_exclall8BL, coef_aer_res_exclall8BL, "aer_res_exclall8BL")

#12.AER Business excluding 8BL list (all 8BLs)
choice_model(data_aer_bus_exclall8BL, coef_aer_bus_exclall8BL, "aer_bus_exclall8BL")
