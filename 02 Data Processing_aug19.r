####################################################################
#Project: Australian Energy Regulator - Choice Model
#Description: Data preparation process

#Owner: Garry Singh
#Date Created: 20 May 2019
#Last Modified: 6 Sep 2019

####################################################################

# Install packages to read data
# install.packages("readxl")
# install.packages("readr")
# install.packages('dplyr')
list.of.packages <- c(
  "readxl", 
  "readr", 
  "dplyr",
  "corrplot",
  "zoo"
  )

#Set path
data_path <- "C:/Users/mshi1/OneDrive - KPMG/Documents/Choice Modelling/Main Survey"
input_data <- "ORD-428748-D8W2 Electricity Consumer & B2B MAIN Stage 2 Pilot Data_06Aug_v1.xlsx"
input_namelist <- "variable input file.csv"
###Install/load required packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# Set working directory where the data is located
#getwd()
setwd(data_path)

# Read sheets and use for filenames
sheets <- excel_sheets(input_data)
filenames <- paste0(sheets, ".csv")

# read_excel only reads a single sheet, so lapply over each sheet name
dats <- lapply(sheets, read_excel, path = input_data)

# Save each data frame with different filename using write_csv
# This should output 3 csv files: Codeframe.csv, Data.csv & Labels.csv in the current working directory
lapply(seq_along(dats), function(i) write_csv(dats[[i]], filenames[i]))


# SETUP
  
  # Create myCodeframe df 
  myCodeframe <- read.csv(file="Codeframe.csv")
  head(myCodeframe)
  
  # Rename columns and remove first 2 rows from myCodeframe
  names(myCodeframe) <- c("Value", "Indicator", "Label")
  myCodeframe <- myCodeframe[-c(1),]
  head(myCodeframe)
  
  # Update Value column in myCodeframe 
#  holdvalue <- myCodeframe[1,1]
#  for(i in 1:nrow(myCodeframe)) {
#    if(myCodeframe[i,2]==1) {
#      holdvalue = myCodeframe[i,1] 
#    } else {
#      myCodeframe[i,1] = holdvalue
#    }
#  }

myCodeframe$Value<-na.locf(myCodeframe$Value)
    
  # Create myLabels df
  myLabels <- read.csv(file="Labels.csv")
  head(myLabels)
  # Rename columns and remove first 2 rows from myLabels
  names(myLabels) <- c("Variable", "Label")
  myLabels <- myLabels[-c(1,2),]
  head(myLabels)
  
  # Create myData df 
  myData <- read.csv(file="Data.csv", header=TRUE)
  
  # Keep only the relevant columns for analysis

  variable_namelist <- read.csv(input_namelist)
  variable_namelist$name_order <- seq.int(nrow(variable_namelist))
  label_namelist <- variable_namelist[variable_namelist$label==1,]$name_order
  variable_namelist$severity_ind <- ifelse(grepl("ATT_1", variable_namelist$variable)==TRUE,
                                           1,0)
    
    
  relevantcols <- as.character(variable_namelist$variable)
  
  #relevantcols = c(1,18,151:351,374,479,480,491:691,700:900)
  
  test3 <- subset(myData, select = relevantcols)
  
  # RUN WITHOUT THIS FOR CORRELATION MATRIX
#  for(j in c(2,7:27,32:52,57:77,82:102,107:127,132:152,157:177,182:202,204,
#             211:231,236:256,261:281,286:306,311:331,336:356,361:381,386:406,
#             412:432,437:457,462:482,487:507,512:532,537:557,562:582,587:607)) {
#    for(i in 1:nrow(test3)) {
#      if(is.na(test3[i,j])){
#        NA 
#      } else {
#      test3[i,j] <- toString(subset(myCodeframe, myCodeframe$Value==colnames(test3[j]) & myCodeframe$Indicator==test3[i,j])[3][[1]])
#      }
#    }
#  }

  for(j in label_namelist) {
    for(i in 1:nrow(test3)) {
      if(is.na(test3[i,j])){
        NA 
      } else {
        test3[i,j] <- toString(subset(myCodeframe, myCodeframe$Value==colnames(test3[j]) & myCodeframe$Indicator==test3[i,j])[3][[1]])
      }
    }
  }

# MAIN LOGIC 
  
  # Initialise vectors
  
  Severity <- c()
  Duration <- c()
  Frequency <- c()
  Season <- c()
  TimeofDay <- c()
  Weekday <- c()
  Discounts <- c()
  Response <- c()
  Flag <- c()
  RESPID <- c()
  Block <- c()
  State <- c()
  Choiceset <- c()
  Response_Index <- c()
  Climate_Zone <- c()
  Remoteness <- c()
  
  # Initialise Lists
  
  duration_list <- list()
  frequency_list <- list()
  season_list <- list()
  timeofday_list <- list()
  weekday_list <- list()
  discounts_list <- list()
  
  # Create lists 
  # Most lists are based off the severity list
  
#  severity_list <- list(c(7:9),c(32:34),c(57:59),c(82:84),c(107:109),c(132:134),c(157:159),c(182:184),
#                        c(210:212),c(235:237),c(260:262),c(285:287),c(310:312),c(335:337),c(360:362),c(385:387),
#                        c(411:413),c(436:438),c(461:463),c(486:488),c(511:513),c(536:538),c(561:563),c(586:588))

  
  severity_list <- variable_namelist[variable_namelist$severity_ind==1,]$name_order
  temp <- variable_namelist[variable_namelist$severity_ind==1,]$name_order
  rm(temp1, severity_list)
  
for (i in 1:(length(temp)/3))  {
  temp1 <- temp[(3*(i-1)+1):(3*i)]
  if (exists("severity_list")==FALSE) {
    severity_list <- list(temp1)
  } else {
    severity_list <- c(severity_list, list(f=temp1))
  }
}
    
  for(i in 1:length(severity_list)) {
    duration_list <- append(duration_list, list(severity_list[[i]]+c(3:3)))
    frequency_list <- append(frequency_list, list(severity_list[[i]]+c(6:6)))
    season_list <- append(season_list, list(severity_list[[i]]+c(9:9)))
    timeofday_list <- append(timeofday_list, list(severity_list[[i]]+c(12:12)))
    weekday_list <- append(weekday_list, list(severity_list[[i]]+c(15:15)))
    discounts_list <- append(discounts_list, list(severity_list[[i]]+c(18:18)))
  }
  
  state_cols <-  match(c("HIDSTATE", "HIDRSTATE"),names(test3))
  block_cols <- match(c("HIDVERSION_2"),names(test3))
  climate_col <- match(c("HIDCLIMATEZONE"),names(test3))
  remote_col <- match(c("HIDREMCLASS"),names(test3))
    response_list <- match(c(
    "QBCM1_1",
    "QBCM1_2",
    "QBCM1_3",
    "QBCM1_4",
    "QBCM1_5",
    "QBCM1_6",
    "QBCM1_7",
    "QBCM1_8",
    "QRCM1_1",
    "QRCM1_2",
    "QRCM1_3",
    "QRCM1_4",
    "QRCM1_5",
    "QRCM1_6",
    "QRCM1_7",
    "QRCM1_8",
    "QRCM2_1",
    "QRCM2_2",
    "QRCM2_3",
    "QRCM2_4",
    "QRCM2_5",
    "QRCM2_6",
    "QRCM2_7",
    "QRCM2_8"
    ),names(test3))
  
  # Create Severity Vector
  for(b in 1:nrow(test3)) {
  for(i in 1:length(severity_list)){

      for(a in severity_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
        Severity <- c(Severity, test3[b,a])
        }
      }
    }
  }
  
  
  # Create Duration Vector
  for(b in 1:nrow(test3)) {
  for(i in 1:length(duration_list)) {

      for(a in duration_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
          Duration <- c(Duration, test3[b,a])
        }
      }
    }
  }
  
  
  # Create Frequency Vector
  for(b in 1:nrow(test3)) {
  for(i in 1:length(frequency_list)) {
      for(a in frequency_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
          Frequency <- c(Frequency, test3[b,a])
        }
      }
    }
  }
  
  
  # Create Season Vector
  for(b in 1:nrow(test3)) { 
  for(i in 1:length(season_list)) {
      for(a in season_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
          Season <- c(Season, test3[b,a])
        }
      }
    }
  }
  
  
  # Create TimeofDay Vector
  for(b in 1:nrow(test3)) {
  for(i in 1:length(timeofday_list)) {
      for(a in timeofday_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
          TimeofDay <- c(TimeofDay, test3[b,a])
        }
      }
    }
  }
  
  
  # Create Weekday Vector
  for(b in 1:nrow(test3)) {
  for(i in 1:length(weekday_list)) {
      for(a in weekday_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
          Weekday <- c(Weekday, test3[b,a])
        }
      }
    }
  }
  
  
  # Create Discounts Vector
  for(b in 1:nrow(test3)) {
  for(i in 1:length(discounts_list)) {
      for(a in discounts_list[[i]]) {
        if(is.na(test3[b,a])) {
          NA
        } else {
          Discounts <- c(Discounts, test3[b,a])
        }
      }
    }
  }
  
  
  # Create State vector
  
  for(a in 1:nrow(test3)) {
    for(b in state_cols) {
      for(i in 1:length(severity_list)) {
        if(is.na(test3[a,b])) {
          NA
        } else {
          State <- c(State, test3[a,b])
        }
      }
    }
  }
  
  # Create Climate Zone vector
  
  for(a in 1:nrow(test3)) {
    for(b in climate_col) {
      for(i in 1:length(severity_list)) {
          Climate_Zone <- c(Climate_Zone, test3[a,b])
      }
    }
  }  
  
  # Create Remoteness vector
  
  for(a in 1:nrow(test3)) {
    for(b in remote_col) {
      for(i in 1:length(severity_list)) {
        Remoteness <- c(Remoteness, test3[a,b])
      }
    }
  }  
  
  # Create Block and Choiceset Vector
  
  for(a in 1:nrow(test3)) {
    for(b in block_cols) {
      for(i in 1:length(severity_list)) {
#        if(is.na(test3[a,b])) {
#          NA
#        } else {
          Block <- c(Block, test3[a,b])
          Choiceset <- c(Choiceset, ceiling(i/3))
#        }
      }
    }
  }
  
  
  # Create RESPID Vector
  
  for(a in 1:nrow(test3)) {
    for(b in 1) {
      for(i in 1:length(severity_list)) {
        if(is.na(test3[a,b])) {
          NA
        } else {
          RESPID <- c(RESPID, test3[a,b])
        }
      }
    }
  }
  
  
  # Create Residential/Business Flag Vector
  
  for(a in 1:nrow(test3)) {
    for(b in block_cols) {
      for(i in 1:length(severity_list)) {
          if (!is.na(test3[a,b]) == TRUE) {
          Flag <- c(Flag, c("Residential"))
        } else {
          Flag <- c(Flag, c("Business"))
        }
      }
    }
  }
  
  
  # Create Response Vector
  
  for(a in 1:nrow(test3)) {
    for(b in response_list) {
        if(is.na(test3[a,b])) {
          NA
        } else if (test3[a,b] == 1) {
          Response <- c(Response, c(1,0,0))
          Response_Index <- c(Response_Index, c(1,2,3))
        } else if (test3[a,b] == 2) {
          Response <- c(Response, c(0,1,0))
          Response_Index <- c(Response_Index, c(1,2,3))
        } else {
          Response <- c(Response, c(0,0,1))
          Response_Index <- c(Response_Index, c(1,2,3))
        }
    }
  }


# Create Output DataFrame using all relevant vectors

# Mapped Output
#rm(output)
output <- data.frame(RESPID, Flag, State, Severity, Duration, Frequency, Season, TimeofDay, Weekday, Discounts, Block, Choiceset, Response, Climate_Zone)

#Add choice number
#output$choice <- head(rep(1:3, ceiling(nrow(data_input)/3)),nrow(data_input))

# Raw Output
output_raw <- data.frame(
  RESPID, 
  Flag, 
  State, 
  Severity, 
  Duration, 
  Frequency, 
  Season, 
  TimeofDay, 
  Weekday, 
  Discounts, 
  Block, 
  Choiceset, 
  Response,
  Climate_Zone,
  Remoteness)
output_raw$choice <- head(rep(1:3, ceiling(nrow(output_raw)/3)),nrow(output_raw))
output_raw$choiceID <- paste(output_raw$RESPID, output_raw$Block, output_raw$Choiceset, sep="-")
output_raw$discount_num <- ifelse(output_raw$Discounts == "No Change", 0,
                                  ifelse(output_raw$Discounts == "Discount Level 1", 3,
                                         ifelse(output_raw$Discounts == "Discount Level 2", 7,
                                                ifelse(output_raw$Discounts == "Discount Level 3", 15, -1))))
output_raw$sq_outage <- ifelse(output_raw$choice==1, "yes","no")

write_csv(output_raw, "output_transformed_data.csv")


# CHECKS

# Check length of vectors (should be number of rows * 24) - each row of data produces 8 blocks * 3 choices = 24 rows of output data 
# Check unique values in all columns

checks <- list(RESPID, Flag, State, Severity, Duration, Frequency, Season, TimeofDay, Weekday, Discounts, Block, Choiceset, Response)

for (check in checks) {
  print(length(check))
  print(unique(check))
}

# Check that every 24th row of the output is the same
output_check <- output[seq(1, nrow(output), 24), 4:10]

# EXPLORATORY WORK

# Total respondents by State

Total_Respondents <- output %>% 
                      group_by(State, Flag) %>%
                        summarise(Total_respondents = n_distinct(RESPID))
write_csv(Total_Respondents, "output_respondent_by_state.csv")

# Distribution of Responses

response_dist <- data.frame(Response, Response_Index) %>% 
  group_by(Response_Index) %>%
    count(Response)

write_csv(response_dist, "output_response_dist.csv")


