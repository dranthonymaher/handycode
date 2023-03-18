####################################################################
#Project: 
#Description: 

#Owner: Garry Singh
#Date Created: 20 May 2019
#Last Modified: 

####################################################################

# Install packages to read data
# install.packages("readxl")
# install.packages("readr")
# install.packages('dplyr')
list.of.packages <- c(
  "readxl", 
  "readr", 
  "dplyr",
  "corrplot"
  )

#Set path
data_path <- "C:/Users/amaher2/OneDrive - KPMG/Documents/Choice Modelling"
input_data <- "ORD-401222-G0H2 Electricity Consumer and B2B Final Data_17May_v1.xlsx"
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
  holdvalue <- myCodeframe[1,1]
  for(i in 1:nrow(myCodeframe)) {
    if(myCodeframe[i,2]==1) {
      holdvalue = myCodeframe[i,1] 
    } else {
      myCodeframe[i,1] = holdvalue
    }
  }
  head(myCodeframe)
  
  # Create myLabels df
  myLabels <- read.csv(file="Label.csv")
  head(myLabels)
  # Rename columns and remove first 2 rows from myLabels
  names(myLabels) <- c("Variable", "Label")
  myLabels <- myLabels[-c(1,2),]
  head(myLabels)
  
  # Create myData df 
  myData <- read.csv(file="Data.csv", header=TRUE)
  head(myData)
  # Keep only the relevant columns for analysis
  # relevantcols = c(152,153,154,155,177,178,179,180,202,203,204,205,227,228,229,230,
  #                 252,253,254,255,277.278,279,280,302,303,304,305,327,328,329,330,352,
  #                 492,493,494,495,517,518,519,520,542,543,544,545,567,568,569,570,
  #                 592,593,594,595,617,618,619,620,642,643,644,645,667,668,669,670,692,
  #                 701,702,703,704,726,727,728,729,751,752,753,754,776,777,778,779,
  #                 801,802,803,804,826,827,828,829,851,852,853,854,876,877,878,879,901) 
  
  # AM comment -how were these selected??
  relevantcols = c(1,18,151:351,374,479,491:691,700:900)
  
  test3 <- subset(myData, select = relevantcols)
  head(test3)
  # AM checkS
  i<-4
  j<-7
  toString(subset(myCodeframe, myCodeframe$Value==colnames(test3[j]) & myCodeframe$Indicator==test3[i,j])[3][[1]])
  
  # 
  for(j in c(2,7:27,32:52,57:77,82:102,107:127,132:152,157:177,182:202,204,
             210:230,235:255,260:280,285:305,310:330,335:355,360:380,385:405,
             411:431,436:456,461:481,486:506,511:531,536:556,561:581,586:606)) {
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
  
  # Initialise Lists
  
  duration_list <- list()
  frequency_list <- list()
  season_list <- list()
  timeofday_list <- list()
  weekday_list <- list()
  discounts_list <- list()
  
  # Create lists 
  # Most lists are based off the severity list
  
  severity_list <- list(c(7:9),c(32:34),c(57:59),c(82:84),c(107:109),c(132:134),c(157:159),c(182:184),
                        c(210:212),c(235:237),c(260:262),c(285:287),c(310:312),c(335:337),c(360:362),c(385:387),
                        c(411:413),c(436:438),c(461:463),c(486:488),c(511:513),c(536:538),c(561:563),c(586:588))
  
  # do not run more than once:
  for(i in 1:length(severity_list)) {
    duration_list <- append(duration_list, list(severity_list[[i]]+c(3:3)))
    frequency_list <- append(frequency_list, list(severity_list[[i]]+c(6:6)))
    season_list <- append(season_list, list(severity_list[[i]]+c(9:9)))
    timeofday_list <- append(timeofday_list, list(severity_list[[i]]+c(12:12)))
    weekday_list <- append(weekday_list, list(severity_list[[i]]+c(15:15)))
    discounts_list <- append(discounts_list, list(severity_list[[i]]+c(18:18)))
  }
  
  state_cols <- c(2,204)
  block_cols <- c(3,206,407)
  climate_col <- c(205)
  response_list <- c(28,53,78,103,128,153,178,203,
                     231,256,281,306,331,356,381,406,
                     432,457,482,507,532,557,582,607)
  # 
  # newdf<-test3[,c(1,4,5,6)]
  # head(newdf)
  # library(reshape)
  # 
  # df2<-melt(newdf,id = ("RESPID"))
  # head(df2)
  # df2<-df2[with(df2, order(RESPID,variable)), ]
  # 
  
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
#        if(is.na(test3[a,b])) {
#          NA
#        } else {
          Climate_Zone <- c(Climate_Zone, test3[a,b])
#        }
      }
    }
  }  
  
  # Create Block and Choiceset Vector
  
  for(a in 1:nrow(test3)) {
    for(b in block_cols) {
      for(i in 1:length(severity_list)) {
        if(is.na(test3[a,b])) {
          NA
        } else {
          Block <- c(Block, test3[a,b])
          Choiceset <- c(Choiceset, ceiling(i/3))
        }
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
        if(is.na(test3[a,b])) {
          NA
        } else if (b == 3) {
          Flag <- c(Flag, c("Business"))
        } else if (b == 206) {
          Flag <- c(Flag, c("Residential 1"))
        } else {
          Flag <- c(Flag, c("Residential 2"))
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
length(RESPID)
length(Flag)
summary(Discounts)



# Mapped Output
#rm(output)
output <- data.frame(RESPID, Flag, State, Severity, Duration, Frequency, Season, TimeofDay, Weekday, Discounts, Block, Choiceset, Response, Climate_Zone)

#Add choice number
#output$choice <- head(rep(1:3, ceiling(nrow(data_input)/3)),nrow(data_input))

# Raw Output
output_raw <- data.frame(RESPID, Flag, State, Severity, Duration, Frequency, Season, TimeofDay, Weekday, Discounts, Block, Choiceset, Response,Climate_Zone)
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
output_check
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


