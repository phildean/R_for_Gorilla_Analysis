##### Data tidying script for Gorilla Questionnaire and Task Data
## Brief IAT Project
## 22/01/2021
## Philip Dean; University of Surrey, p.dean@surrey.ac.uk

######### To run in console:
# setwd("C:/Users/username/OneDrive - University of Surrey/Supervision/UG_Students/2020/Brief_IAT/Brief_IAT_Analyse")
# source('Brief_IAT_Analyse.R')
##########

# This script loads .csv short form, unblinded forms downloaded from Gorilla using the 4-letter/number unique ID (UID)
# at the end of the filename.
# There is only one task in this study

# The output of this script is:
# 1. BIAT_Analysed_Data.csv: 
  # This has Participant ID, Order of conditions, Total trials, 
  # trials used in analysis (after trim extreme reaction times), D, Means & STD across blocks
  # and for all blocks individually, errors, and RT errors (too fast or slow - made from trial_no - trial_no_used)


#################################################
######################START OF CODE##############
#################################################


# Clears variables in global environment
rm(list=ls())

# Set main working directory to folder within all your separate CSV files
setwd("C:/Users/pss1pd/OneDrive - University of Surrey/Supervision/UG_Students/2020/Brief_IAT/Data/csv files")

#Load packages
library(plyr)
library(tidyverse)


#Global Variables
lowRTBound = 100     # From Adam analysis. in ms
highRTBound = 5000   # From Adam analysis. in ms
minTrials = 58       # Maximum = 64. This is trials after taking out high and low RT


#############
### Load Data
############


# First, bring in the names of the csv files into R from their 4 digit code
BIAT <- read.csv(file="Example_Data.csv", header = TRUE, na.strings = "")


###
# Brief IAT Scoring
########### 

# Find participant names
participants_BIAT<-as.character(BIAT$Participant.Private.ID[1])
#participants_BIAT<-as.character(levels(BIAT$Participant.Public.ID))
#participants_BIAT<-as.character(levels(BIAT$Participant.Private.ID))

#create a data frame with required variables to be filled in with analysis
BIAT_Final<-data.frame(Subject_No = t(t(participants_BIAT)))
BIAT_Final$Order = NA
BIAT_Final$Trial_No = NA
BIAT_Final$Trial_No_Used = NA
BIAT_Final$UseData = NA
BIAT_Final$D = NA
BIAT_Final$Overall_RT = NA
BIAT_Final$Overall_SD = NA
BIAT_Final$B1_Mean = NA
BIAT_Final$B2_Mean = NA
BIAT_Final$B3_Mean = NA
BIAT_Final$B4_Mean = NA
BIAT_Final$B1_SD = NA
BIAT_Final$B2_SD = NA
BIAT_Final$B3_SD = NA
BIAT_Final$B4_SD = NA
BIAT_Final$Errors = NA
BIAT_Final$RT_Errors = NA


# Loop through all the participants
for(i in 1:length(participants_BIAT)){
  
  # get individual subjects data
  #BIAT.subj<-subset(BIAT, BIAT$Participant.Public.ID==participants_BIAT[i])
  BIAT.subj<-subset(BIAT, BIAT$Participant.Private.ID==participants_BIAT[i])
  
  # Find blocks
  block1.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="1")
  block2.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="2")
  block3.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="3")
  block4.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="4")
  
  # Find Spreadhseet type/order of blocks (Class_AB = Upper then Lower)
  blockOrder<-as.character(block1.16$Spreadsheet.Name[1])

  # Get rid of NA rows that cause error in analysis
  block1.16<-block1.16[!is.na(block1.16$Answer), ]
  block2.16<-block2.16[!is.na(block2.16$Answer), ]
  block3.16<-block3.16[!is.na(block3.16$Answer), ]
  block4.16<-block4.16[!is.na(block4.16$Answer), ]

  # Find rows where participant pressed correct button
    # What this does by looking for correct = 1 is takes the data where participant responded correctly
    # This means that is respond incorrectly first, then this code ignores this, and takes data from 
    # subsequent correct response
  block1.16_Correct<- block1.16[ block1.16$Correct=="1" &  block1.16$Zone.Name=="ResponseZone", ]
  block2.16_Correct<- block2.16[ block2.16$Correct=="1" &  block2.16$Zone.Name=="ResponseZone", ]
  block3.16_Correct<- block3.16[ block3.16$Correct=="1" &  block3.16$Zone.Name=="ResponseZone", ]
  block4.16_Correct<- block4.16[ block4.16$Correct=="1" &  block4.16$Zone.Name=="ResponseZone", ]
    
  # Making a combined "ALL-Block" block for untrimmed data
    # This creates a log of the whole block before trimming for extreme reaction times
  blockAll_Correct<-bind_rows(block1.16_Correct, block2.16_Correct, block3.16_Correct, block4.16_Correct)
  tot_Trials<-nrow(blockAll_Correct)
  
  # Check for extreme (High or Low) Reaction Time Responses
  block1.16_Correct<-subset(block1.16_Correct, block1.16_Correct$Reaction.Time<highRTBound)
  block2.16_Correct<-subset(block2.16_Correct, block2.16_Correct$Reaction.Time<highRTBound)
  block3.16_Correct<-subset(block3.16_Correct, block3.16_Correct$Reaction.Time<highRTBound)
  block4.16_Correct<-subset(block4.16_Correct, block4.16_Correct$Reaction.Time<highRTBound)
  
  block1.16_Correct<-subset(block1.16_Correct, block1.16_Correct$Reaction.Time>lowRTBound)
  block2.16_Correct<-subset(block2.16_Correct, block2.16_Correct$Reaction.Time>lowRTBound)
  block3.16_Correct<-subset(block3.16_Correct, block3.16_Correct$Reaction.Time>lowRTBound)
  block4.16_Correct<-subset(block4.16_Correct, block4.16_Correct$Reaction.Time>lowRTBound)
 
  
  # Making a combined "ALL-Block" block for trimmed data
  blockAll_Correct_Trimmed<-bind_rows(block1.16_Correct, block2.16_Correct, block3.16_Correct, block4.16_Correct)
  
  # Making combined condition blocks ("Block 1 & 3" and "Block 2 & 4") 
  block.1.3<-bind_rows(block1.16_Correct, block3.16_Correct)
  block.2.4<-bind_rows(block2.16_Correct, block4.16_Correct)
  
  # Mark Bad subjects (if they have less than minTrials after throwing out too fast/slow RT)
  tot_Trials_Trimmed<-nrow(blockAll_Correct_Trimmed)
  if(tot_Trials_Trimmed>=minTrials){
    subj.include = 'yes'
  } else {
    subj.include = 'no'
  }

  
  # Calculate D Score (D = [M2 - M1]/SD), where SD is across ALL RT, and M2/M1 are average RT for each condition
  pooledMean<-mean(blockAll_Correct_Trimmed$Reaction.Time)
  pooledSTD<-sd(blockAll_Correct_Trimmed$Reaction.Time)
  M1.mean<-mean(block.1.3$Reaction.Time)
  M2.mean<-mean(block.2.4$Reaction.Time)

  D=((M2.mean-M1.mean)/pooledSTD)
  

  # Calculate errors
    # Trimmed data (no extreme reaction times)
        # This looks at attempt number to see if correct on first attempt "1"
  correctTrial.All.Trimmed<-length(which(blockAll_Correct_Trimmed$Attempt=="1"))
  errorTrial.All.Trimmed<-(tot_Trials_Trimmed - correctTrial.All.Trimmed)
  errorTrial.All.Trimmed.Percent<-(errorTrial.All.Trimmed/tot_Trials_Trimmed)
        # RT Error Trials
  RTerrorTrial<-(tot_Trials - tot_Trials_Trimmed)
  RTerrorTrial.Percent<-(RTerrorTrial/tot_Trials)
      # All Data (inc. extreme reaction times)
  correctTrial.All<-length(which(blockAll_Correct$Attempt=="1"))
  errorTrial.All<-(tot_Trials - correctTrial.All)
  errorTrial.All.Percent<-(errorTrial.All/tot_Trials)
  
  
  # Inputting the analysed variables into the data frame

  BIAT_Final$Order[i] = blockOrder
  BIAT_Final$Trial_No[i] = tot_Trials
  BIAT_Final$Trial_No_Used[i] = tot_Trials_Trimmed
  BIAT_Final$UseData[i] = subj.include
  BIAT_Final$D[i] = D
  BIAT_Final$Overall_RT[i] = pooledMean
  BIAT_Final$Overall_SD[i] = pooledSTD
  BIAT_Final$B1_Mean[i] <- mean(block1.16_Correct$Reaction.Time)
  BIAT_Final$B2_Mean[i] <- mean(block2.16_Correct$Reaction.Time)
  BIAT_Final$B3_Mean[i] <- mean(block3.16_Correct$Reaction.Time)
  BIAT_Final$B4_Mean[i] <- mean(block4.16_Correct$Reaction.Time)
  BIAT_Final$B1_SD[i] <- sd(block1.16_Correct$Reaction.Time)
  BIAT_Final$B2_SD[i] <- sd(block2.16_Correct$Reaction.Time)
  BIAT_Final$B3_SD[i] <- sd(block3.16_Correct$Reaction.Time)
  BIAT_Final$B4_SD[i] <- sd(block4.16_Correct$Reaction.Time)
  BIAT_Final$Errors[i] = errorTrial.All.Trimmed
  BIAT_Final$RT_Errors[i] = RTerrorTrial

  
  # Remove no longer needed participant-specific nback data frames
  rm(BIAT.subj, block.1.3, block.2.4, block1.16, block2.16, block3.16, block4.16, block1.16_Correct, 
     block2.16_Correct, block3.16_Correct, block4.16_Correct, blockAll_Correct, blockAll_Correct_Trimmed)
  # Remove no longer needed participant-specific nback variables
  rm(blockOrder, subj.include, D, pooledMean, pooledSTD, M1.mean, M2.mean, correctTrial.All, 
     correctTrial.All.Trimmed, errorTrial.All, errorTrial.All.Trimmed, errorTrial.All.Percent, 
     errorTrial.All.Trimmed.Percent, RTerrorTrial, RTerrorTrial.Percent, tot_Trials, tot_Trials_Trimmed)
}

# Remove no longer needed whole-data nback variables 
rm(i, participants_BIAT)


###############################
# Write out finished, cleaned & analysed data into csv files
##############################
#This line exports your combined data as a CSV. This new CSV will appear in your working directory
write.csv(BIAT_Final,"BIAT_Analysed_Data.csv",row.names=FALSE)