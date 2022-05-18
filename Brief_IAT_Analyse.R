##### Data tidying script for Gorilla Questionnaire and Task Data
## Brief IAT Project
## 18/05/2022
## Philip Dean; University of Surrey, p.dean@surrey.ac.uk

######### To run in console:
# 1. Set directiory to where you have put this script:
#     setwd("C:/Users/username/OneDrive - University of Surrey/Supervision/UG_Students/2020/Brief_IAT/Brief_IAT_Analyse")
# 2. Then run it:
#     source('Brief_IAT_Analyse.R')
##########

## This script loads .csv short form, unblinded forms downloaded from Gorilla using the 4-letter/number unique ID (UID)
# at the end of the filename.
# There is only one task in this study

## This imports, tidies and analyses the data as per guidelines in Nosek et al 2014 (DOI:10.1371/journal.pone.0110938).
#  1. REMOVES FIRST FOUR TRIALS OF BLOCK; 2. KEEP ERROR TRIALS (sourcing RT for subsequent correct response);
#  3. REMOVES HIGH RT TRIALS; 4. COUNT NUMBER OF TRIALS WITH FAST RESPONSE & MARK PARTICIPANTS WITH LOT OF FAST RESPONSE
#  5. RECODE HIGH AND LOW RT; 6. CALCULATE D VALUE
# NB: Also Marks particicpants with low trial number after cleaning
# NB: 6. CALCULATE D VALUE can be done by Nosek way (Analysis_choice <- "Nosek") of doing it block by block (2-1; 4-3, then average these)
# Or by doing this at condition level ((2+4)-(1+3)), and can be changed by altering Analysis_choice <- "Adam"

## A note on D Calculation:
#   D is (M2 - M1)/SD in the script, so positive number = bias to M2, negative number ,bias to M1. 
#   Sriram & Greenwald 2009 (DOI: 10.1027/1618-3169.56.4.283):
#     e.g. Coke-Pepsi/pleasant-(unpleasant) order of listing indicates interpretation of scores
#     High scores (>0) represent greater association of first-listed category with the third.
#     i.e. that Coke-pleasant association is stronger than Pepsi-pleasant association

## The output of this script is:
# 1. BIAT_Analysed_Data.csv: 
  # This has Participant ID, Order of conditions, Total trials, 
  # trials used in analysis (after trim extreme reaction times), Whether participants have at least 58 trials,
  # whether participants have at least 90% trials >300ms RT (too fast response), D, errors, and RT errors (too slow)
  # Means & STD  for M1 and M2 in formula for D, Means & STD across blocks and for all blocks individually, 
  # D for block 1 & 2 (D1), D for Block 3 & 4 (D2) as used in Nosek et al 2014 analysis, 
  # and STD for blocks 1 & 2, and blocks 3 & 4. 


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
#Adam_highRTThreshold = 5000   # From Adam analysis. in ms
Nosek_highRTThreshold = 10000  # From Nosek et al 2014, in ms
Nosek_FastRT = 300            # From Nosek analysis, in ms
lowRTBound = 400              # From Nosek analysis. in ms
tooFastRTProportion = 10      # From Nosek analysis, >10% trials with fast RT
highRTBound = 2000            # From Nosek analysis. in ms
minTrials = 58                # From Adam Analysis, Maximum = 64. This is trials after taking out high RT

# Block and Spreadsheet Names
# D is always (M2 - M1)/SD, so positive number = bias to M2, negative number ,bias to M1. 
  # Sriram & Greenwald 2009: e.g. Coke-Pepsi/pleasant-(unpleasant) order of listing indicates interpretation of scores
  # High scores (>0) represent greater association of first-listed category with the third.
  # i.e. that Coke-pleasant association is stronger than Pepsi-pleasant association
  # So example below is Lower Class - Upper Class / pleasant - (unpleasant)
M1_A_name <- "Upper_Class"
M2_B_name <- "Lower_Class"
AB <- "Class_AB"
BA <- "Class_BA"

## Which Analysis to use
# "Nosek" = Nosek et al 2014 recommended guidelines
# "Adam" = analysis used in BIAT task in University of Surrey (for consistency)
Analysis_choice <- "Nosek"

#############
### Load Data
############


# First, bring in the names of the csv files into R from their 4 digit code
#BIAT <- read.csv(file="Example_Data.csv", header = TRUE, na.strings = "")
BIAT_F <-  list.files(pattern="*-xhjh.csv")

# Second, open these files and bind them together (row by row) 
BIAT<-do.call("rbind.fill",lapply(BIAT_F,read.csv,header=TRUE,na.strings = c(""))) 

# Remove initial file list values/variables from memory
rm(BIAT_F)


###
# Brief IAT Scoring
########### 

# Find participant names
participants_BIAT_F<-as.factor(BIAT$Participant.Public.ID)
participants_BIAT<-as.character(levels(participants_BIAT_F))
#participants_BIAT<-as.character(BIAT$Participant.Public.ID[1])
#participants_BIAT<-as.character(levels(BIAT$Participant.Private.ID))

# Remove initial participants list
rm(participants_BIAT_F)


#create a data frame with required variables to be filled in with analysis
BIAT_Final<-data.frame(Participant_No = t(t(participants_BIAT)))
BIAT_Final$Order = NA
BIAT_Final$Trial_No = NA
BIAT_Final$Trial_No_Used = NA
BIAT_Final$UseData_Trial = NA
BIAT_Final$UseData_RT = NA
BIAT_Final$D = NA
BIAT_Final$Errors = NA
BIAT_Final$RT_Errors = NA
BIAT_Final$M1_Mean = NA
BIAT_Final$M1_SD = NA
BIAT_Final$M2_Mean = NA
BIAT_Final$M2_SD = NA
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
BIAT_Final$D1 = NA
BIAT_Final$D2 = NA
BIAT_Final$B1_2_SD = NA
BIAT_Final$B3_4_SD = NA


# Loop through all the participants
for(i in 1:length(participants_BIAT)){
  
  # get individual participants data
  BIAT.subj<-subset(BIAT, BIAT$Participant.Public.ID==participants_BIAT[i])
  #BIAT.subj<-subset(BIAT, BIAT$Participant.Private.ID==participants_BIAT[i])
  
  ## Find blocks ##
  # 1. Also REMOVES FIRST FOUR TRIALS OF BLOCK, 
  # as these are coded as e.g. "1_start" so not found here
  block1.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="1")
  block2.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="2")
  block3.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="3")
  block4.16<-subset(BIAT.subj, BIAT.subj$Block_Type=="4")
  
  ## Tidy Data ##
  # Find Spreadsheet type/order of blocks (Class_AB = Upper then Lower)
  blockOrder<-as.character(block1.16$Spreadsheet.Name[1])
  
  
  # Get rid of NA rows that cause error in analysis
  block1.16<-block1.16[!is.na(block1.16$Answer), ]
  block2.16<-block2.16[!is.na(block2.16$Answer), ]
  block3.16<-block3.16[!is.na(block3.16$Answer), ]
  block4.16<-block4.16[!is.na(block4.16$Answer), ]

  
  ## Find rows where participant pressed correct button ##
    # 2. KEEP ERROR TRIALS (this just finds RT for correct response, keeping all trials)
    # What this does by looking for correct = 1 is takes the data where participant responded correctly
    # This means that if respond incorrectly first, then this code ignores this, and takes data from 
    # subsequent correct response
     
  block1.16_Correct<- block1.16[ block1.16$Correct=="1" &  block1.16$Zone.Name=="ResponseZone", ]
  block2.16_Correct<- block2.16[ block2.16$Correct=="1" &  block2.16$Zone.Name=="ResponseZone", ]
  block3.16_Correct<- block3.16[ block3.16$Correct=="1" &  block3.16$Zone.Name=="ResponseZone", ]
  block4.16_Correct<- block4.16[ block4.16$Correct=="1" &  block4.16$Zone.Name=="ResponseZone", ]
    
  ## Making a combined "ALL-Block" block for untrimmed data ##
    # This creates a log of the whole "raw data" block before trimming for extreme reaction times
  blockAll_Correct<-bind_rows(block1.16_Correct, block2.16_Correct, block3.16_Correct, block4.16_Correct)
  tot_Trials<-nrow(blockAll_Correct)
  
  ## 3. REMOVE HIGH RT TRIALS (keeps all trials <= threshold [same as removing >threshold])
  block1.16_Correct<-subset(block1.16_Correct, block1.16_Correct$Reaction.Time<=Nosek_highRTThreshold)
  block2.16_Correct<-subset(block2.16_Correct, block2.16_Correct$Reaction.Time<=Nosek_highRTThreshold)
  block3.16_Correct<-subset(block3.16_Correct, block3.16_Correct$Reaction.Time<=Nosek_highRTThreshold)
  block4.16_Correct<-subset(block4.16_Correct, block4.16_Correct$Reaction.Time<=Nosek_highRTThreshold)  
  
  ## 4. COUNT NUMBER OF TRIALS WITH LOT OF FAST RESPONSE
  NumFastResp<-length(which(blockAll_Correct$Reaction.Time<Nosek_FastRT))
  PercentFastResp<-((NumFastResp/tot_Trials)*100)
  
  ## 5. RECODE HIGH AND LOW RT
  # Recode > High RT as HighRT, < Low RT as LowRT
  block1.16_Correct$Reaction.Time[block1.16_Correct$Reaction.Time>highRTBound]=highRTBound
  block2.16_Correct$Reaction.Time[block2.16_Correct$Reaction.Time>highRTBound]=highRTBound
  block3.16_Correct$Reaction.Time[block3.16_Correct$Reaction.Time>highRTBound]=highRTBound
  block4.16_Correct$Reaction.Time[block4.16_Correct$Reaction.Time>highRTBound]=highRTBound
  
  block1.16_Correct$Reaction.Time[block1.16_Correct$Reaction.Time<lowRTBound]=lowRTBound
  block2.16_Correct$Reaction.Time[block2.16_Correct$Reaction.Time<lowRTBound]=lowRTBound
  block3.16_Correct$Reaction.Time[block3.16_Correct$Reaction.Time<lowRTBound]=lowRTBound
  block4.16_Correct$Reaction.Time[block4.16_Correct$Reaction.Time<lowRTBound]=lowRTBound

  
  ## Making a combined "ALL-Block" block for trimmed data ##
    # This is the "cleaned" block using Nosek et al 2014 guidelines
  blockAll_Correct_Trimmed<-bind_rows(block1.16_Correct, block2.16_Correct, block3.16_Correct, block4.16_Correct)
  tot_Trials_Trimmed<-nrow(blockAll_Correct_Trimmed)
  
  ## Mark Participants with too few trials or too many fast trials as "Bad" - Use Data = No in logfile ##
    
   # if they have less than minTrials due to incomplete data and/or after throwing out too slow RT
  if(tot_Trials_Trimmed>=minTrials){
    subj.include = 'yes'
  } else {
    subj.include = 'no'
  }
  
   ## 4. MARK PARTICIPANTS WITH FAST RESPONSE ##
      # if they have > tooFastRTProportion (%) fast trials (<Nosek_FastRT) 
  if(PercentFastResp<=tooFastRTProportion){
    subj.includeRT = 'yes'
  } else {
    subj.includeRT = 'no'
  }
  
  
  ## 6. CALCULATE D VALUE ##
    # D = ([M2 - M1]/SD)
    # Nosek et al: Compute D separately for each pair of two consecutive blocks separately, then average
    # Adam: Compute D once across all blocks, where SD is across ALL RT, and M2/M1 are average RT for each condition (e.g ((B1 + B3) - (B2 + B4))/SD(B1 + B2 + B3 + B4))
  
  # Making combined condition blocks ("Block 1 & 3"; "Block 2 & 4"; For Nosek: "Block 1 & 2"; "Block 3 & 4") 
  block.1.3<-bind_rows(block1.16_Correct, block3.16_Correct)
  block.2.4<-bind_rows(block2.16_Correct, block4.16_Correct)
  block.1.2<-bind_rows(block1.16_Correct, block2.16_Correct)
  block.3.4<-bind_rows(block3.16_Correct, block4.16_Correct)
  
  # Variables to use
  pooledMean<-mean(blockAll_Correct_Trimmed$Reaction.Time)
  pooledSTD<-sd(blockAll_Correct_Trimmed$Reaction.Time)
  B1_B2_STD<-sd(block.1.2$Reaction.Time) 
  B3_B4_STD<-sd(block.3.4$Reaction.Time)
  
  # NOSEK ANALYSIS
  if(Analysis_choice == "Nosek"){
    
    # Change M1 & M2 dependent on block order in spreadsheet used. 
    if(blockOrder==AB){
      D1=((mean(block2.16_Correct$Reaction.Time)-mean(block1.16_Correct$Reaction.Time))/B1_B2_STD)
      D2=((mean(block4.16_Correct$Reaction.Time)-mean(block3.16_Correct$Reaction.Time))/B3_B4_STD)
      D=((D1+D2)/2)

      M1.mean<-mean(block.1.3$Reaction.Time)
      M2.mean<-mean(block.2.4$Reaction.Time)
      M1.sd<-sd(block.1.3$Reaction.Time)
      M2.sd<-sd(block.2.4$Reaction.Time)
      
    } else if (blockOrder==BA){
      D1=((mean(block1.16_Correct$Reaction.Time)-mean(block2.16_Correct$Reaction.Time))/B1_B2_STD)
      D2=((mean(block3.16_Correct$Reaction.Time)-mean(block4.16_Correct$Reaction.Time))/B3_B4_STD)
      D=((D1+D2)/2)

      M1.mean<-mean(block.2.4$Reaction.Time)
      M2.mean<-mean(block.1.3$Reaction.Time)
      M1.sd<-sd(block.2.4$Reaction.Time)
      M2.sd<-sd(block.1.3$Reaction.Time)
      
    }
    
  }
    

  # ADAM ANALYSIS
  if(Analysis_choice == "Adam"){
    
    # Change M1 & M2 dependent on block order in spreadsheet used. 
    if(blockOrder==AB){
      M1.mean<-mean(block.1.3$Reaction.Time)
      M2.mean<-mean(block.2.4$Reaction.Time)
      M1.sd<-sd(block.1.3$Reaction.Time)
      M2.sd<-sd(block.2.4$Reaction.Time)
      
      D1=((mean(block2.16_Correct$Reaction.Time)-mean(block1.16_Correct$Reaction.Time))/B1_B2_STD)
      D2=((mean(block4.16_Correct$Reaction.Time)-mean(block3.16_Correct$Reaction.Time))/B3_B4_STD)
      
    } else if (blockOrder==BA){
      M1.mean<-mean(block.2.4$Reaction.Time)
      M2.mean<-mean(block.1.3$Reaction.Time)
      M1.sd<-sd(block.2.4$Reaction.Time)
      M2.sd<-sd(block.1.3$Reaction.Time)
      
      D1=((mean(block1.16_Correct$Reaction.Time)-mean(block2.16_Correct$Reaction.Time))/B1_B2_STD)
      D2=((mean(block3.16_Correct$Reaction.Time)-mean(block4.16_Correct$Reaction.Time))/B3_B4_STD)
    }
    
    D=((M2.mean-M1.mean)/pooledSTD)
  }



## Calculate other interesting variables for output ##
  
  # Calculate errors
    # First, done on Raw data (all data)
      # This looks at attempt number to see if correct on first attempt: "1"       
  correctTrial.All<-length(which(blockAll_Correct$Attempt=="1"))
  errorTrial.All<-(tot_Trials - correctTrial.All)
  errorTrial.All.Percent<-((errorTrial.All/tot_Trials)*100)
     # Slow RT Error Trials
     RTerrorTrial<-(tot_Trials - tot_Trials_Trimmed)
     RTerrorTrial.Percent<-(RTerrorTrial/tot_Trials)

    # Next done on clean data ("Trimmed", so no extreme slow reaction times)  
         # This looks at attempt number to see if correct on first attempt: "1"
  correctTrial.All.Trimmed<-length(which(blockAll_Correct_Trimmed$Attempt=="1"))
  errorTrial.All.Trimmed<-(tot_Trials_Trimmed - correctTrial.All.Trimmed)
  errorTrial.All.Trimmed.Percent<-((errorTrial.All.Trimmed/tot_Trials_Trimmed)*100)


  ## CREATING THE FINAL DATA FRAME ##
   # Inputting the analysed variables into the data frame

  BIAT_Final$Order[i] = blockOrder
  BIAT_Final$Trial_No[i] = tot_Trials
  BIAT_Final$Trial_No_Used[i] = tot_Trials_Trimmed
  BIAT_Final$UseData_Trial[i] = subj.include
  BIAT_Final$UseData_RT[i] = subj.includeRT
  BIAT_Final$D[i] = D
  BIAT_Final$Errors[i] = errorTrial.All.Trimmed
  BIAT_Final$RT_Errors[i] = RTerrorTrial
  BIAT_Final$M1_Mean[i] = M1.mean
  BIAT_Final$M1_SD[i] = M1.sd
  BIAT_Final$M2_Mean[i] = M2.mean
  BIAT_Final$M2_SD[i] = M2.sd
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
  BIAT_Final$D1[i] = D1
  BIAT_Final$D2[i] = D2
  BIAT_Final$B1_2_SD[i] = B1_B2_STD
  BIAT_Final$B3_4_SD[i] = B3_B4_STD


## Clean the variable list for next run ##
  
  # Remove no longer needed participant-specific data frames
  rm(BIAT.subj, block1.16, block2.16, block3.16, block4.16, block1.16_Correct, 
     block2.16_Correct, block3.16_Correct, block4.16_Correct, blockAll_Correct, 
     blockAll_Correct_Trimmed, block.1.3, block.2.4, block.1.2, block.3.4)
  # Remove no longer needed participant-specific variables
  rm(blockOrder, tot_Trials, NumFastResp, PercentFastResp, tot_Trials_Trimmed, 
     subj.include, subj.includeRT, pooledMean, pooledSTD, B1_B2_STD, B3_B4_STD,
     D, D1, D2, M1.mean, M2.mean, M1.sd, M2.sd, correctTrial.All, errorTrial.All, 
     errorTrial.All.Percent, RTerrorTrial, RTerrorTrial.Percent, correctTrial.All.Trimmed,  
     errorTrial.All.Trimmed, errorTrial.All.Trimmed.Percent)
}

# Remove no longer needed whole-data variables 
rm(i, participants_BIAT)


###############################
# Write out finished, cleaned & analysed data into csv files
##############################
#This line exports your combined data as a CSV. This new CSV will appear in your working directory
write.csv(BIAT_Final,"BIAT_Analysed_Data.csv",row.names=FALSE)