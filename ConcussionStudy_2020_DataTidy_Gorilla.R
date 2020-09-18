##### Data tidying script for Gorilla Questionnaire and Task Data
## Creatine & Concussion Project
## 09/04/2020
## Philip Dean; University of Surrey, p.dean@surrey.ac.uk

######### To run in console:
# setwd("C:/Users/username/OneDrive - University of Surrey/UG_Students/Concussion_Creatine_2019/Data/Concussion_Gorilla")
# source('Concussion_Gorilla_DataTidy.R')
##########

# This script loads .csv short form, unblinded forms downloaded from Gorilla using the 4-letter/number unique ID (UID)
# at the end of the filename.
# There are 6 questionnaires and two tasks in this study, with the tasks randomised in order 
# (meaning that there are two UID for each task depending on whether it came first or second) 
# In addition, there is a consent form, used here to check participants consented to all (9) points
# To "merge" the different data files together, it uses the Participant.Public.ID as a commone variable/column

# For this data, there was: 
# 1. A consent form,  
# 2. Demographic Questionnaire, 3. Head Injury Questionnaire, 4. Sports Questionnaire, 5. Diet/Supplementation Questionnare,
# 6. Rivermead Post-Concussion Qeuestionnaire (RPQ), 7. Cognitive Failures Questionnaire (CFQ)
# 8. n-Back Task (one with nback first, one with nback second)
# 9. Attentional Network Task (ANT, one with nback first, one with nback second)

# An additional consideration was that the Gorilla Project was changed halway through, so that some participants data 
# was collected with an earlier version (v19) of the project, and some with a modified version (v20). 
# v19 had a pre and post design. Due to circumstances, the post couldnt be collected, 
# v20 was therefore a version with just the pre-testing. All files in Pre were the same as v20, except the debrief 
# having two versions of a project meant that there were double the usual amount of files, 
# and that the v19 and v20 files needed to be stitched together

# The outputs of this script are:
# 1. Participant_Key_COMPLETED.csv: This has all the key data for participants that completed the study
# 2. Participant_Key_STARTED.csv: This has all the key data for participants that started the study
# 3. FULL_combineddata.csv: This is all the data collected across the questionnaires and tasks. RPQ & CFQ only show Total Score
# 4. FULL_combineddata_Quantised.csv: this is the same as above but instead of string data it uses the .quantised data (number data)
# 5. DemographicsOnly_data.csv: THis is the data from the questionnaires except RPQ & CFQ
# 6. AnalysisOnly_data.csv: This is the data with the key variables for analysis picked out. 
# Which are written into the same directory as the downloaded csv files from Gorilla they are made from

#################################################
######################START OF CODE##############
#################################################


# Clears variables in global environment
rm(list=ls())

# Set main working directory to folder within all your separate CSV files
setwd("C:/Users/username/OneDrive - University of Surrey/UG_Students/Concussion_Creatine_2019/Data/csv files")

#Load packages
library(plyr)
library(tidyverse)

#############
### Load Data
############

# Combine different versions of same questionnaires when experiment has been updated (e.g. from V19 to V20), 
# so all participants are in one sheet. 

# First, bring in the names of the csv files into R from their 4 digit code
#Questionnaires
Consent_F <- list.files(pattern="*-ib1v.csv")
Demog_F <-  list.files(pattern="*-fmsx.csv")
HeadInj_F <-  list.files(pattern="*-vef7.csv")
Sports_F <-  list.files(pattern="*-6cws.csv")
DietSupp_F <-  list.files(pattern="*-2utx.csv")
RPQ_F <-  list.files(pattern="*-uxak.csv")
CFQ_F <-  list.files(pattern="*-1kji.csv")
Debrief_F <- c(list.files(pattern="*-37h8.csv"), list.files(pattern="*-2gyq.csv"))
#Tasks
nbackFirst_F <-  list.files(pattern="*-xhjh.csv")
nbackSecond_F <-  list.files(pattern="*-l1in.csv")
ANTFirst_F <-  list.files(pattern="*-attn.csv")
ANTSecond_F <-  list.files(pattern="*-swhf.csv")

nback_F <- c(nbackFirst_F, nbackSecond_F)
ANT_F <- c(ANTFirst_F, ANTSecond_F)


# Second, open these files and bind them together (row by row) 
Consent<-do.call("rbind.fill",lapply(Consent_F,read.csv,header=TRUE,na.strings = c(""))) 
Demog<-do.call("rbind.fill",lapply(Demog_F,read.csv,header=TRUE,na.strings = c(""))) 
HeadInj<-do.call("rbind.fill",lapply(HeadInj_F,read.csv,header=TRUE,na.strings = c(""))) 
Sports<-do.call("rbind.fill",lapply(Sports_F,read.csv,header=TRUE,na.strings = c(""))) 
DietSupp<-do.call("rbind.fill",lapply(DietSupp_F,read.csv,header=TRUE,na.strings = c(""))) 
RPQ<-do.call("rbind.fill",lapply(RPQ_F,read.csv,header=TRUE,na.strings = c(""))) 
CFQ<-do.call("rbind.fill",lapply(CFQ_F,read.csv,header=TRUE,na.strings = c(""))) 
Debrief<-do.call("rbind.fill",lapply(Debrief_F,read.csv,header=TRUE,na.strings = c(""))) 
nback<-do.call("rbind.fill",lapply(nback_F,read.csv,header=TRUE,na.strings = c(""))) 
ANT<-do.call("rbind.fill",lapply(ANT_F,read.csv,header=TRUE,na.strings = c(""))) 

# Remove initial file list values/variables from memory
rm(Consent_F, Demog_F, HeadInj_F, Sports_F, DietSupp_F, RPQ_F, CFQ_F, Debrief_F, nbackFirst_F, nbackSecond_F, ANTFirst_F, ANTSecond_F, nback_F, ANT_F)

###########################
# Make Key File & Save it
##########################
Key_File1 <- select(Consent, Participant.Public.ID:Participant.Private.ID, Schedule.ID, Experiment.ID:Experiment.Version,
                    Participant.Device.Type:Participant.Viewport.Size, Participant.Status, branch.5zfl:randomiser.m6bv, 
                    UTC.Date)
Key_File1<-Key_File1[complete.cases(Key_File1[ , 1]),]
Key_File2 <- select(Debrief, Schedule.ID:Participant.Private.ID, UTC.Date, X2_PreSeasonFeedback, X1_Feedback)
colnames(Key_File2)[colnames(Key_File2) == "UTC.Date"] <- "UTC.Date.End"
Key_File2<-Key_File2[complete.cases(Key_File2[ , 1]),]
Key_File <- merge(Key_File1, Key_File2, by=c("Participant.Public.ID", "Participant.Private.ID"), all=TRUE)
Key_File<-Key_File[complete.cases(Key_File[ , 1]),]

write.csv(Key_File,"Participant_Key_COMPLETED.csv",row.names=FALSE)
write.csv(Key_File1,"Participant_Key_STARTED.csv",row.names=FALSE)

# Remove no longer needed Key Files
rm(Key_File1, Key_File2)


###############################
# Picking out variables/data of interest from Questionnaires
##############################

#################
###### This is a stubexample of how to clean the data of not-needed data from the files #######
# Consent_Tidied <- select(Consent, -(UTC.Timestamp:Schedule.ID), -(Participant.Starting.Group:Checkpoint), -randomiser.wo6j)
##################

###
# Consent Scoring adds up consent - should equal 9.
#########
Consent_Scoring <- select(Consent, contains("_Consent")) # pick out the consent buttons
Consent_Scoring <- data.frame(Consent.Total = t(t(rowSums(Consent_Scoring))))
Consent_Final<-cbind(select(Consent, "Participant.Public.ID","Participant.Private.ID", "randomiser.m6bv"), Consent_Scoring)
colnames(Consent_Final) <- c("Subject_No","Private_ID", "Task_First", "Consent_Total")
Consent_Final<-Consent_Final[complete.cases(Consent_Final[ , 1]),]

###
# Demographic Data
########### 
Demog_Final <- select(Demog, "Participant.Public.ID",
                      "X1_Age", "X3_Gender",  "X14_HeadInjury_YesNo", "X4_Occupation", "X5_Handedness", "X6_Education", 
                      "X6_Education.text", "X7a_Alcohol_YesNo", "X7b_Alcohol_Amount", "X8a_Medication_YesNo", 
                      "X8b_Medication_Type", "X9a_NeuroConditions_YesNo", "X9b_NeuroConditions_Type", "X10_Dyslexia", 
                      "X11_ChronicPain", "X12_Eyesight", "X13_Hearing")
Demog_Final_Quant <- select(Demog, "Participant.Public.ID",
                           "X1_Age", "X3_Gender.quantised",  "X14_HeadInjury_YesNo.quantised", "X4_Occupation", "X5_Handedness.quantised", "X6_Education.quantised", 
                           "X6_Education.text", "X7a_Alcohol_YesNo.quantised", "X7b_Alcohol_Amount", "X8a_Medication_YesNo.quantised", 
                           "X8b_Medication_Type", "X9a_NeuroConditions_YesNo.quantised", "X9b_NeuroConditions_Type", "X10_Dyslexia.quantised", 
                           "X11_ChronicPain.quantised", "X12_Eyesight.quantised", "X13_Hearing.quantised")
colnames(Demog_Final) <- c("Subject_No","Age", "Gender", "Head_Injury", "Occupation", "Handedness", 
                           "Education", "Education_Other ", "Drink_Alcohol", "Alcohol_Amount", "Take_Medication", "Med_Type",
                           "Neuro_Condition", "NeuroCond_Type", "Dyslexia", "Chronic_Pain", "Eyesight", "Hearing")
colnames(Demog_Final_Quant) <- c("Subject_No","Age", "Gender", "Head_Injury", "Occupation", "Handedness", 
                           "Education", "Education_Other ", "Drink_Alcohol", "Alcohol_Amount", "Take_Medication", "Med_Type",
                           "Neuro_Condition", "NeuroCond_Type", "Dyslexia", "Chronic_Pain", "Eyesight", "Hearing")
Demog_Final<-Demog_Final[complete.cases(Demog_Final[ , 1]),]
Demog_Final_Quant<-Demog_Final_Quant[complete.cases(Demog_Final_Quant[ , 1]),]

###
# Head Injury Data
########### 
HeadInj_Final <- select(HeadInj, "Participant.Public.ID",
                        "X15a_HowManyInjury_YesNo", "X15b_HowManyInjury_Amount", "X16a_TenDays", "X16b_TenDays",
                        "X17a_MajorInjury_YesNo", "X17b_MajorInjury_Amount", "X18_WhenInjury.year", "X18_WhenInjury.month",
                        "X19_DizzinessConfuse", "X20a_LossConc_YesNo", "X20b_LossConc_Amount", "X21a_Amnesia_YesNo", 
                        "X21b_Amnesia_Amount", "X22_HowInjure", "X23_WhereInjure.1", "X23_WhereInjure.2", "X23_WhereInjure.3",
                        "X23_WhereInjure.4", "X23_WhereInjure.other", "X23_WhereInjure.text", "X24_LinearRotate", 
                        "X24_LinearRotate.text")
HeadInj_Final_Quant <- select(HeadInj, "Participant.Public.ID",
                        "X15a_HowManyInjury_YesNo.quantised", "X15b_HowManyInjury_Amount", "X16a_TenDays.quantised", "X16b_TenDays",
                        "X17a_MajorInjury_YesNo", "X17b_MajorInjury_Amount", "X18_WhenInjury.year", "X18_WhenInjury.month",
                        "X19_DizzinessConfuse.quantised", "X20a_LossConc_YesNo.quantised", "X20b_LossConc_Amount", "X21a_Amnesia_YesNo.quantised", 
                        "X21b_Amnesia_Amount", "X22_HowInjure", "X23_WhereInjure.1", "X23_WhereInjure.2", "X23_WhereInjure.3",
                        "X23_WhereInjure.4", "X23_WhereInjure.other", "X23_WhereInjure.text", "X24_LinearRotate.quantised", 
                        "X24_LinearRotate.text")
colnames(HeadInj_Final) <- c("Subject_No", "MoreThanOne_HeadInj", "HowMany_HeadInj", "Within10Days", 
                             "HowManyWithin10", "MajorHeadInj", "HowManyMajorInj", "WhenInj_Year", "WhenInj_Month", 
                             "DizzinesConfuse", "LossConc", "LossConc_Length", "Amnesia", "Amnesia_Length", "HowInjure", 
                             "Inj_Front", "Inj_Back", "Inj_Left", "Inj_Right", "Inj_Other", "Inj_Other_where", "LinearRotate",
                             "LinearRotate_Other")
colnames(HeadInj_Final_Quant) <- c("Subject_No", "MoreThanOne_HeadInj", "HowMany_HeadInj", "Within10Days", 
                             "HowManyWithin10", "MajorHeadInj", "HowManyMajorInj", "WhenInj_Year", "WhenInj_Month", 
                             "DizzinesConfuse", "LossConc", "LossConc_Length", "Amnesia", "Amnesia_Length", "HowInjure", 
                             "Inj_Front", "Inj_Back", "Inj_Left", "Inj_Right", "Inj_Other", "Inj_Other_where", "LinearRotate",
                             "LinearRotate_Other")
HeadInj_Final<-HeadInj_Final[complete.cases(HeadInj_Final[ , 1]),]
HeadInj_Final_Quant<-HeadInj_Final_Quant[complete.cases(HeadInj_Final_Quant[ , 1]),]

###
# Sports Data
########### 
Sports_Final <- select(Sports, "Participant.Public.ID",
                       "X1_PlayRugby", "X2_Team_Rugby", "X3_HeadInj_Rugby", "X4_HowLong_Rugby", "X5_Position_Rugby", 
                       "X6_PlaySport", "X7_SportType_HeadHit.1", "X7_SportType_HeadHit.2", "X7_SportType_HeadHit.3", 
                       "X7_SportType_HeadHit.4", "X7_SportType_HeadHit.5", "X8_RegularSport_HeadHit", "X9_RegularSport_Team",
                       "X10_HeadInj_HeadHitSport", "X11_HowLongSport_HeadHit", "X12_SportPosition_HeadHit", 
                       "X13_SportType_NonHeadHit.1", "X13_SportType_NonHeadHit.2", "X13_SportType_NonHeadHit.3", 
                       "X13_SportType_NonHeadHit.4", "X13_SportType_NonHeadHit.5", "X13_SportType_NonHeadHit.6",
                       "X13_SportType_NonHeadHit.7", "X13_SportType_NonHeadHit.8", "X13_SportType_NonHeadHit.other",
                       "X13_SportType_NonHeadHit.text", "X14_RegularSport_NonHeadHit", "X14_RegularSport_NonHeadHit.text",
                       "X15_TeamSport_NonHeadHit", "X16_HowLongSport_NonHeadHit")
Sports_Final_Quant <- select(Sports, "Participant.Public.ID",
                       "X1_PlayRugby.quantised", "X2_Team_Rugby.quantised", "X3_HeadInj_Rugby.quantised", "X4_HowLong_Rugby.quantised", "X5_Position_Rugby.quantised", 
                       "X6_PlaySport.quantised", "X7_SportType_HeadHit.1", "X7_SportType_HeadHit.2", "X7_SportType_HeadHit.3", 
                       "X7_SportType_HeadHit.4", "X7_SportType_HeadHit.5", "X8_RegularSport_HeadHit.quantised", "X9_RegularSport_Team.quantised",
                       "X10_HeadInj_HeadHitSport.quantised", "X11_HowLongSport_HeadHit.quantised", "X12_SportPosition_HeadHit", 
                       "X13_SportType_NonHeadHit.1", "X13_SportType_NonHeadHit.2", "X13_SportType_NonHeadHit.3", 
                       "X13_SportType_NonHeadHit.4", "X13_SportType_NonHeadHit.5", "X13_SportType_NonHeadHit.6",
                       "X13_SportType_NonHeadHit.7", "X13_SportType_NonHeadHit.8", "X13_SportType_NonHeadHit.other",
                       "X13_SportType_NonHeadHit.text", "X14_RegularSport_NonHeadHit.quantised", "X14_RegularSport_NonHeadHit.text",
                       "X15_TeamSport_NonHeadHit.quantised", "X16_HowLongSport_NonHeadHit.quantised")
colnames(Sports_Final) <- c("Subject_No", "PlayRugby_Regular", "TeamRugby_Regular", "HeadInj_Rugby", 
                            "HowLong_Rugby", "Position_Rugby", "NonRugby_Sport", "Play_AmFo_Reg", "Play_Foot_Reg", 
                            "Boxing_Reg", "MMA_Reg", "Taekwondo_Reg", "MostReg_HeadHitSport", "HeadHitSport_Team", 
                            "HeadInj_HeadHitSport", "HowLong-HeadHitSport", "Position_HeadHitSport", "Athletics_Reg", 
                            "Triathlon_Reg", "Tennis_Reg", "Squash_Reg", "Badmington_Reg", "Cycling_Reg", "Rowing_Reg",
                            "Swimming_Reg", "OtherSport_Reg", "OtherSport_Text", "MostReg_NonHeadHit", "MostReg_NonHeadHit_Text",
                            "NonHeadHit_Team", "HowLong_NonHeadHit")
colnames(Sports_Final_Quant) <- c("Subject_No", "PlayRugby_Regular", "TeamRugby_Regular", "HeadInj_Rugby", 
                            "HowLong_Rugby", "Position_Rugby", "NonRugby_Sport", "Play_AmFo_Reg", "Play_Foot_Reg", 
                            "Boxing_Reg", "MMA_Reg", "Taekwondo_Reg", "MostReg_HeadHitSport", "HeadHitSport_Team", 
                            "HeadInj_HeadHitSport", "HowLong-HeadHitSport", "Position_HeadHitSport", "Athletics_Reg", 
                            "Triathlon_Reg", "Tennis_Reg", "Squash_Reg", "Badmington_Reg", "Cycling_Reg", "Rowing_Reg",
                            "Swimming_Reg", "OtherSport_Reg", "OtherSport_Text", "MostReg_NonHeadHit", "MostReg_NonHeadHit_Text",
                            "NonHeadHit_Team", "HowLong_NonHeadHit")
Sports_Final<-Sports_Final[complete.cases(Sports_Final[ , 1]),]
Sports_Final_Quant<-Sports_Final_Quant[complete.cases(Sports_Final_Quant[ , 1]),]

###
# Diet & Supplementaion Data
########### 
DietSupp_Final <- select(DietSupp, "Participant.Public.ID",
                       "X1_CreatineYesNo", "X2_OtherSupplements.1", "X2_OtherSupplements.2", "X2_OtherSupplements.3",
                       "X2_OtherSupplements.4", "X2_OtherSupplements.5", "X2_OtherSupplements.6", "X2_OtherSupplements.7",
                       "X2_OtherSupplements.8", "X2_OtherSupplements.9", "X2_OtherSupplements.10", "X2_OtherSupplements.11",
                       "X2_OtherSupplements.12", "X2_OtherSupplements.13", "X2_OtherSupplements.14", "X2_OtherSupplements.15",
                       "X2_OtherSupplements.16", "X2_OtherSupplements.17", "X2_OtherSupplements.18", "X2_OtherSupplements.19",
                       "X2_OtherSupplements.other", "X2_OtherSupplements.text", "X3_EatMeat", "X4_Meat_HowOften", "X5_EatFish",
                       "X6_Fish_HowOften", "X7_EatDairy", "X8_Dairy_HowOften", "X9_Dairy_Vegan", "X10_ProteinSource",
                       "X10_ProteinSource.text", "X11_Coffee", "X11_SmokeYesNo", "X12_DiabetesYesNo")
DietSupp_Final_Quant <- select(DietSupp, "Participant.Public.ID",
                         "X1_CreatineYesNo.quantised", "X2_OtherSupplements.1", "X2_OtherSupplements.2", "X2_OtherSupplements.3",
                         "X2_OtherSupplements.4", "X2_OtherSupplements.5", "X2_OtherSupplements.6", "X2_OtherSupplements.7",
                         "X2_OtherSupplements.8", "X2_OtherSupplements.9", "X2_OtherSupplements.10", "X2_OtherSupplements.11",
                         "X2_OtherSupplements.12", "X2_OtherSupplements.13", "X2_OtherSupplements.14", "X2_OtherSupplements.15",
                         "X2_OtherSupplements.16", "X2_OtherSupplements.17", "X2_OtherSupplements.18", "X2_OtherSupplements.19",
                         "X2_OtherSupplements.other", "X2_OtherSupplements.text", "X3_EatMeat.quantised", "X4_Meat_HowOften.quantised", "X5_EatFish.quantised",
                         "X6_Fish_HowOften.quantised", "X7_EatDairy.quantised", "X8_Dairy_HowOften.quantised", "X9_Dairy_Vegan.quantised", "X10_ProteinSource.quantised",
                         "X10_ProteinSource.text", "X11_Coffee.quantised", "X11_SmokeYesNo.quantised", "X12_DiabetesYesNo.quantised")

colnames(DietSupp_Final) <- c("Subject_No", "Take_Creatine", "Protein_Supp", "Glutamine", 
                              "BCAA_Amino", "Citrulline", "Arginine", "Beta-Alanine", "Caffeine_Supp", "Ginseng", 
                              "Ginkgo_Biloba", "Iron", "Ribose", "Quercetin", "Beetroot", "FishOil_Omega3", "VitaminC", 
                              "VitaminE", "CoEnzymeQ10", "Acetyl_L_Carnitine", "Antioxidant_Other", "Other_Supp", 
                              "OtherSupp_Text", "Eat_Meat", "EatMeat_HowOften", "Eat_Fish", "EatFish_HowOften", "Eat_Dairy", 
                              "EastDairy_HowOften", "Dairy_Vegan", "MainProteinSource", "ProteinSource_Other", "CoffeeTea_Day", "Smoke", "Diabetes")
colnames(DietSupp_Final_Quant) <- c("Subject_No", "Take_Creatine", "Protein_Supp", "Glutamine", 
                              "BCAA_Amino", "Citrulline", "Arginine", "Beta-Alanine", "Caffeine_Supp", "Ginseng", 
                              "Ginkgo_Biloba", "Iron", "Ribose", "Quercetin", "Beetroot", "FishOil_Omega3", "VitaminC", 
                              "VitaminE", "CoEnzymeQ10", "Acetyl_L_Carnitine", "Antioxidant_Other", "Other_Supp", 
                              "OtherSupp_Text", "Eat_Meat", "EatMeat_HowOften", "Eat_Fish", "EatFish_HowOften", "Eat_Dairy", 
                              "EastDairy_HowOften", "Dairy_Vegan", "MainProteinSource", "ProteinSource_Other", "CoffeeTea_Day", "Smoke", "Diabetes")
DietSupp_Final<-DietSupp_Final[complete.cases(DietSupp_Final[ , 1]),]
DietSupp_Final_Quant<-DietSupp_Final_Quant[complete.cases(DietSupp_Final_Quant[ , 1]),]


###############################
# Picking out KEY VARIABLES of interest from cleaned data
##############################

Key_Demog <-select(Demog_Final, "Subject_No","Age", "Gender", "Head_Injury", "Occupation", "Handedness", 
                   "Education", "Education_Other ", "Alcohol_Amount", "Take_Medication", "Med_Type",
                   "Neuro_Condition", "NeuroCond_Type", "Dyslexia", "Chronic_Pain")

Key_HeadInj <-select(HeadInj_Final,"Subject_No", "MoreThanOne_HeadInj", "HowMany_HeadInj", "Within10Days", 
                     "HowManyWithin10", "MajorHeadInj", "HowManyMajorInj", "WhenInj_Year", "WhenInj_Month", 
                     "DizzinesConfuse", "LossConc", "LossConc_Length", "Amnesia", "Amnesia_Length", "HowInjure", 
                     "LinearRotate")

Key_Sports <-select(Sports_Final,"Subject_No", "PlayRugby_Regular", "TeamRugby_Regular", "HeadInj_Rugby", 
                    "HowLong_Rugby", "Position_Rugby", "NonRugby_Sport", "Play_AmFo_Reg", "Play_Foot_Reg", 
                    "Boxing_Reg", "MMA_Reg", "Taekwondo_Reg", "MostReg_HeadHitSport", "HeadHitSport_Team", 
                    "HeadInj_HeadHitSport", "HowLong-HeadHitSport", "Position_HeadHitSport", "MostReg_NonHeadHit", "MostReg_NonHeadHit_Text",
                    "NonHeadHit_Team", "HowLong_NonHeadHit")

Key_DietSupp <-select(DietSupp_Final,"Subject_No", "Take_Creatine", "Eat_Meat", "EatMeat_HowOften", "Eat_Fish", "EatFish_HowOften", "Eat_Dairy", 
                      "EastDairy_HowOften", "CoffeeTea_Day", "Smoke", "Diabetes")


###############################
# Doing Analysis in CFQ and RPQ Data to get Total Scores
##############################

###
# CFQ Scoring
########### 
CFQ_Scoring<-select(CFQ, ends_with(".quantised")) # pick out the quantised questionnaire data
CFQ_Scoring<-((CFQ_Scoring-5)*-1) # change scale from 1-5 (in raw data) to 4-0 (needed for CFQ)
CFQ_Scoring<-data.frame(CFQ.Total = t(t(rowSums(CFQ_Scoring))))
CFQ_Final<-cbind(select(CFQ, "Participant.Public.ID"), CFQ_Scoring)
colnames(CFQ_Final) <- c("Subject_No", "CFQ_Total")
CFQ_Final<-CFQ_Final[complete.cases(CFQ_Final[ , 1]),]

###
# RPQ Scoring
########### 
RPQ_Scoring<-select(RPQ, ends_with(".quantised")) # pick out the quantised questionnaire data
RPQ_Scoring<-(RPQ_Scoring-1) # change scale from 1-5 (in raw data) to 0-4 (needed for RPQ)
RPQ_Scoring<-data.frame(RPQ.Total = t(t(rowSums(RPQ_Scoring[ ,1:16]))), 
                   RPQ.Other = t(t(rowSums(RPQ_Scoring[ ,17:18], na.rm=TRUE))), 
                   RPQ.Special = t(t(RPQ_Scoring[ ,19]))) # The first 16 variables for RPQ (RPQ Total), then the "Other" Total, Then the "Special Circumstance" (Yes/No)
RPQ_Final<-cbind(select(RPQ, "Participant.Public.ID"), RPQ_Scoring)
colnames(RPQ_Final) <- c("Subject_No", "RPQ_Total", "RPQ_Other", "RPQ.Special")
RPQ_Final<-RPQ_Final[complete.cases(RPQ_Final[ , 1]),]

# Remove scoring data.frames no longer needed
rm(Consent_Scoring, CFQ_Scoring, RPQ_Scoring)


###############################
# Task (nBack & ANT) Data analysis to get variables of interest
##############################

###
# nBack Scoring
########### 

# Find participant names
participants_nback<-as.character(levels(nback$Participant.Public.ID))
# Take out those with incomplete data
participants_nback_remove1 <- c("p126")
participants_nback_remove2 <- c("p143")
participants_nback<-participants_nback[!participants_nback == participants_nback_remove1]
participants_nback<-participants_nback[!participants_nback == participants_nback_remove2]

#create a data frame with required variables to be filled in with analysis
nback_Final<-data.frame(Subject_No = t(t(participants_nback)))
nback_Final$Error2Back_T = NA
nback_Final$Error2Back_NT = NA
nback_Final$RT2Back_T = NA
nback_Final$RT2Back_NT = NA
nback_Final$Error4Back_T = NA
nback_Final$Error4Back_NT = NA
nback_Final$RT4Back_T = NA
nback_Final$RT4Back_NT = NA

# Loop through all the participants
for(i in 1:length(participants_nback)){
  
  # get individual subjects data
  nback.subj<-subset(nback, nback$Participant.Public.ID==participants_nback[i])
  
  # Find beginning and end of blocks
  begin.back2<-which(nback.subj$display=="Begin_Block_2")
  begin.back4<-which(nback.subj$display=="Begin_Block_4")
  end.back<-which(nback.subj$display=="End_Block")
  block.start.end<-sort(c(begin.back2, begin.back4, end.back))
  
  
  begin.end.back2<-block.start.end[sort(c(match(begin.back2, block.start.end), 
                                          (match(begin.back2, block.start.end)+1)))]
  begin.end.back4<-block.start.end[sort(c(match(begin.back4, block.start.end), 
                          (match(begin.back4, block.start.end)+1)))]
  
  # Use beginning and end of block to cut out just the blocks wanted
  # 2 back
  back2.allblock<-rbind(nback.subj[begin.end.back2[1]:begin.end.back2[2], ], 
                        nback.subj[begin.end.back2[3]:begin.end.back2[4], ], 
                        nback.subj[begin.end.back2[5]:begin.end.back2[6], ], 
                        nback.subj[begin.end.back2[7]:begin.end.back2[8], ], 
                        nback.subj[begin.end.back2[9]:begin.end.back2[10], ])
  # 4 back
  back4.allblock<-rbind(nback.subj[begin.end.back4[1]:begin.end.back4[2], ], 
                        nback.subj[begin.end.back4[3]:begin.end.back4[4], ], 
                        nback.subj[begin.end.back4[5]:begin.end.back4[6], ], 
                        nback.subj[begin.end.back4[7]:begin.end.back4[8], ], 
                        nback.subj[begin.end.back4[9]:begin.end.back4[10], ])
  
  # Get rid of NA rows what throw out analysis
  back2.allblock<-back2.allblock[!is.na(back2.allblock$Answer), ]
  back4.allblock<-back4.allblock[!is.na(back4.allblock$Answer), ]
  
  # Find target (Yes) and non target (No) button presses only
  back2.allblock_T<-back2.allblock[back2.allblock$Answer=="Yes" & back2.allblock$Zone.Name=="button", ]
  back2.allblock_NT<-back2.allblock[back2.allblock$Answer=="No" & back2.allblock$Zone.Name=="button", ]
  back4.allblock_T<-back4.allblock[back4.allblock$Answer=="Yes" & back4.allblock$Zone.Name=="button", ]
  back4.allblock_NT<-back4.allblock[back4.allblock$Answer=="No" & back4.allblock$Zone.Name=="button", ]
  
  # Inputting the analused variables into the data frame
  nback_Final$Error2Back_T[i] = 100-(((sum(back2.allblock_T$Correct, na.rm=TRUE))/50)*100)
  nback_Final$Error2Back_NT[i] = 100-(((sum(back2.allblock_NT$Correct, na.rm=TRUE))/100)*100)
  nback_Final$RT2Back_T[i] = mean(as.numeric(back2.allblock_T[back2.allblock_T$Correct==1, "Reaction.Time"]))
  nback_Final$RT2Back_NT[i] = mean(as.numeric(back2.allblock_NT[back2.allblock_NT$Correct==1, "Reaction.Time"]))
  nback_Final$Error4Back_T[i] = 100-(((sum(back4.allblock_T$Correct, na.rm=TRUE))/50)*100)
  nback_Final$Error4Back_NT[i] = 100-(((sum(back4.allblock_NT$Correct, na.rm=TRUE))/100)*100)
  nback_Final$RT4Back_T[i] = mean(as.numeric(back4.allblock_T[back4.allblock_T$Correct==1, "Reaction.Time"]))
  nback_Final$RT4Back_NT[i] = mean(as.numeric(back4.allblock_NT[back4.allblock_NT$Correct==1, "Reaction.Time"]))

  # Remove no longer needed nback data frames
  rm(nback.subj, begin.back2, begin.back4, end.back, block.start.end, begin.end.back2, begin.end.back4, 
     back2.allblock, back4.allblock, back2.allblock_T, back2.allblock_NT, back4.allblock_T, back4.allblock_NT)
  
}

# Remove no longer needed nback variables 
rm(i, participants_nback, participants_nback_remove1, participants_nback_remove2)


###
# ANT Scoring
########### 

# Find participnt names
participants_ANT<-as.character(levels(ANT$Participant.Public.ID))
# Take out those with incomplete data
participants_ANT_remove1 <- c("p130")
participants_ANT<-participants_ANT[!participants_ANT == participants_ANT_remove1]

#create a data frame with required variables to be filled in with analysis
ANT_Final<-data.frame(Subject_No = t(t(participants_ANT)))
ANT_Final$ErrCong_ALL = NA
ANT_Final$RTCong_ALL = NA
ANT_Final$ErrIncong_ALL = NA
ANT_Final$RTIncong_ALL = NA
ANT_Final$ErrNeu_ALL = NA
ANT_Final$RTNeu_ALL = NA
ANT_Final$Err_NoCue_ALL = NA
ANT_Final$RT_NoCue_ALL = NA
ANT_Final$Err_CentreCue_ALL = NA
ANT_Final$RT_CentreCue_ALL = NA
ANT_Final$Err_DoubleCue_ALL = NA
ANT_Final$RT_DoubleCue_ALL = NA
ANT_Final$Err_SpatialCue_ALL = NA
ANT_Final$RT_SpatialCue_ALL = NA
ANT_Final$RT_Altering = NA
ANT_Final$RT_Orienting = NA
ANT_Final$RT_Exec = NA
ANT_Final$Err_Altering = NA
ANT_Final$Err_Orienting = NA
ANT_Final$Err_Exec = NA

# Loop through all the participants
for(i in 1:length(participants_ANT)){
  
  # get individual subjects data
  ANT.subj<-subset(ANT, ANT$Participant.Public.ID==participants_ANT[i])
  
  # Find beginning and end of blocks
  begin.ANT<-which(ANT.subj$display=="Begin_Block")
  end.ANT<-which(ANT.subj$display=="End_Block")
  
  # Use beginnign and end of block to cut out just the blocks wanted
  ANT.allblock<-ANT.subj[begin.ANT[1]:end.ANT[length(end.ANT)], ]
  
  # Find Congruent, Incongruent and Neutral reponses only
  ANT.cong<-ANT.allblock[ANT.allblock$Type=="Congruent" & ANT.allblock$Zone.Name=="Response", ]
  ANT.incong<-ANT.allblock[ANT.allblock$Type=="Incongruent" & ANT.allblock$Zone.Name=="Response", ]
  ANT.neu<-ANT.allblock[ANT.allblock$Type=="Neutral" & ANT.allblock$Zone.Name=="Response", ]
  
  # Find No Cue, Centre cue, double cue and spatial cue reponses only
  # look at centre (cue_fixation cross), top (Cue1), or bottom (Cue2) asterisks
  # Spatial cue look at top cue, bottom cue, then adds these together
  ANT.nocue<-ANT.allblock[ANT.allblock$Cue_Fixation_Cross=="fixation_cross_grey.png" & 
                            ANT.allblock$Cue1=="blank_asterisk_grey.png" &
                            ANT.allblock$Cue2=="blank_asterisk_grey.png" &
                            ANT.allblock$Zone.Name=="Response", ]
  ANT.centrecue<-ANT.allblock[ANT.allblock$Cue_Fixation_Cross=="asterisk_grey.png" &
                                ANT.allblock$Cue1=="blank_asterisk_grey.png" &
                                ANT.allblock$Cue2=="blank_asterisk_grey.png" &
                                ANT.allblock$Zone.Name=="Response", ]
  ANT.doublecue<-ANT.allblock[ANT.allblock$Cue_Fixation_Cross=="fixation_cross_grey.png" & 
                                ANT.allblock$Cue1=="asterisk_grey.png" &
                                ANT.allblock$Cue2=="asterisk_grey.png" &
                                ANT.allblock$Zone.Name=="Response", ]  
  ANT.spatialcuetop<-ANT.allblock[ANT.allblock$Cue_Fixation_Cross=="fixation_cross_grey.png" & 
                                    ANT.allblock$Cue1=="asterisk_grey.png" &
                                    ANT.allblock$Cue2=="blank_asterisk_grey.png" &
                                    ANT.allblock$Zone.Name=="Response", ] 
  ANT.spatialcuebottom<-ANT.allblock[ANT.allblock$Cue_Fixation_Cross=="fixation_cross_grey.png" & 
                                    ANT.allblock$Cue1=="blank_asterisk_grey.png" &
                                    ANT.allblock$Cue2=="asterisk_grey.png" &
                                    ANT.allblock$Zone.Name=="Response", ] 
  # make spatial cue (all) from spatial cue top and spatial cue bottom. 
  ANT.spatialcue<-rbind(ANT.spatialcuetop, ANT.spatialcuebottom)
  
  # Get rid of NA rows what throw out analysis
  ANT.cong<-ANT.cong[!is.na(ANT.cong$Response), ]
  ANT.incong<-ANT.incong[!is.na(ANT.incong$Response), ]
  ANT.neu<-ANT.neu[!is.na(ANT.neu$Response), ]
  ANT.nocue<-ANT.nocue[!is.na(ANT.nocue$Response), ]
  ANT.centrecue<-ANT.centrecue[!is.na(ANT.centrecue$Response), ]
  ANT.doublecue<-ANT.doublecue[!is.na(ANT.doublecue$Response), ]
  ANT.spatialcue<-ANT.spatialcue[!is.na(ANT.spatialcue$Response), ]
  
  # Inputting the analused variables into the data frame
  ANT_Final$ErrCong_ALL[i] = 100-(((sum(ANT.cong$Correct, na.rm=TRUE))/64)*100)
  ANT_Final$RTCong_ALL[i] = mean(as.numeric(ANT.cong[ANT.cong$Correct==1, "Reaction.Time"]))
  ANT_Final$ErrIncong_ALL[i] = 100-(((sum(ANT.incong$Correct, na.rm=TRUE))/64)*100)
  ANT_Final$RTIncong_ALL[i] = mean(as.numeric(ANT.incong[ANT.incong$Correct==1, "Reaction.Time"]))
  ANT_Final$ErrNeu_ALL[i] = 100-(((sum(ANT.neu$Correct, na.rm=TRUE))/64)*100)
  ANT_Final$RTNeu_ALL[i] = mean(as.numeric(ANT.neu[ANT.neu$Correct==1, "Reaction.Time"]))
  ANT_Final$Err_NoCue_ALL[i] = 100-(((sum(ANT.nocue$Correct, na.rm=TRUE))/48)*100)
  ANT_Final$RT_NoCue_ALL[i] = mean(as.numeric(ANT.nocue[ANT.nocue$Correct==1, "Reaction.Time"]))
  ANT_Final$Err_CentreCue_ALL[i] = 100-(((sum(ANT.centrecue$Correct, na.rm=TRUE))/48)*100)
  ANT_Final$RT_CentreCue_ALL[i] = mean(as.numeric(ANT.centrecue[ANT.centrecue$Correct==1, "Reaction.Time"]))
  ANT_Final$Err_DoubleCue_ALL[i] = 100-(((sum(ANT.doublecue$Correct, na.rm=TRUE))/48)*100)
  ANT_Final$RT_DoubleCue_ALL[i] = mean(as.numeric(ANT.doublecue[ANT.doublecue$Correct==1, "Reaction.Time"]))
  ANT_Final$Err_SpatialCue_ALL[i] = 100-(((sum(ANT.spatialcue$Correct, na.rm=TRUE))/48)*100)
  ANT_Final$RT_SpatialCue_ALL[i] = mean(as.numeric(ANT.spatialcue[ANT.spatialcue$Correct==1, "Reaction.Time"]))
  ANT_Final$RT_Altering[i] = ANT_Final$RT_NoCue_ALL[i] - ANT_Final$RT_DoubleCue_ALL[i]
  ANT_Final$RT_Orienting[i] = ANT_Final$RT_CentreCue_ALL[i] - ANT_Final$RT_SpatialCue_ALL[i]
  ANT_Final$RT_Exec[i] = ANT_Final$RTIncong_ALL[i] - ANT_Final$RTCong_ALL[i]
  ANT_Final$Err_Altering[i] = ANT_Final$Err_NoCue_ALL[i] - ANT_Final$Err_DoubleCue_ALL[i]
  ANT_Final$Err_Orienting[i] = ANT_Final$Err_CentreCue_ALL[i] - ANT_Final$Err_SpatialCue_ALL[i]
  ANT_Final$Err_Exec[i] = ANT_Final$ErrIncong_ALL[i] - ANT_Final$ErrCong_ALL[i]
  
  # Remove no longer needed ANT data frames
  rm(ANT.subj, begin.ANT, end.ANT, ANT.allblock, ANT.cong, ANT.incong, ANT.neu, ANT.nocue, ANT.centrecue,
     ANT.doublecue, ANT.spatialcuetop, ANT.spatialcuebottom, ANT.spatialcue)
  
}

# Remove no longer needed ANT variables
rm(i, participants_ANT, participants_ANT_remove1)

# Remove no longer needed data frames from initial import
rm(ANT, CFQ, Consent, Debrief, Demog, DietSupp, HeadInj, Key_File, nback, RPQ, Sports)


###############################
# Combine all the analysed and cleaned data togather into one data frame
##############################

# NOW, combine questionnaires together across columns using merge (and Reduce), so all questionnaires in same file
Full.Data <-   Reduce(function(x, y) merge(x, y, by="Subject_No", all=TRUE), 
                      list(Consent_Final, Demog_Final, HeadInj_Final, Sports_Final, DietSupp_Final, 
                           RPQ_Final, CFQ_Final, nback_Final, ANT_Final))
Full.Quant.Data <-   Reduce(function(x, y) merge(x, y, by="Subject_No", all=TRUE), 
                      list(Consent_Final, Demog_Final_Quant, HeadInj_Final_Quant, Sports_Final_Quant, DietSupp_Final_Quant, 
                           RPQ_Final, CFQ_Final, nback_Final, ANT_Final))
Demog.Data <- Reduce(function(x, y) merge(x, y, by="Subject_No", all=TRUE), 
                     list(Consent_Final, Demog_Final, HeadInj_Final, Sports_Final, DietSupp_Final))
Analysis.Data <- Reduce(function(x, y) merge(x, y, by="Subject_No", all=TRUE), 
                        list(Consent_Final, Key_Demog, Key_HeadInj, Key_Sports, Key_DietSupp, 
                             RPQ_Final, CFQ_Final, nback_Final, ANT_Final))

###############################
# Write out finished, cleaned & analysed data into csv files
##############################
#This line exports your combined data as a CSV. This new CSV will appear in your working directory
write.csv(Full.Data,"FULL_combineddata.csv",row.names=FALSE)
write.csv(Full.Quant.Data,"FULL_combineddata_Quantised.csv",row.names=FALSE)
write.csv(Demog.Data,"DemographicsOnly_data.csv",row.names=FALSE)
write.csv(Analysis.Data,"AnalysisOnly_data.csv",row.names=FALSE)