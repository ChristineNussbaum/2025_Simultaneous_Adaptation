##########################################################################
## File: 00_Exp3_data_preparation.R
## Data Preparation for Exp 3: Adaptation of Emotion - pseudowords
# author: Christine Nussbaum
# date 03/2025, updates 12/2025

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load required packages
library("tidyverse")

# load relevant functions
source("functions/loadPTKExperimentData.R")
source("functions/mySummary.R") 


#---------------------------------------------------------------------------------
#get the raw data:
D <- loadPTKExperimentData(relDirPath = "input/raw/")

#number of participants
print(paste("There are currently", length(unique(D$Subject)), "participants loaded."))

#reorder columns - Subject Variablen zuerst
D <- D[,c(17,16,15, 1:14)]

#---------------------------------------------------------------------------------
#Data preparation


#[1] Split the data in the Baseline and the Adaptation-Block

E3_Bline <- D[,-c(12:17)] %>% filter(V1 == "Baseline")

E3_Adapt <- D %>% filter(V1 == "Antwort")


#[2] Rename Variables
names(E3_Bline) <- c("Participant", "Date", "Experiment","Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp","RT" )
names(E3_Adapt) <- c("Participant", "Date", "Experiment", "Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp", "RT", "AdaptType", "Blockorder", "TopUp", "z", "Topup1", "Topup2")

#[3] Adjust some settings of variables
E3_Bline$Resp <- as.numeric(E3_Bline$Resp)
E3_Adapt$Resp <- as.numeric(E3_Adapt$Resp)

#sort out all the counting variables responsible for the top-up E3_Adaptation: 
E3_Adapt$z <- ifelse(E3_Adapt$TopUp != 0, NA, E3_Adapt$z)
E3_Adapt$Topup1 <- ifelse(E3_Adapt$TopUp != 0, NA, E3_Adapt$Topup1)
E3_Adapt$Topup2 <- ifelse(E3_Adapt$TopUp != 0, NA, E3_Adapt$Topup2)

E3_Adapt$TopUp <- ifelse(E3_Adapt$TopUp == 0, "Yes", NA) #Code if this was a TopUp Trial


#recode the Response for both datasets
E3_Bline$Resp <- E3_Bline$Resp -1 # umkodieren
E3_Adapt$Resp <- E3_Adapt$Resp -1 # umkodieren


#recode AdaptType

E3_Adapt$AdaptType <- recode(E3_Adapt$AdaptType, 
                             "pw12_angry/pw35_fearful" = "pw12_ang/pw34_fea",
                             "pw35_angry/pw12_fearful" = "pw12_fea/pw34_ang")



#---------------------------------------------------------------------------------
#Check if everything is loaded correctly

#[1] N different stimuli per participant 
table(E3_Bline$Participant) #should be 112 per participant
table(E3_Adapt$Participant) #should be 224 per participant

#[2]Is every stimulus picked exactly once? - Baseline

#code a stimulus variable
E3_Bline$Stimulus <- str_c(E3_Bline$SpID, E3_Bline$Word, E3_Bline$tML, sep = "_")
table(table(E3_Bline$Stimulus, E3_Bline$Participant)) #should give out only "1"s

#[3]Is every stimulus picked exactly twice? - Adaptation
E3_Adapt$Stimulus <- str_c(E3_Adapt$SpID, E3_Adapt$Word, E3_Adapt$tML, sep = "_")
table(table(E3_Adapt$Stimulus, E3_Adapt$Participant)) #should give out only "2"s

#---------------------------------------------------------------------------------
#Check if some participants only pressed one key during a whole block
# if so -> remove

Check_Bline <- E3_Bline %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one
Check_Adapt <- E3_Adapt %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one

#"59095ef5" has too many missing and must be removed
E3_Adapt <- E3_Adapt %>% filter(Participant != "59095ef5")
E3_Bline <- E3_Bline %>% filter(Participant != "59095ef5")


#"501e2935" has too many missing and must be removed
E3_Adapt <- E3_Adapt %>% filter(Participant != "501e2935")
E3_Bline <- E3_Bline %>% filter(Participant != "501e2935")


#"1d53fae5" has too many missing and must be removed
E3_Adapt <- E3_Adapt %>% filter(Participant != "1d53fae5")
E3_Bline <- E3_Bline %>% filter(Participant != "1d53fae5")


#we remove three participant who reported problems with stimulus playback (in the free text comments)
#"26021dea", "678c9205", "3d663343"

E3_Adapt <- E3_Adapt %>% filter(Participant != "26021dea")
E3_Bline <- E3_Bline %>% filter(Participant != "26021dea")

E3_Adapt <- E3_Adapt %>% filter(Participant != "678c9205")
E3_Bline <- E3_Bline %>% filter(Participant != "678c9205")

E3_Adapt <- E3_Adapt %>% filter(Participant != "3d663343")
E3_Bline <- E3_Bline %>% filter(Participant != "3d663343")

rm(Check_Bline, Check_Adapt)

#---------------------------------------------------------------------------------
#Save datasets
save(E3_Bline, E3_Adapt, file ="input/Exp3_raw_data.RData")


rm(D)
##################################################################################
##################################################################################
##################################################################################

#---------------------------------------------------------------------------------
# survey data preparation

#load raw survey data
S3 <- read.csv(file ="input/data_Exp3.csv")

#rename participant
S3$participant <- substr(S3$participant, 31,38)

#keep only the ones with an experimental file
S3 <- S3 %>% filter(participant %in% unique(E3_Adapt$Participant)) # check: N = 42

S3$VPN_Code <- paste0(S3$LPartCode_1, S3$LPartCode_2, S3$LPartCode_3, S3$LPartCode_4, S3$LPartCode_5, S3$LPartCode_6)

#rename a few variables
S3 <- S3 %>% select(!c(intro_question1_1,
                     TIME_start, 
                     TIME_end, 
                     CLIENT_start,
                     LPartCode_1,
                     LPartCode_2,
                     LPartCode_3,
                     LPartCode_4,
                     LPartCode_5,
                     LPartCode_6,
                     StartRatingExperiment_1)) %>% 
  rename(LAge = LAge_1,
         LSex = LSex_1,
         LMotherLanguage = LMotherLanguage_1,
         LStudyWork = LStudyWork_1,
         LHearingKown = LHearingDisabilitiesKnown_1,
         LHearingKind = LHearingDisabilitiesKind_1,
         LHearingImp = LHearingDisabilitiesConstraint_1,
         Anmerkungen = Anmerkungen_1,
         InstructionsClear = afterExp2InstructionsClear_1,
         Strategy = afterExp4Strategy_1, 
         Eval1 = afterExp3RatingEvaluation_1,
         Eval2 = afterExp3RatingEvaluation_2,
         Eval3 = afterExp3RatingEvaluation_3,
         Eval4 = afterExp3RatingEvaluation_4,
         Eval5 = afterExp3RatingEvaluation_5,
         Eval6 = afterExp3RatingEvaluation_6,
         Eval7 = afterExp3RatingEvaluation_7,
         Eval8 = afterExp3RatingEvaluation_8)


#save survey
save(S3, file="input/Exp3_survey.RData")


##End of Script