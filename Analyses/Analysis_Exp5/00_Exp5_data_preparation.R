##########################################################################
## File: 00_Exp5_data_preparation.R
## Data Preparatation for Exp 5: Adaptation of Emotion - speaker identities
# author: Christine Nussbaum
# date 03/2025

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

E5_Bline <- D[,-c(12:17)] %>% filter(V1 == "Baseline")

E5_Adapt <- D %>% filter(V1 == "Antwort")


#[2] Rename Variables
names(E5_Bline) <- c("Participant", "Date", "Experiment","Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp","RT" )
names(E5_Adapt) <- c("Participant", "Date", "Experiment", "Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp", "RT", "AdaptType", "Blockorder", "TopUp", "z", "TopupM", "TopupF")

#[3] Adjust some settings of variables
E5_Bline$Resp <- as.numeric(E5_Bline$Resp)
E5_Adapt$Resp <- as.numeric(E5_Adapt$Resp)

#sort out all the counting variables responsible for the top-up E5_Adaptation: 
E5_Adapt$z <- ifelse(E5_Adapt$TopUp != 0, NA, E5_Adapt$z)
E5_Adapt$TopupM <- ifelse(E5_Adapt$TopUp != 0, NA, E5_Adapt$TopupM)
E5_Adapt$TopupF <- ifelse(E5_Adapt$TopUp != 0, NA, E5_Adapt$TopupF)

E5_Adapt$TopUp <- ifelse(E5_Adapt$TopUp == 0, "Yes", NA) #Code if this was a TopUp Trial


#recode the Response for both datasets
E5_Bline$Resp <- E5_Bline$Resp -1 # umkodieren
E5_Adapt$Resp <- E5_Adapt$Resp -1 # umkodieren


#recode AdaptType
E5_Adapt$AdaptType <- recode(E5_Adapt$AdaptType, 
                             "HO05_angry/JW07_fearful" = "f1_ang/f2_fea",
                             "JW07_angry/HO05_fearful" = "f1_fea/f2_ang",
                             "TL12_angry/WK01_fearful" = "m3_ang/m4_fea",
                             "WK01_angry/TL12_fearful" = "m3_fea/m4_ang")



#---------------------------------------------------------------------------------
#Check if everything is loaded correctly

#[1] N different stimuli per participant 
table(E5_Bline$Participant) #should be 112 per participant
table(E5_Adapt$Participant) #should be 224 per participant

#[2]Is every stimulus picked exactly once? - Baseline

#code a stimulus variable
E5_Bline$Stimulus <- str_c(E5_Bline$SpID, E5_Bline$Word, E5_Bline$tML, sep = "_")
table(table(E5_Bline$Stimulus, E5_Bline$Participant)) #should give out only "1"s

#[3]Is every stimulus picked exactly twice? - Adaptation
E5_Adapt$Stimulus <- str_c(E5_Adapt$SpID, E5_Adapt$Word, E5_Adapt$tML, sep = "_")
table(table(E5_Adapt$Stimulus, E5_Adapt$Participant)) #should give out only "2"s

#---------------------------------------------------------------------------------
#Check if some participants only pressed one key during a whole block
# if so -> remove

Check_Bline <- E5_Bline %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one
Check_Adapt <- E5_Adapt %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one

#no one has to be removed

rm(Check_Bline, Check_Adapt)

#---------------------------------------------------------------------------------
#Save datasets
save(E5_Bline, E5_Adapt, file ="input/Exp5_raw_data.RData")

##################################################################################
##################################################################################
##################################################################################

#---------------------------------------------------------------------------------
# survey data preparation

#load raw survey data
S5 <- read.csv(file ="input/data_Exp5.csv")

#rename participant
S5$participant <- substr(S5$participant, 31,38)

#keep only the ones with an experimental file
S5 <- S5 %>% filter(participant %in% unique(E5_Adapt$Participant)) # check: N = 44

S5$VPN_Code <- paste0(S5$LPartCode_1, S5$LPartCode_2, S5$LPartCode_3, S5$LPartCode_4, S5$LPartCode_5, S5$LPartCode_6)

#rename a few variables
S5 <- S5 %>% select(!c(intro_question1_1,
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
save(S5, file="input/Exp5_survey.RData")


##End of Script