##########################################################################
## File: 00_Exp2_data_preparation.R
## Data Preparatation for Exp 2: Adaptation of Emotion - speaker identities
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

E2_Bline <- D[,-c(12:17)] %>% filter(V1 == "Baseline")

E2_Adapt <- D %>% filter(V1 == "Antwort")


#[2] Rename Variables
names(E2_Bline) <- c("Participant", "Date", "Experiment","Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp","RT" )
names(E2_Adapt) <- c("Participant", "Date", "Experiment", "Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp", "RT", "AdaptType", "Blockorder", "TopUp", "z", "TopupM", "TopupF")

#[3] Adjust some settings of variables
E2_Bline$Resp <- as.numeric(E2_Bline$Resp)
E2_Adapt$Resp <- as.numeric(E2_Adapt$Resp)

#sort out all the counting variables responsible for the top-up E2_Adaptation: 
E2_Adapt$z <- ifelse(E2_Adapt$TopUp != 0, NA, E2_Adapt$z)
E2_Adapt$TopupM <- ifelse(E2_Adapt$TopUp != 0, NA, E2_Adapt$TopupM)
E2_Adapt$TopupF <- ifelse(E2_Adapt$TopUp != 0, NA, E2_Adapt$TopupF)

E2_Adapt$TopUp <- ifelse(E2_Adapt$TopUp == 0, "Yes", NA) #Code if this was a TopUp Trial


#recode the Response for both datasets
E2_Bline$Resp <- E2_Bline$Resp -1 # umkodieren
E2_Adapt$Resp <- E2_Adapt$Resp -1 # umkodieren


#recode AdaptType

E2_Adapt$AdaptType <- recode(E2_Adapt$AdaptType, 
                             "Identität1_angry/Identität3_fearful" = "f1_ang/f2_fea",
                             "Identität3_angry/Identität1_fearful" = "f1_fea/f2_ang",
                             "Identität3_angry/Identität4_fearful" = "m3_ang/m4_fea",
                             "Identität4_angry/Identität3_fearful" = "m3_fea/m4_ang")



#---------------------------------------------------------------------------------
#Check if everything is loaded correctly

#[1] N different stimuli per participant 
table(E2_Bline$Participant) #should be 112 per participant
table(E2_Adapt$Participant) #should be 224 per participant

#[2]Is every stimulus picked exactly once? - Baseline

#code a stimulus variable
E2_Bline$Stimulus <- str_c(E2_Bline$SpID, E2_Bline$Word, E2_Bline$tML, sep = "_")
table(table(E2_Bline$Stimulus, E2_Bline$Participant)) #should give out only "1"s

#[3]Is every stimulus picked exactly twice? - Adaptation
E2_Adapt$Stimulus <- str_c(E2_Adapt$SpID, E2_Adapt$Word, E2_Adapt$tML, sep = "_")
table(table(E2_Adapt$Stimulus, E2_Adapt$Participant)) #should give out only "2"s

#---------------------------------------------------------------------------------
#Check if some participants only pressed one key during a whole block
# if so -> remove

Check_Bline <- E2_Bline %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one
Check_Adapt <- E2_Adapt %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one

#"dd1685a2f" pressed only one key, so will be removed
E2_Adapt <- E2_Adapt %>% filter(Participant != "d1685a2f")
E2_Bline <- E2_Bline %>% filter(Participant != "d1685a2f")
#44 participants left

rm(Check_Bline, Check_Adapt)

#---------------------------------------------------------------------------------
#Save datasets
save(E2_Bline, E2_Adapt, file ="input/Exp2_raw_data.RData")


rm(D)
##################################################################################
##################################################################################
##################################################################################

#---------------------------------------------------------------------------------
# survey data preparation

#load raw survey data
S2 <- read.csv(file ="input/data_Exp2.csv")

#rename participant
S2$participant <- substr(S2$participant, 31,38)

#keep only the ones with an experimental file
S2 <- S2 %>% filter(participant %in% unique(E2_Adapt$Participant)) # check: N = 44

S2$VPN_Code <- paste0(S2$LPartCode_1, S2$LPartCode_2, S2$LPartCode_3, S2$LPartCode_4, S2$LPartCode_5, S2$LPartCode_6)

#rename a few variables
S2 <- S2 %>% select(!c(intro_question1_1,
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
save(S2, file="input/Exp2_survey.RData")


##End of Script