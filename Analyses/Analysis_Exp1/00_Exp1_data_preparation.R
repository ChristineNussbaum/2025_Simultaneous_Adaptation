##########################################################################
## File: 00_Exp1_data_preparation.R
## Data Preparatation for Exp 1: Adaptation of Emotion - male/female voices 
# author: Christine Nussbaum/Dorothea Berges
# date 05/2024

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
D <- D[,c(18,17,16, 1:15)]

#---------------------------------------------------------------------------------
#Data preparation


#[1] Split the data in the Baseline and the Adaptation-Block

E1_Bline <- D[,-c(12:18)] %>% filter(V1 == "Baseline")

E1_Adapt <- D %>% filter(V1 == "Antwort")


#[2] Rename Variables
names(E1_Bline) <- c("Participant", "Date", "Experiment","Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp","RT" )
names(E1_Adapt) <- c("Participant", "Date", "Experiment", "Block", "Duration", "SpID", "Word", "tML", "SpSex","TopUp", "y", "z", "TopupM", "TopupF", "Resp", "RT", "AdaptType", "Blockorder" )

#[3] Adjust some settings of variables
E1_Bline$Resp <- as.numeric(E1_Bline$Resp)
E1_Adapt$Resp <- as.numeric(E1_Adapt$Resp)

#sort out all the counting variables responsible for the top-up E1_Adaptation: 
E1_Adapt$y <- ifelse(E1_Adapt$TopUp != 0, NA, E1_Adapt$y)
E1_Adapt$z <- ifelse(E1_Adapt$TopUp != 0, NA, E1_Adapt$z)
E1_Adapt$TopupM <- ifelse(E1_Adapt$TopUp != 0, NA, E1_Adapt$TopupM)
E1_Adapt$TopupF <- ifelse(E1_Adapt$TopUp != 0, NA, E1_Adapt$TopupF)

E1_Adapt$TopUp <- ifelse(E1_Adapt$TopUp == 0, "Yes", NA) #Code if this was a TopUp Trial


#recode the Response for both datasets
E1_Bline$Resp <- E1_Bline$Resp -1 # umkodieren
E1_Adapt$Resp <- E1_Adapt$Resp -1 # umkodieren

#---------------------------------------------------------------------------------
#Check if everything is loaded correctly

#[1] N different stimuli per participant 
table(E1_Bline$Participant) #should be 112 per participant
table(E1_Adapt$Subject) #should be 224 per participant

#[2]Is every stimulus picked exactly once? - Baseline

#code a stimulus variable
E1_Bline$Stimulus <- str_c(E1_Bline$SpID, E1_Bline$Word, E1_Bline$tML, sep = "_")
table(table(E1_Bline$Stimulus, E1_Bline$Participant)) #should give out only "1"s

#[3]Is every stimulus picked exactly twice? - Adaptation
E1_Adapt$Stimulus <- str_c(E1_Adapt$SpID, E1_Adapt$Word, E1_Adapt$tML, sep = "_")
table(table(E1_Adapt$Stimulus, E1_Adapt$Participant)) #should give out only "2"s

#---------------------------------------------------------------------------------
#Check if some participants only pressed one key during a whole block
# if so -> remove

Check_Bline <- E1_Bline %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one
Check_Adapt <- E1_Adapt %>% group_by(Participant) %>% summarise(var = var(Resp)) # no one

# no one has to be removed here

rm(Check_Bline, Check_Adapt)

#---------------------------------------------------------------------------------
#Save datasets
save(E1_Bline, E1_Adapt, file ="input/Exp1_raw_data.RData")

#CG_input <- mySummary(Adapt, Resp, Subject, AdaptType, SpSex, tML) # Input f?r jeden Probanden einzeln
#CG_agg_input <- mySummary(CG_input, Resp, AdaptType, SpSex, tML) # input ?ber alle Probanden gemittelt
#save(CG_input, CG_agg_input, CG_input_E1_Bline, file ="input/CG_input_data.RData")
#CG_input_E1_Bline <- mySummary(E1_Bline, Resp, Subject, SpSex, tML)

rm()
##################################################################################
##################################################################################
##################################################################################

#---------------------------------------------------------------------------------
# survey data preparation

#load raw survey data
S1 <- read.csv(file ="input/data_Exp1.csv")

#rename participant
S1$participant <- substr(S1$participant, 31,38)

#keep only the ones with an experimental file
S1 <- S1 %>% filter(participant %in% unique(E1_Adapt$Participant)) # check: N = 40

S1$VPN_Code <- paste0(S1$LPartCode_1, S1$LPartCode_2, S1$LPartCode_3, S1$LPartCode_4, S1$LPartCode_5, S1$LPartCode_6)

#rename a few variables
S1 <- S1 %>% select(!c(intro_question1_1,
                     TIME_start, 
                     TIME_end, 
                     CLIENT_start,
                     LPartCode_1,
                     LPartCode_2,
                     LPartCode_3,
                     LPartCode_4,
                     LPartCode_5,
                     LPartCode_6,
                     StartRatingExperiment_1,
                     country)) %>% 
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
save(S1, file="input/Exp1_survey.RData")

names(S)
##End of S1cript