##########################################################################
## File: 00_Exp4_data_preparation.R
## Data Preparatation for Exp 4: manipulation check for identity matching 
# author: Christine Nussbaum
# date 03/2024

# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"


# clear directory
rm(list=ls())


# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# load required packages
library(tidyverse)

# load relevant functions
source("functions/loadPTKExperimentData.R")
source("functions/mySummary.R") 


#---------------------------------------------------------------------------------
#laden der Rohdaten (dazu haben wir eine eigene Funktion geschrieben, die das macht)
E4 <- loadPTKExperimentData(relDirPath = "input/raw/")

#wieviele sind gelasen: 
print(paste("There are currently", length(unique(E4$Participant)), "participants loaded."))

#---------------------------------------------------------------------------------
# Data preparation

#[1] Rename Variables

names(E4) <- c("File1", "tDur1", "File2", "tDur2", "Ident", "Word", "SpSex", 
              "Dataset", "X", "Key", "RT", "ACC", "Blockorder", "Experiment", 
              "Date", "Participant")

#[2] code same or different

E4$Cond <- ifelse(E4$Ident %in% c("nf01-01", "nf03-03", "nm03-03", "nm04-04", 
                                   "TL12-TL12", "WK01-WK01", "JW07-JW07", "HO05-HO05"),
                   "same", "diff")

unique(E4$Ident)

table(E4$Ident, E4$Cond)

#recode the Response for both datasets

E4$ACC <- ifelse(E4$ACC == 1, 1, 0)

#---------------------------------------------------------------------------------
#Check if some participants only pressed one key during a whole block
# if so -> remove

Check_E4 <- E4 %>% group_by(Participant) %>% summarise(var = var(Key)) # no one

# no one has to be removed here
rm(Check_E4)

#but this was my test dataset: 
E4 <- E4 %>% filter(Participant != "e436e90b")



#------------------------------------------
# calculate Signal detection Parameters: 

#[1]
# Remember that Hits and Misses are only defined for Same Trials
# Correct Rejections and False Alarms are only defined for Different Trials
# This is why we put NAs (=not defined) in the other cells respectively

# Hits
E4$H <- with(E4, ifelse(Cond=="same" & ACC ==1, 1,0))
E4$H <- with(E4, ifelse(Cond=="diff",  NA, E4$H)) 

# Misses

E4$M <- with(E4, ifelse(Cond=="same" & ACC ==0,1,0))
E4$M <- with(E4, ifelse(Cond=="diff",  NA, E4$M))

# Correct Rejections

E4$CR <- with(E4, ifelse(Cond=="diff" & ACC ==1  ,1,0))
E4$CR <- with(E4, ifelse(Cond=="same",  NA, E4$CR))

# False Alarms

E4$FA <- with(E4, ifelse(Cond=="diff"& ACC ==0  ,1,0))
E4$FA <- with(E4, ifelse(Cond=="same",  NA, E4$FA))


# checking
table(E4$H,E4$M)
table(E4$CR,E4$FA)


#[2] aggregrate for Signal detection parameters
##aggregate across:
E4_same <- E4[E4$Cond == "same",] %>% group_by(Participant, Dataset, SpSex) %>% summarise(H = mean(H, na.rm= TRUE),
                                                                     M = mean(M, na.rm= TRUE),
                                                                     N= length(RT))

E4_diff <- E4[E4$Cond == "diff",] %>% group_by(Participant, Dataset, SpSex) %>% summarise(CR = mean(CR, na.rm= TRUE),
                                                                          FA = mean(FA, na.rm= TRUE),
                                                                          N= length(RT))

E4_signal <- merge(E4_same, E4_diff)
rm(E4_same, E4_diff)

#[3]
####  calculate the z parameters

#correction for values of 0 and 1
E4_signal$H <- ifelse(E4_signal$H == 1, (90-0.5)/90, E4_signal$H)
E4_signal$H <- ifelse(E4_signal$H == 0, 0.5/90, E4_signal$H)

E4_signal$FA <- ifelse(E4_signal$FA == 0, 0.5/90, E4_signal$FA)
E4_signal$FA <- ifelse(E4_signal$FA == 1, (90-0.5)/90, E4_signal$FA)

# calculation of z-values
# help: dnorm, qnorm, pnorm, rnorm

E4_signal$Hz <- qnorm(E4_signal$H)
E4_signal$FAz <- qnorm(E4_signal$FA)


#[4]
#calculate d-prime and criterion

#d-prime
E4_signal$dprime <- E4_signal$Hz - E4_signal$FAz

#criterion
E4_signal$crit <- (-0.5*(E4_signal$Hz + E4_signal$FAz))



#speichern aller Datensätze: 
save(E4, file="input/Exp4_raw_data.RData")
save(E4_signal, file="input/E4_signal_detection.RData")

rm(E4_signal)
##################################################################################
##################################################################################
##################################################################################

#---------------------------------------------------------------------------------
# survey data preparation

#load raw survey data
S4 <- read.csv(file ="input/data_Exp4.csv")

#rename participant
S4$participant <- substr(S4$participant, 31,38)

#keep only the ones with an experimental file
S4 <- S4 %>% filter(participant %in% unique(E4$Participant)) # check: N = 36

S4$VPN_Code <- paste0(S4$LPartCode_1, S4$LPartCode_2, S4$LPartCode_3, S4$LPartCode_4, S4$LPartCode_5, S4$LPartCode_6)

#rename a few variables
S4 <- S4 %>% select(!c(intro_question1_1,
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
         Strategy = afterExp4Strategy_1)


#save survey
save(S4, file="input/Exp4_survey.RData")

## End of Script