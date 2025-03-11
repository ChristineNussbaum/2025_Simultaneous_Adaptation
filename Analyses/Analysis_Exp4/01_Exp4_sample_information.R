##########################################################################
## File: 01_Exp4_sample_information.R
## This script gives me an overview of the sample in Exp4
# author: Christine Nussbaum
# date 03/2025


# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load required packages
library("tidyverse")

# load relevant functions
source("functions/mySummary.R") 

#---------------------------------------------------------------------------------
#get the preprocessed data: 

#S data
load(file="input/Exp4_Survey.RData")


## Meaning of Variables

#ToDo


#------------------------------------------------------------------------------------------------------------------#
#                                           Sample Demographics                                                    #
#------------------------------------------------------------------------------------------------------------------#

#general demographics
Age <- mySummary(S4, LAge)
range <- S4 %>% summarise(range(LAge))
sex <- table(S4$LSex)
language <- table(S4$LMotherLanguage)
LStudyWork <- table(S4$LStudyWork)
LHearingKown <- table(S4$LHearingKown)
LHearingKind <- table(S4$LHearingKind)
LHearingImp <- table(S4$LHearingImp)
duration <- S4 %>% summarise(Tmean = mean(TIME_total),
                            min = min(TIME_total),
                            max = max(TIME_total))
capture.output(as.matrix(Age), as.matrix(range), sex, language, 
               LStudyWork, LHearingKown, LHearingKind, LHearingImp, as.matrix(duration), 
               file="output/Exp4_demographics.txt")
rm(Age, range, sex, language, LStudyWork, LHearingKown, LHearingKind, LHearingImp, duration)


## comments
write.csv(data.frame(S4$participant, S4$VPN_Code, S4$Anmerkungen, S4$InstructionsClear, S4$Strategy), file="output/Exp4_comments.txt")



## End of Script