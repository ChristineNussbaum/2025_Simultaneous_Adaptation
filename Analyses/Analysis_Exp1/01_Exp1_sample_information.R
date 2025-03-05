##########################################################################
## File: 01_Exp1_sample_information.R
## This script gives me an overview of the sample in Exp1
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
load(file="input/Exp1_Survey.RData")


## Meaning of Variables

#ToDo


#------------------------------------------------------------------------------------------------------------------#
#                                           Sample Demographics                                                    #
#------------------------------------------------------------------------------------------------------------------#

#general demographics
Age <- mySummary(S1, LAge)
range <- S1 %>% summarise(range(LAge))
sex <- table(S1$LSex)
language <- table(S1$LMotherLanguage)
LStudyWork <- table(S1$LStudyWork)
LHearingKown <- table(S1$LHearingKown)
LHearingKind <- table(S1$LHearingKind)
LHearingImp <- table(S1$LHearingImp)
duration <- S1 %>% summarise(Tmean = mean(TIME_total),
                            min = min(TIME_total),
                            max = max(TIME_total))
capture.output(as.matrix(Age), as.matrix(range), sex, language, 
               LStudyWork, LHearingKown, LHearingKind, LHearingImp, as.matrix(duration), 
               file="output/Exp1_demographics.txt")
rm(Age, range, sex, language, LStudyWork, LHearingKown, LHearingKind, LHearingImp, duration)


## comments
capture.output(S1$Anmerkungen, S1$InstructionsClear, S1$Strategy, file="output/Exp1_comments.txt")

# after Exp-Evaluation
Eval1 <- table(S1$Eval1)
Eval2 <- table(S1$Eval2)
Eval3 <- table(S1$Eval3)
Eval4 <- table(S1$Eval4)
Eval5 <- table(S1$Eval5)
Eval6 <- table(S1$Eval6)
Eval7 <- table(S1$Eval7)
Eval8 <- table(S1$Eval8)

label <- c("Im Alltag achte ich stets auf den Klang der Stimme einer Person.",
 "Es fiel mir äußerst schwer, die Stimmen zu bewerten.",
  "Bei den meisten Stimmen hatte ich gar keine Ahnung, was ich dr?cken sollte.",
  "Ich habe die ganze Zeit aufmerksam zugehört.",
  "Oft habe ich einfach irgendetwas geklickt.",
  "Ich fand dieses Hörexperiment äußerst interessant.",
  "Ich habe in jedem Durchgang versucht, die Bewertung so gut wie möglich zu machen.",
  "Ich achte normalerweise gar nicht auf die Stimmen von Personen.")
  

capture.output(label[1], Eval1, label[2],Eval2, label[3],Eval3,label[4], Eval4, label[5],Eval5, label[6], Eval6,label[7], Eval7, label[8], Eval8,file="output/Exp1_after_experiment_evaluation.txt")
rm(label, Eval1, Eval2, Eval3, Eval4, Eval5, Eval6, Eval7, Eval8)


## End of Script