##########################################################################
## File: 02_Exp1_sample_information.R
## This script gives me an overview of the sample in Exp2
# author: Christine Nussbaum
# date 03/2025, updated 11/2025


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
load(file="input/Exp2_Survey.RData")

#number of participants
print(paste("There are currently", length(unique(E2_Adapt$Participant)), "participants loaded."))

## Meaning of Variables

#ToDo


#------------------------------------------------------------------------------------------------------------------#
#                                           Sample Demographics                                                    #
#------------------------------------------------------------------------------------------------------------------#

#general demographics
Age <- mySummary(S2, LAge)
range <- S2 %>% summarise(range(LAge))
sex <- table(S2$LSex)
language <- table(S2$LMotherLanguage)
LStudyWork <- table(S2$LStudyWork)
LHearingKown <- table(S2$LHearingKown)
LHearingKind <- table(S2$LHearingKind)
LHearingImp <- table(S2$LHearingImp)
duration <- S2 %>% summarise(Tmean = mean(TIME_total, na.rm= TRUE),
                            min = min(TIME_total, na.rm= TRUE),
                            max = max(TIME_total, na.rm= TRUE))
capture.output(as.matrix(Age), as.matrix(range), sex, language, 
               LStudyWork, LHearingKown, LHearingKind, LHearingImp, as.matrix(duration), 
               file="output/Exp2_demographics.txt")
rm(Age, range, sex, language, LStudyWork, LHearingKown, LHearingKind, LHearingImp, duration)


## comments
write.csv(data.frame(S2$participant, S2$VPN_Code, S2$Anmerkungen, S2$InstructionsClear, S2$Strategy), file="output/Exp2_comments.txt")


# after Exp-Evaluation
Eval1 <- table(S2$Eval1)
Eval2 <- table(S2$Eval2)
Eval3 <- table(S2$Eval3)
Eval4 <- table(S2$Eval4)
Eval5 <- table(S2$Eval5)
Eval6 <- table(S2$Eval6)
Eval7 <- table(S2$Eval7)
Eval8 <- table(S2$Eval8)

label <- c("Im Alltag achte ich stets auf den Klang der Stimme einer Person.",
 "Es fiel mir äußerst schwer, die Stimmen zu bewerten.",
  "Bei den meisten Stimmen hatte ich gar keine Ahnung, was ich dr?cken sollte.",
  "Ich habe die ganze Zeit aufmerksam zugehört.",
  "Oft habe ich einfach irgendetwas geklickt.",
  "Ich fand dieses Hörexperiment äußerst interessant.",
  "Ich habe in jedem Durchgang versucht, die Bewertung so gut wie möglich zu machen.",
  "Ich achte normalerweise gar nicht auf die Stimmen von Personen.")
  

capture.output(label[1], Eval1, label[2],Eval2, label[3],Eval3,label[4], Eval4, label[5],Eval5, label[6], Eval6,label[7], Eval7, label[8], Eval8,file="output/Exp2_after_experiment_evaluation.txt")
rm(label, Eval1, Eval2, Eval3, Eval4, Eval5, Eval6, Eval7, Eval8)

#------------------------------------------------------------------------------------------------------------------#
#                                        Analysis of Trials of omission                                            #
#------------------------------------------------------------------------------------------------------------------#

#load the preprocessed raw data
load(file ="input/Exp2_raw_data.RData")

#check for the number of missing (indicated by RT == 3000)
missings_Adapt <-E2_Adapt %>%  group_by(Participant) %>% filter(RT == 3000) %>% summarise(N = length(Participant),
                                                                                          prop = N/224) 

missings_Bline <- E2_Bline %>%  group_by(Participant) %>% filter(RT == 3000) %>% summarise(N = length(Participant),
                                                                                           prop = N/112) 
capture.output(as.matrix(missings_Adapt), as.matrix(missings_Bline), file = "output/Exp2_omissions_summary.txt")

#remove omissions from the data
E2_Adapt <- E2_Adapt %>% filter(RT != 3000) # 20 removed
E2_Bline <- E2_Bline %>% filter(RT != 3000) # 20 removed

rm(missings_Bline, missings_Adapt)

#save datasets again
save(E2_Bline, E2_Adapt, file ="input/Exp2_without_omissions.RData")


## End of Script