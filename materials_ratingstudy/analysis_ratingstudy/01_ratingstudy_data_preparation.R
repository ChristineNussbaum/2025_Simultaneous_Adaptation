#############################################################################################
### file: 01_ratingstudy_data_preparation.R
### author: Christine Nussbaum
### contact: christine.Nussbaum@uni-jena.de
### date: 06.2021

# clear directory
rm(list=ls())

# set working directory

setwd("~/Arbeit/Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # dienstlicher PC
setwd("~/Christine/Arbeit/01_Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # privater PC

# load required packages

library("tidyverse")

# load data

# Data with 20 Vpn
D <- read.delim("input/full_morphs_merge20vpn.txt", header = TRUE, sep="\t")


#Check: 18280 Obs * 68 Variables


##################################################################################################################
# reduce dataset to relevant variables and Trials

D <- D %>% filter(ExpPart == "Main")
D <- D %>% select(Subject, 
                  Age, 
                  Bildungsabschluss, 
                  SubjectCode, 
                  Handedness, 
                  Hoerschaedigung, 
                  Studiengang, 
                  Sex.Session.,
                  stimulus, 
                  correct2, 
                  emotion, 
                  morphlevel, 
                  Rating1.ACC, 
                  Rating1.RESP, 
                  Rating1.RT, 
                  Rating2.ACC, 
                  Rating2.RESP, 
                  Rating2.RT,
                  Sex.Block.,
                  speaker,
                  word)

# 896 Trials *20 Participants

# Rename Variables 

names(D) <- c("Subject", "Age", "Bildung", "SubjectCode", "Handedness", "Hoerschaedigung", "Studiengang", "LSex", "stimulus","Int", 
              "Emo", "tML", "Emo_ACC", "Emo_RESP", "Emo_RT", "Int_ACC", "Int_RESP", "Int_RT","SpSex","SpID","word")

#prepare variables
D$Int <- str_replace(D$Int, "[{]", "")
D$Int <- str_replace(D$Int, "[}]", "")
D$Int_RESP <- str_replace(D$Int_RESP, "[{]", "")
D$Int_RESP <- str_replace(D$Int_RESP, "[}]", "")
D$Emo_RESP <- str_replace(D$Emo_RESP, "[{]", "")
D$Emo_RESP <- str_replace(D$Emo_RESP, "[}]", "")


D$Emo <- factor(D$Emo,
                  levels = c("hap",  "ple", "fea","sad", "dis", "sur", "ang"))

D$Emo_RESP <- factor(D$Emo_RESP,
                        levels = c("hap",  "ple", "fea","sad", "dis", "sur", "ang"))

D$Int <- factor(as.character(D$Int),
                     levels = c("100","75","50","25"))

D$Int_RESP <- factor(as.character(D$Int_RESP),
                levels = c("100","75","50","25"))

# Save raw file and preprocessed data with missings

save(D, file = "input/ratingdata_prep.RData")


