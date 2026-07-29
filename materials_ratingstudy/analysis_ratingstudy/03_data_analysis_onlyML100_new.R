#############################################################################################
### file: 03_data_analysis.R
### author: Christine Nussbaum
### contact: christine.nussbaum@uni-jena.de
### date: 06.2021

## reanalyzed July 2026

# R version 4.0.2 (2020-06-22)

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###2: load relevant packages
library(tidyverse)  # version 1.3.0
library(ez)         # version 4.4-0

###2: load relevant functions
source("functions/mySummary.R")
source("functions/tracedEZout.R")


###4: load data: ClassTask.RData
load(file = "input/ratingdata_prep.RData")




##############################################################################
########     Statistical analysis of 100% Morphs only          #############
##############################################################################


### Filter and aggregate new dataset 
D<- D %>% filter(Int =="100")

#include only speakers and emotions of interest
D <- D %>% filter(Emo %in% c("ang", "fea"))
D <- D %>% filter(SpID%in% c("nf01", "nf03", "nm03", "nm04"))

D$Int_RESP <- as.numeric(as.character(D$Int_RESP))

#----------------------------------------------------------
#separate for each emotion, to see the confusions
#----------------------------------------------------------

Ang <- D %>% filter(Emo %in% c("ang"))
table(Ang$Emo_RESP) # never confused with fear

Fea <- D %>% filter(Emo %in% c("fea"))
table(Fea$Emo_RESP) # never confused with anger

#----------------------------------------------------------
#now look at averaged recognition
#----------------------------------------------------------

Emo <- D %>% group_by(Subject, Emo, SpID) %>% summarise(Emo_ACC=mean(Emo_ACC))


ACC <- Emo %>% group_by(Emo, SpID) %>% summarise(Emo_ACC=mean(Emo_ACC))

#now convert them  Emo into wide format
Emo <- Emo %>% pivot_wider(names_from = c("Emo", "SpID"), values_from = "Emo_ACC")


#now run all the ttests

female_fea <- t.test(Emo$fea_nf01, Emo$fea_nf03, paired = TRUE) 
female_ang<- t.test(Emo$ang_nf01, Emo$ang_nf03, paired = TRUE)


male_fea <- t.test(Emo$fea_nm03, Emo$fea_nm04, paired = TRUE)
male_ang<- t.test(Emo$ang_nm03, Emo$ang_nm04, paired = TRUE)


#----------------------------------------------------------
#now look at intensity
#----------------------------------------------------------


Int <- D %>% group_by(Subject, Emo, SpID) %>% summarise(Int_RESP=mean(Int_RESP))


ACC <- Int %>% group_by(Emo, SpID) %>% summarise(Int_RESP=mean(Int_RESP))

#now convert them  Int into wide format
Int <- Int %>% pivot_wider(names_from = c("Emo", "SpID"), values_from = "Int_RESP")


#now run all the ttests

female_fea <- t.test(Int$fea_nf01, Int$fea_nf03, paired = TRUE) 
female_ang<- t.test(Int$ang_nf01, Int$ang_nf03, paired = TRUE)


male_fea <- t.test(Int$fea_nm03, Int$fea_nm04, paired = TRUE)
male_ang<- t.test(Int$ang_nm03, Int$ang_nm04, paired = TRUE)


### End of Script