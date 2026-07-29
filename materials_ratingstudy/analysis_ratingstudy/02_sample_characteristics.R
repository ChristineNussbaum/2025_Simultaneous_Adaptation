#############################################################################################
### file: 02_sample_characteristics.R
### author: Christine Nussbaum
### contact: christine.nussbaum@uni-jena.de
### date: 06.2021

# R version 4.0.2 (2020-06-22)

# clear environment
rm(list = ls())


# set working directory

setwd("~/Arbeit/Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # dienstlicher PC
setwd("~/Christine/Arbeit/01_Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # privater PC

# load required packages

library("tidyverse")

# load relevant functions

source("functions/mySummary.R")

# load data

load(file = "input/ratingdata_prep.RData")


# aggregate dataset

sample <- mySummary(D, Age, Subject, Bildung, Handedness, Hoerschaedigung, LSex)

############################# Extract basic sample information ###############################

  
SampleInfo<- list(Sex= table(sample$LSex),
                  Age= c(summary(sample$Age), SD= sd(sample$Age)),
                  Handeness = (table(sample$Handedness)),
                  Bildung = (table(sample$Bildung)), 
                  Hoerschaedigung = (table(sample$Hoerschaedigung)))

# save output: 
capture.output(SampleInfo, file="output/SampleInfo.txt")

##################################################################################################
#expected output:
# $Sex
# female   male 
# 13      7 
# 
# $Age
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.      SD 
# 19.0000 19.0000 20.0000 21.2500 21.5000 30.0000  3.2424 
# 
# $Handeness
# left right 
# 3    17 
# 
# $Bildung
# 
# Abitur Bachelor 
# 19        1 
# 
# $Hoerschaedigung
# Nein 
# 20 
##################################################################################################



# End of Script