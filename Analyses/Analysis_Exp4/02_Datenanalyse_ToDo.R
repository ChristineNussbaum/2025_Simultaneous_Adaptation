##########################################################################
## File: 01_Datenaufbereitung_experiment.R
## Einlesen und Aufbereiten der Experimentaldaten 
# author: Christine Nussbaum
# date 02/2025 

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
D <- loadPTKExperimentData(relDirPath = "input/raw/")

#wieviele sind gelasen: 
print(paste("Es sind aktuell", length(unique(D$Subject)), "Probandendatensätze geladen"))

#---------------------------------------------------------------------------------
# Datenaufbereitung

# richtige Variablen benennen: 

names(D) <- c("File1", "tDur1", "File2", "tDur2", "Ident", "Word", "SpSex", 
              "Dataset", "X", "Key", "RT", "ACC", "Blockorder", "Experiment", 
              "Date", "Participant")

#code same of different

D$Cond <- ifelse(D$Ident %in% c("nf01-01", "nf03-03", "nm03-03", "nm04-04", 
                                   "TL12-TL12", "WK01-WK01", "JW07-JW07", "HO05-HO05"),
                   "same", "diff")

unique(D$Ident)

table(D$Ident, D$Cond)

#Accuraccy rekodieren: (1= richtig, 2 = falsch, 3= keine Antwort)

D$ACC <- ifelse(D$ACC == 1, 1, 0)


##aggregate across:
D1 <- D %>% group_by(Participant, Dataset, SpSex, Cond) %>% summarise(ACC = mean(ACC),
                                                              N= length(RT))

D_agg <- mySummary(D1,ACC, Dataset,SpSex,  Cond )


#rough test 
test <- D %>% group_by(Participant, Dataset) %>% summarise(ACC = mean(ACC))
t.test(data= test, ACC~Dataset, paired= TRUE)
test <- mySummary(test,ACC, Dataset)


#------------------------------------------
# calculate Signal detection Parameters: 

#[1]
# Remember that Hits and Misses are only defined for Same Trials
# Correct Rejections and False Alarms are only defined for Different Trials
# This is why we put NAs (=not defined) in the other cells respectively

# Hits
D$H <- with(D, ifelse(Cond=="same" & ACC ==1, 1,0))
D$H <- with(D, ifelse(Cond=="diff",  NA, D$H)) 

# Misses

D$M <- with(D, ifelse(Cond=="same" & ACC ==0,1,0))
D$M <- with(D, ifelse(Cond=="diff",  NA, D$M))

# Correct Rejections

D$CR <- with(D, ifelse(Cond=="diff" & ACC ==1  ,1,0))
D$CR <- with(D, ifelse(Cond=="same",  NA, D$CR))

# False Alarms

D$FA <- with(D, ifelse(Cond=="diff"& ACC ==0  ,1,0))
D$FA <- with(D, ifelse(Cond=="same",  NA, D$FA))


# checking
table(D$H,D$M)
table(D$CR,D$FA)


#[2] aggregrate for Signal detection parameters
##aggregate across:
B_same <- D[D$Cond == "same",] %>% group_by(Participant, Dataset, SpSex) %>% summarise(H = mean(H, na.rm= TRUE),
                                                                     M = mean(M, na.rm= TRUE),
                                                                     N= length(RT))

B_diff <- D[D$Cond == "diff",] %>% group_by(Participant, Dataset, SpSex) %>% summarise(CR = mean(CR, na.rm= TRUE),
                                                                          FA = mean(FA, na.rm= TRUE),
                                                                          N= length(RT))

B <- merge(B_same, B_diff)
rm(B_same, B_diff)

#[3]
####  calculate the z parameters

#correction for values of 0 and 1
B$H <- ifelse(B$H == 1, (90-0.5)/90, B$H)
B$H <- ifelse(B$H == 0, 0.5/90, B$H)

B$FA <- ifelse(B$FA == 0, 0.5/90, B$FA)
B$FA <- ifelse(B$FA == 1, (90-0.5)/90, B$FA)

# calculation of z-values
# help: dnorm, qnorm, pnorm, rnorm

B$Hz <- qnorm(B$H)
B$FAz <- qnorm(B$FA)


#[4]
#calculate d-prime and criterion

#d-prime
B$dprime <- B$Hz - B$FAz

#criterion
B$crit <- (-0.5*(B$Hz + B$FAz))


B_agg <- B %>% group_by(Dataset, SpSex) %>% summarise(dprime = mean(dprime),
                                                      crit = mean(crit), 
                                                      N = length(Dataset))

#speichern aller Datensätze: 
save(D, file="input/D_raw.RData")
save(B, file="input/B_signaldetection.RData")

## End of Script