##########################################################################
## File: 01_data_preparation.R
## Data Preparatation for Emotion-Adaptation-Experiment in Voices, BA Berges
# author: Christine Nussbaum/Dorothea Berges
# date 02/2022, revised 05/2024

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


#reorder columns - Subject Variablen zuerst
D <- D[,c(18,17,16, 1:15)]



#----------------------------------------- Baseline Block -------------------------------

#jetzt trennen wir den Datensatz nach Adapt und Baseline: 
Baseline <- D[,-c(12:18)] %>% filter(V1 == "Baseline")
#und richtig benennen: 
names(Baseline) <- c("Subject", "Date", "Experiment","Block", "Duration", "SpID", "Word", "tML", "SpSex", "Resp","RT" )
view(Baseline)

Baseline$Resp <- as.numeric(Baseline$Resp)

# jetzt m?ssen wir pr?fen, ob alle Stimuli richtig geladen und da sind

#(1) Wieviele Stimuli werden pro Proband gezogen: 
table(Baseline$Subject) # 112 sollte passen, oder? #ja, passt

#(2) Wird jeder Stimulus bei jedem Probanden genau einmal gezogen? 

# dazu m?ssen wir erstmal eine Variable erstellen, die uns den Stimulus kodiert: 
Baseline$Stimulus <- str_c(Baseline$SpID, Baseline$Word, Baseline$tML, sep = "_")

table(Baseline$Stimulus, Baseline$Subject) # nicht die eleganteste L?sung, aber zeigt dass jeder Stimulus bei jedem probanden genau einmal gezogen wird. Passt

CG_input_Baseline <- mySummary(Baseline, Resp, Subject, SpSex, tML)

#----------------------------------------- Adapt Block -------------------------------

Adapt <- D %>% filter(V1 == "Antwort")


# name variable 
names(Adapt) <- c("Subject", "Date", "Experiment", "Block", "Duration", "SpID", "Word", "tML", "SpSex","TopUp", "y", "z", "TopupM", "TopupF", "Resp", "RT", "AdaptType", "Blockorder" )
view(Adapt)

# soo.. na da schauen wir mal... wir gucken erstmal wieder, ob alles da ist: 
Adapt$Resp <- as.numeric(Adapt$Resp)

#(1) Wieviele Stimuli werden pro Proband gezogen: 
table(Adapt$Subject) # zweimal 112 

#(2) Wird jeder Stimulus bei jedem Probanden genau einmal pro Adaptationsbedingung gezogen? 

# dazu m?ssen wir erstmal eine Variable erstellen, die uns den Stimulus kodiert: 
Adapt$Stimulus <- str_c(Adapt$SpID, Adapt$Word, Adapt$tML, sep = "_")

table(Adapt$Stimulus, Adapt$Subject) # passt




# jetzt schauen wir uns dieses ganze Variablen-Chaos mal noch bisschen an: 

# erstmal aufr?umen: 
Adapt$y <- ifelse(Adapt$TopUp != 0, NA, Adapt$y)
Adapt$z <- ifelse(Adapt$TopUp != 0, NA, Adapt$z)
Adapt$TopupM <- ifelse(Adapt$TopUp != 0, NA, Adapt$TopupM)
Adapt$TopupF <- ifelse(Adapt$TopUp != 0, NA, Adapt$TopupF)


Adapt$TopUp <- ifelse(Adapt$TopUp == 0, "Yes", NA) # so, jetzt kann man hier auch was erkennen
 view(Adapt)

# jetzt speichern wir die beiden Datens?tze erstmal ab: 
save(Baseline, Adapt, file ="input/raw_data.RData")

# aggregieren f?r die CG-Analyse
Adapt$Resp <- Adapt$Resp -1 # umkodieren


CG_input <- mySummary(Adapt, Resp, Subject, AdaptType, SpSex, tML) # Input f?r jeden Probanden einzeln
CG_agg_input <- mySummary(CG_input, Resp, AdaptType, SpSex, tML) # input ?ber alle Probanden gemittelt

save(CG_input, CG_agg_input, CG_input_Baseline, file ="input/CG_input_data.RData")
