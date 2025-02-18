##########################################################################
## File: 00_EmoIdent_Expcheck
## Das Script checkt ob die ersten Testdatensätze des identitäts-Experiments in Ordnung sind. 
# author: Christine Nussbaum
# date 01/2025

# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"


# clear directory
rm(list=ls())


# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# load required packages
library("tidyverse") # version 2.0.0




#---------------------------------------------------------------------------------
# Datensatz laden
D <- read.csv(file = "Emo_Ident2.2025-02-13-1354.data.94dd9d4d-5621-4d23-ba8c-8fa5e436e90b.txt", 
              sep = " ", header = FALSE, 
              col.names = paste0("V", c(1:13))) # das zwingt ihn, einen Datensatz mit 14 Spalten zu machen



#[2] jetzt erstmal für alle Datensätze die richtigen Variablennamen zuweisen: 

names(D) <- c("File1", "tDur1", "File2", "tDur2", "Ident", "Word", "SpSex", "Block", "X", "Key", "RT", "ACC", "Blockorder")
#Fragen Sie unbedingt nach, wenn Sie eine Variable nicht verstehen! 
# t = target
 


#Durchchecken

D <- D %>% filter(Block != "Practise")

table(D$Ident, D$Word)

table(D$Ident)
table(D$Block)


table(D$Key)
table(D$ACC, D$Key)
table(D$ACC)
## End of Script