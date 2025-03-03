##########################################################################
## File: 01_Adapt_Identitäten_Expcheck.R
## Das Script checkt ob die ersten Testdatensätze des identitäts-Experiments in Ordnung sind. 
# author: Christine Nussbaum
# date 06/2024 

# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"


# clear directory
rm(list=ls())


# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# load required packages
library("tidyverse") # version 2.0.0




#---------------------------------------------------------------------------------
# Datensatz laden
D <- read.csv(file = "Identitäten_mitTopUp_JAVMEPS.2025-02-21-1637.data.4d06de39-c55c-4930-974d-701c6d73936b.txt", 
              sep = " ", header = FALSE, 
              col.names = paste0("V", c(1:14))) # das zwingt ihn, einen Datensatz mit 14 Spalten zu machen

#[1] Datensatz in die unterschiedlichen Blöcke aufspalten: 

#Baseline
Baseline <- D %>% filter(V1 == "Baseline") #check: sollte 112 Zeilen haben

#Adapt-Phasen:
Adapt <- D %>% filter(V1 == "Adaptation") #check: sollte 16 x 4 = 64 Zeilen haben

#Response-Phasen: 
Antwort <- D %>% filter(V1 == "Antwort") #check: sollte 112 x 1 = 224 Zeilen haben

#[2] jetzt erstmal für alle Datensätze die richtigen Variablennamen zuweisen: 

#Baseline
Baseline <- Baseline[, c(1:8)]
names(Baseline) <- c("Block", "tDuration", "tSpeaker", "tPseudoword", "tML", "tSpSex", "Resp", "RT")
#Fragen Sie unbedingt nach, wenn Sie eine Variable nicht verstehen! 
# t = target
#Resp: 1 = fear, 2 = anger 


#Adapt
Adapt <- Adapt[,c(1:8)]
names(Adapt) <- c("Block", "aDuration", "aSpeaker", "aPseudoword", "aEmo", "aSpSex", "AdaptCondition", "Blockorder")
# a = adapt


#Antwort
names(Antwort) <- c("Block", "tDuration", "tSpeaker", "tPseudoword", "tML", "tSpSex", "Resp", "RT",
                    "AdaptCondition", "Blockorder", "n", "x", "TopUp1", "TopUp2")


#[3] bisschen mehr Datenaufbereitung

#Rekodieren der Antwort in 0 und 1 

Antwort$Resp <- as.numeric(Antwort$Resp)
Antwort$Resp <- Antwort$Resp - 1
Baseline$Resp <- as.numeric(Baseline$Resp)
Baseline$Resp <- Baseline$Resp - 1


#jetzt müssen wir bei den TopUp Adaptoren mal aufräumen

Antwort$n <- ifelse(Antwort$n == 0, 4, Antwort$n) # erstmal aus allen n = 0 ein n = 4 machen, erkläre ich später gern nochmal

#in den weiteren Spalten werden die Infos jetzt nur aufgehoben in den Trials, in denen auch wirklich TopUp Adaptoren gezeigt wurden
Antwort$x <- ifelse(Antwort$n == 4, Antwort$x, NA)
Antwort$TopUp1 <- ifelse(Antwort$n == 4, Antwort$TopUp1, NA)
Antwort$TopUp2 <- ifelse(Antwort$n == 4, Antwort$TopUp2, NA)

#jetzt sind wir durch die grobe Datenaufbereitung einmal durch, jetzt können wir die Daten prüfen

#[4] Daten prüfen

#Baseline
table(Baseline$tSpeaker) # jeder Sprecher 28 mal gezogen, passt
table(Baseline$tSpSex) # je 56 weiblich und männlich, passt auch
table(Baseline$tML) # passt auch

#schauen wir uns an, ob jeder Stimulus genau einmal gezogen wurde, dazu müssen wir kurz eine neue Variable machen
Baseline$Stim <- paste(Baseline$tSpeaker, Baseline$tPseudoword, Baseline$tML, sep = "_")
length(unique(Baseline$Stim)) # müssen 112 sein, passt

#Adapt
table(Adapt$AdaptCondition) # passt
table(Adapt$AdaptCondition, Adapt$aSpSex) # getrennt nach Geschlecht, passt auch
table(Adapt$AdaptCondition, Adapt$aPseudoword) # passt
table(Adapt$AdaptCondition, Adapt$aEmo) # passt auch

#schauen wir uns wieder an, ob jeder Stimulus genau einmal gezogen wurde
Adapt$Stim <- paste(Adapt$aSpeaker, Adapt$aPseudoword, Adapt$aEmo, sep = "_")
length(unique(Adapt$Stim)) # müssen 32 sein, passt


#Antwort
table(Antwort$AdaptCondition) # passt
table(Antwort$AdaptCondition, Antwort$tSpSex) # getrennt nach Geschlecht, passt auch
table(Antwort$AdaptCondition, Antwort$tPseudoword) # passt
table(Antwort$AdaptCondition, Antwort$tML) # passt auch

#schauen wir uns wieder an, ob jeder Stimulus genau ZWEIMAL gezogen wurde
Antwort$Stim <- paste(Antwort$tSpeaker, Antwort$tPseudoword, Antwort$tML, sep = "_")
length(unique(Antwort$Stim)) # müssen 112 sein, passt
table(Antwort$Stim) # jeder zweimal gezogen

#alles schick bis hierhin. 

#schauen wir uns kurz die TopUp Adaptoren an: 
length(unique(Antwort$TopUp1)) # 16 verschiedene  plus NA
length(unique(Antwort$TopUp2)) # 16 verschiedene  plus NA

#wieviele wurden gezogen: 
table(!is.na(Antwort$TopUp1)) # 56 mal wird ein TopUp Adaptor gezogen, das passt
table(!is.na(Antwort$TopUp2))

#welche werdne gezogen: 
table(Antwort$x) # x wird ja komplett zufällig gezogen, das sieht man hier jetzt auch

#[5] Kurz auf Antwortmuster schauen, um Plausibilität zu prüfen
#-> mit steigendem morph level muss die Wahrscheinlichkeit für "Angry" antwort steigen
#das schauen wir uns für die Baseline noch ganz kurz an

#Daten aggregieren für die Baseline
B <- Baseline %>% group_by(tML) %>% summarise(Resp = mean(Resp), 
                                              N = length(Block))

#Grafik
ggplot(data = B, aes(x = tML, y = Resp)) + geom_point() # passt halbwegs

#nochmal für das Adaptationsblöcke: 

A <- Antwort %>% group_by(tML) %>% summarise(Resp = mean(Resp), 
                                              N = length(Block))
ggplot(data = A, aes(x = tML, y = Resp)) + geom_point() # passt halbwegs

#Fazit: Daten sind alle in Ordnung. Super Sache!
## End of Script