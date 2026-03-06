############################################
## file: auditoryanalysis of emotional stimuli
## author: Tine Nussbaum
## date: 03/2026

# this is the auditory analysis of the targets used in the parameter specific voice adaptation paradigm, Nussbaum et al. 2020

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load relevant packages
library(tidyverse)

# load relevant functions
source("functions/mySummary.R") 

#---------------------------------------------
# adaptors


# import data
adaptor_male <- read.table(file = "input/adaptor_male_info.txt", sep = ",", header = TRUE)
adaptor_female <- read.table(file = "input/adaptor_female_info.txt", sep = ",", header = TRUE)

#paste them together

adaptor <- rbind(adaptor_male, adaptor_female)


# tidy data
filename<- matrix(data = unlist(str_split(adaptor$filename, "_")), ncol = 4, byrow = TRUE)

filename <- as.data.frame(filename[,1:3])                 
names(filename)<- c("SpID", "Word", "Emo")
adaptor <- cbind(adaptor, filename)
rm(filename)

adaptor$SpSex <- ifelse(str_detect(adaptor$SpID, "f"), "f", "m")
adaptor$stim <- paste(adaptor$SpID, adaptor$Word, sep = "_")

adaptor <- adaptor[,c(1, 25:29, 2:24)]

# aggregate all information that is interesting

info_adapt <- adaptor %>% group_by(SpSex, Emo) %>% summarise(f0 = mean(f0), 
                                                 f0SD = mean(f0SD), 
                                                 f0intonation = mean(f0intonation),
                                                 f0Glide = mean(f0Glide),
                                                 #formDisp = mean(formDisp),
                                                 alphaRatio = mean(alphaRatio),
                                                 hnr = mean(hnr),
                                                 jitRap = mean(jitRap),
                                                 shimAPQ3 = mean(shimAPQ3), 
                                                 duration = mean(duration), 
                                                 N = length(Emo))



dur_adapt <- summary(adaptor$duration)
sd_adapt <- sd(adaptor$duration)

# capture output adaptor infos
capture.output(as.matrix(info_adapt), dur_adapt, sd_adapt, file= "output/infos_adaptors.txt")

#keep environment tidy
rm(info_adapt, dur_adapt, adaptor_female, adaptor_male,  sd_adapt)


#---------------------------------------------
# targets


# import data
target_male <- read.table(file = "input/target_male_info.txt", sep = ",", header = TRUE)
target_female <- read.table(file = "input/target_female_info.txt", sep = ",", header = TRUE)

#paste them together

target <- rbind(target_male, target_female)


# tidy data
filename<- matrix(data = unlist(str_split(target$filename, "_")), ncol = 4, byrow = TRUE)

filename <- as.data.frame(filename[,1:3])                 
names(filename)<- c("SpID", "Word", "tML")
target <- cbind(target, filename)
rm(filename)

target$SpSex <- ifelse(str_detect(target$SpID, "f"), "f", "m")
target$stim <- paste(target$SpID, target$Word, sep = "_")

target <- target[,c(1, 25:29, 2:24)]

# aggregate all information that is interesting

info_target <- target %>% group_by(SpSex, tML) %>% summarise(f0 = mean(f0), 
                                                             f0SD = mean(f0SD), 
                                                             f0intonation = mean(f0intonation),
                                                             f0Glide = mean(f0Glide),
                                                             #formDisp = mean(formDisp),
                                                             alphaRatio = mean(alphaRatio),
                                                             hnr = mean(hnr),
                                                             jitRap = mean(jitRap),
                                                             shimAPQ3 = mean(shimAPQ3), 
                                                             duration = mean(duration), 
                                                             N = length(tML))



dur_target <- summary(target$duration)
sd_target <- sd(target$duration)

# capture output target infos
capture.output(as.matrix(info_target), dur_target,sd_target, file= "output/infos_targets.txt")

#keep environment tidy
rm(info_target, dur_target, target_female, target_male, sd_target)


#---------------------------------------------
# extract duration for both together

#rename one
target <- target %>% rename(Emo = tML)

all <- rbind(adaptor, target)

dur_all <- summary(all$duration)
sd_all <- sd(all$duration)

# capture output of duration infos
capture.output( dur_all,sd_all, file= "output/infos_all_duration.txt")


#End of Script