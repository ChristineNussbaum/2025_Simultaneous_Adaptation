#############################################################################################
### file: 03_data_analysis.R
### author: Christine Nussbaum
### contact: christine.nussbaum@uni-jena.de
### date: 06.2021

# R version 4.0.2 (2020-06-22)

# clear directory
rm(list=ls())

# set working directory

setwd("~/Arbeit/Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # dienstlicher PC
setwd("~/Christine/Arbeit/01_Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # privater PC


###2: load relevant packages
library(tidyverse)  # version 1.3.0
library(ez)         # version 4.4-0

###2: load relevant functions
source("functions/mySummary.R")
source("functions/tracedEZout.R")


###4: load data: ClassTask.RData
load(file = "input/ratingdata_prep.RData")




##############################################################################
########     Statistical analysis of Emotion classification      #############
##############################################################################
# E stands for Emotion

### Aggregate new dataset 
E <- D %>% group_by(Subject, Emo, Int) %>% summarise(Emo_ACC=mean(Emo_ACC))

# 20*7*4


#####################
###### ANOVA ########
# data = E
# dv = Emo_ACC
# wid = Subject 
# within = Emo, Int

ANOVAModel<-ezANOVA(data=E, dv=.(Emo_ACC), wid=.(Subject), within = .(Emo, Int), type=3, detailed = TRUE)
b = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
capture.output(b, file = "output/EMO_ANOVA.txt")


####################################################################
# $`--- FORMATTED RESULTS ------------------------------------`
# Effect                                        Text
# 1 (Intercept)   F(1, 19) = 2228.028, p < .001, np2 = .992
# 2         Emo  F(6, 114) =   19.591, p < .001, np2 = .508
# 3         Int   F(3, 57) =  912.349, p < .001, np2 = .980
# 4     Emo:Int F(18, 342) =   18.477, p < .001, np2 = .493
#####################################################################


#################################
###### Post-Hoc Analyses ########
# PH = post-hoc


################
###Main effects

###############  PH1: Main effect of Emotion category ###############

#reaggreate dataset
PH1 <- E %>% group_by(Subject, Emo) %>% summarise(Emo_ACC=mean(Emo_ACC))

#extract descriptive data
PH1_descriptive <- mySummary(PH1, Emo_ACC, Emo)

PH1_descriptive$CIlow <- PH1_descriptive$Emo_ACC - PH1_descriptive$CI
PH1_descriptive$CIhigh <- PH1_descriptive$Emo_ACC + PH1_descriptive$CI

# save output to textfile
capture.output(as.matrix(PH1_descriptive), file="output/Emo_PH1_HE-Emo.txt")

#remove objects to keep environment tidy
rm(PH1_descriptive, PH1)

# I will not calculate t.tests here... just too many options

###############  PH2: Main effect of Int  ###############

#reaggreate dataset
PH2 <- E %>% group_by(Subject, Int) %>% summarise(Emo_ACC=mean(Emo_ACC))

#extract descriptive data
PH2_descriptive <- mySummary(PH2, Emo_ACC, Int)

# perform pairwise t-tests
Ml100.75   <- PH2 %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH2 %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH2 %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Descriptive Data")

# save output to textfile
capture.output(info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, info[4],  as.matrix(PH2_descriptive), file="output/Emo_PH2_HE-Int.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH2_descriptive, PH2)

#######################################################################
# [1] "ML100 vs Ml75"
# t = 9.531, df = 19, p-value = 1.134e-08
# 
# [1] "ML75 vs Ml50"
# t = -22.801, df = 19, p-value = 2.902e-15
# 
# [1] "ML50 vs Ml25"
# t = -23.333, df = 19, p-value = 1.901e-15
# Int   Emo_ACC     SD           N    SE            CI          
# [1,] "100" "0.7729911" "0.06199912" "20" "0.013863424" "0.02901648"
# [2,] "25"  "0.2477679" "0.04206615" "20" "0.009406276" "0.01968756"
# [3,] "50"  "0.5250000" "0.07258521" "20" "0.016230546" "0.03397092"
# [4,] "75"  "0.7062500" "0.06423152" "20" "0.014362604" "0.03006128"
#######################################################################


###############  PH3: Interaction of Emo and Int ###############
#reaggreate dataset
PH3 <-  E %>% group_by(Subject, Emo, Int) %>% summarise(Emo_ACC=mean(Emo_ACC))

#extract descriptive data
PH3_descriptive <- mySummary(PH3, Emo_ACC, Emo, Int)

# save output to textfile
capture.output( as.matrix(PH3_descriptive), file="output/Emo_PH3_Int-EmoxInt_descriptives.txt")

rm(PH3_descriptive)

#############
### ang only: 

PH3_ang <- PH3 %>% filter(Emo == "ang")

# perform pairwise t-tests
Ml100.75   <- PH3_ang %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_ang %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_ang %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Ang")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Ang.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_ang)

#############
### fea only: 

PH3_fea <- PH3 %>% filter(Emo == "fea")

# perform pairwise t-tests
Ml100.75   <- PH3_fea %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_fea %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_fea %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Fea")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Fea.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_fea)


##############
### hap only: 

PH3_hap <- PH3 %>% filter(Emo == "hap")

# perform pairwise t-tests
Ml100.75   <- PH3_hap %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_hap %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_hap %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Hap")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Hap.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_hap)

##############
### Ple only: 

PH3_ple <- PH3 %>% filter(Emo == "ple")

# perform pairwise t-tests
Ml100.75   <- PH3_ple %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_ple %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_ple %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Ple")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Ple.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_ple)


##############
### Sad only: 

PH3_sad <- PH3 %>% filter(Emo == "sad")

# perform pairwise t-tests
Ml100.75   <- PH3_sad %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_sad %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_sad %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Sad")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Sad.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_sad)


##############
### Sur only: 

PH3_sur <- PH3 %>% filter(Emo == "sur")

# perform pairwise t-tests
Ml100.75   <- PH3_sur %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_sur %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_sur %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Sur")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Sur.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_sur)


##############
### Dis only: 

PH3_dis <- PH3 %>% filter(Emo == "dis")

# perform pairwise t-tests
Ml100.75   <- PH3_dis %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml75.50   <- PH3_dis %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)
Ml50.25   <- PH3_dis %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Emo_ACC ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Dis")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Emo_PH3_Int-EmoxInt_Dis.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_dis, PH3, E)


##############################################################################
########     Statistical analysis of Intensity Ratings           #############
##############################################################################

D$Int_RESP <- as.numeric(as.character(D$Int_RESP))
D$Int_RESP <- D$Int_RESP/25

# I stands for Emotion

### Aggregate new dataset 
I <- D %>% group_by(Subject, Emo, Int) %>% summarise(Int_RESP=mean(Int_RESP))

# 20*7*4


#####################
###### ANOVA ########
# data = I
# dv = Int_RESP
# wid = Subject 
# within = Emo, Int

ANOVAModel<-ezANOVA(data=I, dv=.(Int_RESP), wid=.(Subject), within = .(Emo, Int), type=3, detailed = TRUE)
b = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
capture.output(b, file = "output/INT_ANOVA.txt")


####################################################################
# $`--- FORMATTED RESULTS ------------------------------------`
# Effect                                        Text
# 1 (Intercept)   F(1, 19) = 2530.640, p < .001, np2 = .993
# 2         Emo  F(6, 114) =    6.277, p < .001, np2 = .248
# 3         Int   F(3, 57) =  506.016, p < .001, np2 = .964
# 4     Emo:Int F(18, 342) =   22.780, p < .001, np2 = .545
#####################################################################

###############  PH1: Main effect of Emotion category ###############

#reaggreate dataset
PH1 <- I %>% group_by(Subject, Emo) %>% summarise(Int_RESP=mean(Int_RESP))

#extract descriptive data
PH1_descriptive <- mySummary(PH1, Int_RESP, Emo)

PH1_descriptive$CIlow <- PH1_descriptive$Int_RESP - PH1_descriptive$CI
PH1_descriptive$CIhigh <- PH1_descriptive$Int_RESP + PH1_descriptive$CI

# save output to textfile
capture.output(as.matrix(PH1_descriptive), file="output/Int_PH1_HE-Emo.txt")

#remove objects to keep environment tidy
rm(PH1_descriptive, PH1)

# I will not calculate t.tests here... just too many options

###############  PH2: Main effect of Int  ###############

#reaggreate dataset
PH2 <- I %>% group_by(Subject, Int) %>% summarise(Int_RESP=mean(Int_RESP))

#extract descriptive data
PH2_descriptive <- mySummary(PH2, Int_RESP, Int)

# perform pairwise t-tests
Ml100.75   <- PH2 %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH2 %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH2 %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Descriptive Data")

# save output to textfile
capture.output(info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, info[4],  as.matrix(PH2_descriptive), file="output/Int_PH2_HE-Int.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH2_descriptive, PH2)


###############  PH3: Interaction of Emo and Int ###############
#reaggreate dataset
PH3 <-  I %>% group_by(Subject, Emo, Int) %>% summarise(Int_RESP=mean(Int_RESP))

#extract descriptive data
PH3_descriptive <- mySummary(PH3, Int_RESP, Emo, Int)

# save output to textfile
capture.output( as.matrix(PH3_descriptive), file="output/Int_PH3_Int-EmoxInt_descriptives.txt")

rm(PH3_descriptive)

#############
### ang only: 

PH3_ang <- PH3 %>% filter(Emo == "ang")

# perform pairwise t-tests
Ml100.75   <- PH3_ang %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_ang %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_ang %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Ang")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Ang.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_ang)

#############
### fea only: 

PH3_fea <- PH3 %>% filter(Emo == "fea")

# perform pairwise t-tests
Ml100.75   <- PH3_fea %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_fea %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_fea %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Fea")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Fea.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_fea)


##############
### hap only: 

PH3_hap <- PH3 %>% filter(Emo == "hap")

# perform pairwise t-tests
Ml100.75   <- PH3_hap %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_hap %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_hap %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Hap")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Hap.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_hap)

##############
### Ple only: 

PH3_ple <- PH3 %>% filter(Emo == "ple")

# perform pairwise t-tests
Ml100.75   <- PH3_ple %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_ple %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_ple %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Ple")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Ple.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_ple)


##############
### Sad only: 

PH3_sad <- PH3 %>% filter(Emo == "sad")

# perform pairwise t-tests
Ml100.75   <- PH3_sad %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_sad %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_sad %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Sad")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Sad.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_sad)


##############
### Sur only: 

PH3_sur <- PH3 %>% filter(Emo == "sur")

# perform pairwise t-tests
Ml100.75   <- PH3_sur %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_sur %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_sur %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Sur")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Sur.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_sur)


##############
### Dis only: 

PH3_dis <- PH3 %>% filter(Emo == "dis")

# perform pairwise t-tests
Ml100.75   <- PH3_dis %>% filter(Int == "100" | Int == "75") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml75.50   <- PH3_dis %>% filter(Int == "75" | Int == "50") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)
Ml50.25   <- PH3_dis %>% filter(Int == "50" | Int == "25") %>% t.test(data = ., Int_RESP ~ Int, paired=TRUE)

#add descriptions for output:
info <- c("ML100 vs Ml75","ML75 vs Ml50", "ML50 vs Ml25", "Dis")

# save output to textfile
capture.output(info[4], info[1], Ml100.75,info[2],Ml75.50, info[3],Ml50.25, file="output/Int_PH3_Int-EmoxInt_Dis.txt")

#remove objects to keep environment tidy
rm(info, Ml100.75, Ml75.50, Ml50.25, PH3_dis)


### End of Script