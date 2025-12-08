##########################################################################
## File: 03_Exp3_data_analysis.R
## Data Analysis for Exp 3: Adaptation of Emotion - pseudowords
# author: Christine Nussbaum 
# date 03/2025, updated 12/2025

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library("tidyverse")
library(ez)
library(effectsize)
library(lme4)           # version 1.1-35.1
library(ggeffects)      # version 1.6.0
library(afex)           # version 1.3-1

# load relevant functions
source("functions/mySummary.R") 
source("functions/tracedEzOut.R")



#---------------------------------------------------------------------------------
#get the CG input data:

#load the data
load(file ="input/Exp3_without_omissions.RData")

#-------------------------------------------------------------------------------#
#                  Analysis 1: logistic regression                              #
#-------------------------------------------------------------------------------#


## declare tML as numerical variable
E3_Adapt$tML <- as.numeric(as.character(E3_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E3_Adapt$tML_sc <- scale(E3_Adapt$tML)[,1]

#rename "w5" into "w4"

E3_Adapt$Word <- ifelse(E3_Adapt$Word == "w05", "w04", E3_Adapt$Word)
E3_Bline$Word <- ifelse(E3_Bline$Word == "w05", "w04", E3_Bline$Word)

# outcome: Resp
# predictors: ML, Word, AdaptType, SpSex
# random: Participant, SpID


#------------------------------------------------------------------------
# Step1: estimating the model 

# m: assumed fixed effects structure for Participant and SpID
m <- glmer(Resp ~ tML_sc * Word * AdaptType *SpSex   + (1|SpID) + (1 | Participant), data = E3_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked


#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E3_Adapt, method = "LRT", family = "binomial") # almost an hour to estimate

m_test

# Model: Resp ~ tML_sc * Word * AdaptType * SpSex + (1 | SpID) + (1 | Participant)
# Data: E3_Adapt
# Df full model: 34
# Effect                        df  Chisq     p.value
# 1                       tML_sc  1 4175.47 ***   <.001
# 2                         Word  3  453.41 ***   <.001
# 3                    AdaptType  1     8.13 **    .004
# 4                        SpSex  1        0.00    .963
# 5                  tML_sc:Word  3   39.12 ***   <.001
# 6             tML_sc:AdaptType  1        0.35    .552
# 7               Word:AdaptType  3        0.52    .914
# 8                 tML_sc:SpSex  1      5.31 *    .021
# 9                   Word:SpSex  3   50.93 ***   <.001
# 10             AdaptType:SpSex  1        0.39    .531
# 11       tML_sc:Word:AdaptType  3      8.05 *    .045
# 12           tML_sc:Word:SpSex  3   49.11 ***   <.001
# 13      tML_sc:AdaptType:SpSex  1      2.80 +    .094
# 14        Word:AdaptType:SpSex  3        1.42    .700
# 15 tML_sc:Word:AdaptType:SpSex  3        4.20    .241

summary(m) # this is very long


# save models as R objects
save(m, m_test, file= "input/E3_GLMs.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp3_GLMM.txt")

# extract fitted values
E3_Adapt$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E3_Adapt, fitted, Participant, Word, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, Word, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp3_GLMM.txt")


#aggregate values of fitted, separate for SpSeX
GLM_fit <- mySummary(E3_Adapt, fitted, Participant, SpSex, Word, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpSex,  Word, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp3_GLMM_perSpeSex.txt")


#-------------------------------------------------------------------------------#
#   Analysis 2: The amount of classifications (averaged across tML)             #
#-------------------------------------------------------------------------------#

#aggregate across pseudoword, AdaptType and tML for analysis
E3_Resp <- mySummary(E3_Adapt, Resp, Participant, Word, AdaptType) #does not contain Baseline


##########################################################################
## ANOVA II: 2 x 2 x 4 within subject
# data: E3_Resp
# dv: Resp
# wid: Participant
# within: AdaptType, Word, SpSex

a<-ezANOVA(data=E3_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 42 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept)  F(1,41) = 945.709, p < .001, np2 = .958
# 2      AdaptType  F(1,41) =   4.829, p = .034, np2 = .105
# 3           Word F(3,123) = 110.770, p < .001, np2 = .730
# 4 AdaptType:Word F(3,123) =   0.508, p = .678, np2 = .012
##############################################################################

capture.output(b, file= "output/Exp3_Resp_ANOVAII.txt")

# visualize data (just to check)
ezPlot(data=E3_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), split=Word, x=AdaptType)



########################################################################################
### Post-Hoc Analysis: Interaction of AdaptType und Word, also including the Baseline
########################################################################################

#aggregate Data (in this case not really necessary)
PH1 <- E3_Resp %>% group_by(Participant, Word, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, Resp, Word, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, Resp)

###separate per speaker sex
w01<- PH1 %>% filter(Word == "w01")
w02<- PH1 %>% filter(Word == "w02")
w03<- PH1 %>% filter(Word == "w03")
w04<- PH1 %>% filter(Word == "w04")

###now we add the Baseline information 
E3_Bline_Resp <- mySummary(E3_Bline, Resp, Participant, Word) 
E3_Bline_Resp <- E3_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E3_Bline_Resp, Baseline, Word)


Blinew01<- E3_Bline_Resp[,1:3] %>% filter(Word == "w01")
w01 <- merge(w01, Blinew01)

Blinew02<- E3_Bline_Resp[,1:3] %>% filter(Word == "w02")
w02 <- merge(w02, Blinew02)

Blinew03<- E3_Bline_Resp[,1:3] %>% filter(Word == "w03")
w03 <- merge(w03, Blinew03)

Blinew04<- E3_Bline_Resp[,1:3] %>% filter(Word == "w04")
w04 <- merge(w04, Blinew04)

rm(E3_Bline_Resp, Blinew01, Blinew02,Blinew03,Blinew04)


### t-tests, separately for each Word

###########
#Word w01:

AdaptType_w01 <- t.test(w01$`pw12_ang/pw34_fea`, w01$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w01 <- t_to_d(AdaptType_w01$statistic, AdaptType_w01$parameter, paired = TRUE)
##################################################
# data:  w01$`pw12_ang/pw34_fea` and w01$`pw12_fea/pw34_ang`
# t = 2.452, df = 41, p-value = 0.01855
 
AdaptType_w01b <- t.test(w01$Baseline, w01$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_w01b <- t_to_d(AdaptType_w01b$statistic, AdaptType_w01b$parameter, paired = TRUE)
##################################################
# data:  w01$Baseline and w01$`pw12_ang/pw34_fea`
# t = -1.5053, df = 41, p-value = 0.1399

AdaptType_w01b2 <- t.test(w01$Baseline, w01$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w01b2 <- t_to_d(AdaptType_w01b2$statistic, AdaptType_w01b2$parameter, paired = TRUE)
##################################################
# data:  w01$Baseline and w01$`pw12_fea/pw34_ang`
# t = 0.55119, df = 41, p-value = 0.5845


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_w01,d_AdaptType_w01, 
               AdaptType_w01b, d_AdaptType_w01b,
               AdaptType_w01b2, d_AdaptType_w01b2,
               file = "output/Exp3_Resp_post-hoc_w01.txt")

#keep environment tidy
rm(AdaptType_w01,d_AdaptType_w01, AdaptType_w01b, d_AdaptType_w01b, AdaptType_w01b2, d_AdaptType_w01b2)


###########
#Word w02:

AdaptType_w02 <- t.test(w02$`pw12_ang/pw34_fea`, w02$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w02 <- t_to_d(AdaptType_w02$statistic, AdaptType_w02$parameter, paired = TRUE)
##################################################
# data:  w02$`pw12_ang/pw34_fea` and w02$`pw12_fea/pw34_ang`
# t = 1.4386, df = 41, p-value = 0.1578

AdaptType_w02b <- t.test(w02$Baseline, w02$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_w02b <- t_to_d(AdaptType_w02b$statistic, AdaptType_w02b$parameter, paired = TRUE)
##################################################
# data:  w02$Baseline and w02$`pw12_ang/pw34_fea`
# t = -0.30945, df = 41, p-value = 0.7585

AdaptType_w02b2 <- t.test(w02$Baseline, w02$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w02b2 <- t_to_d(AdaptType_w02b2$statistic, AdaptType_w02b2$parameter, paired = TRUE)
##################################################
# data:  w02$Baseline and w02$`pw12_fea/pw34_ang`
# t = 0.84541, df = 41, p-value = 0.4028


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_w02,d_AdaptType_w02, 
               AdaptType_w02b, d_AdaptType_w02b,
               AdaptType_w02b2, d_AdaptType_w02b2,
               file = "output/Exp3_Resp_post-hoc_w02.txt")

#keep environment tidy
rm(AdaptType_w02,d_AdaptType_w02, AdaptType_w02b, d_AdaptType_w02b, AdaptType_w02b2, d_AdaptType_w02b2)

###########
#Word w03:

AdaptType_w03 <- t.test(w03$`pw12_ang/pw34_fea`, w03$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w03 <- t_to_d(AdaptType_w03$statistic, AdaptType_w03$parameter, paired = TRUE)
##################################################
# data:  w03$`pw12_ang/pw34_fea` and w03$`pw12_fea/pw34_ang`
# t = 0.93714, df = 41, p-value = 0.3542

AdaptType_w03b <- t.test(w03$Baseline, w03$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_w03b <- t_to_d(AdaptType_w03b$statistic, AdaptType_w03b$parameter, paired = TRUE)
##################################################
# data:  w03$Baseline and w03$`pw12_ang/pw34_fea`
# t = -1.5826, df = 41, p-value = 0.1212

AdaptType_w03b2 <- t.test(w03$Baseline, w03$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w03b2 <- t_to_d(AdaptType_w03b2$statistic, AdaptType_w03b2$parameter, paired = TRUE)
##################################################
# data:  w03$Baseline and w03$`pw12_fea/pw34_ang`
# t = -0.8916, df = 41, p-value = 0.377


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_w03,d_AdaptType_w03, 
               AdaptType_w03b, d_AdaptType_w03b,
               AdaptType_w03b2, d_AdaptType_w03b2,
               file = "output/Exp3_Resp_post-hoc_w03.txt")

#keep environment tidy
rm(AdaptType_w03,d_AdaptType_w03, AdaptType_w03b, d_AdaptType_w03b, AdaptType_w03b2, d_AdaptType_w03b2)


###########
#Word w04:

AdaptType_w04 <- t.test(w04$`pw12_ang/pw34_fea`, w04$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w04 <- t_to_d(AdaptType_w04$statistic, AdaptType_w04$parameter, paired = TRUE)
##################################################
# data:  w04$`pw12_ang/pw34_fea` and w04$`pw12_fea/pw34_ang`
# t = 0.90317, df = 41, p-value = 0.3717

AdaptType_w04b <- t.test(w04$Baseline, w04$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_w04b <- t_to_d(AdaptType_w04b$statistic, AdaptType_w04b$parameter, paired = TRUE)
##################################################
# data:  w04$Baseline and w04$`pw12_ang/pw34_fea`
# t = -3.7454, df = 41, p-value = 0.0005546

AdaptType_w04b2 <- t.test(w04$Baseline, w04$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_w04b2 <- t_to_d(AdaptType_w04b2$statistic, AdaptType_w04b2$parameter, paired = TRUE)
##################################################
# data:  w04$Baseline and w04$`pw12_fea/pw34_ang`
# t = -2.2003, df = 41, p-value = 0.033487


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_w04,d_AdaptType_w04, 
               AdaptType_w04b, d_AdaptType_w04b,
               AdaptType_w04b2, d_AdaptType_w04b2,
               file = "output/Exp3_Resp_post-hoc_w04.txt")

#keep environment tidy
rm(AdaptType_w04,d_AdaptType_w04, AdaptType_w04b, d_AdaptType_w04b, AdaptType_w04b2, d_AdaptType_w04b2)


#prepare for JASP
JASP <- rbind(w01, w02, w03, w04)

names(JASP)

JASP <- pivot_wider(JASP[,1:5], names_from = c(Word), values_from = c(`pw12_ang/pw34_fea`:Baseline))

write.csv(JASP, file = "JASP/pw_analysis.csv")


#-------------------------------------------------------------------------------#
#   Analysis 2b: Post-hoc Analysis separately for female voices only            #
#-------------------------------------------------------------------------------#

E3_Resp <- mySummary(E3_Adapt, Resp, Participant, Word, AdaptType, SpSex) #does not contain Baseline
E3_RespF <- E3_Resp %>% filter(SpSex =="f")


##########################################################################
## ANOVA II: 2 x 2 x 4 within subject
# data: E3_Resp
# dv: Resp
# wid: Participant
# within: AdaptType, Word, SpSex

a<-ezANOVA(data=E3_RespF, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 42 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept)  F(1,41) = 945.709, p < .001, np2 = .958
# 2      AdaptType  F(1,41) =   4.829, p = .034, np2 = .105
# 3           Word F(3,123) = 110.770, p < .001, np2 = .730
# 4 AdaptType:Word F(3,123) =   0.508, p = .678, np2 = .012
##############################################################################

capture.output(b, file= "output/Exp3_Resp_ANOVAIIb_females.txt")

# visualize data (just to check)
ezPlot(data=E3_RespF, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), split=Word, x=AdaptType)



########################################################################################
### Post-Hoc Analysis: Interaction of AdaptType und Word, also including the Baseline
########################################################################################

#aggregate Data (in this case not really necessary)
PH1 <- E3_RespF %>% group_by(Participant, Word, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, Resp, Word, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, Resp)

###separate per speaker sex
Fw01<- PH1 %>% filter(Word == "w01")
Fw02<- PH1 %>% filter(Word == "w02")
Fw03<- PH1 %>% filter(Word == "w03")
Fw04<- PH1 %>% filter(Word == "w04")

###now we add the Baseline information 
E3_Bline_Resp <- mySummary(E3_Bline, Resp, Participant, Word, SpSex) 
E3_Bline_Resp <- E3_Bline_Resp %>% filter(SpSex == "f")
E3_Bline_Resp <- E3_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E3_Bline_Resp, Baseline, Word)


BlineFw01<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w01")
Fw01 <- merge(Fw01, BlineFw01)

BlineFw02<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w02")
Fw02 <- merge(Fw02, BlineFw02)

BlineFw03<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w03")
Fw03 <- merge(Fw03, BlineFw03)

BlineFw04<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w04")
Fw04 <- merge(Fw04, BlineFw04)

rm(E3_Bline_Resp, BlineFw01, BlineFw02,BlineFw03,BlineFw04)


### t-tests, separately for each Word

###########
#Word Fw01:

AdaptType_Fw01 <- t.test(Fw01$`pw12_ang/pw34_fea`, Fw01$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw01 <- t_to_d(AdaptType_Fw01$statistic, AdaptType_Fw01$parameter, paired = TRUE)
##################################################
# data:  Fw01$`pw12_ang/pw34_fea` and Fw01$`pw12_fea/pw34_ang`
# t = 0.98871, df = 41, p-value = 0.328

AdaptType_Fw01b <- t.test(Fw01$Baseline, Fw01$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Fw01b <- t_to_d(AdaptType_Fw01b$statistic, AdaptType_Fw01b$parameter, paired = TRUE)
##################################################
# data:  Fw01$Baseline and Fw01$`pw12_ang/pw34_fea`
# t = -0.56487, df = 41, p-value = 0.5752

AdaptType_Fw01b2 <- t.test(Fw01$Baseline, Fw01$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw01b2 <- t_to_d(AdaptType_Fw01b2$statistic, AdaptType_Fw01b2$parameter, paired = TRUE)
##################################################
# data:  Fw01$Baseline and Fw01$`pw12_fea/pw34_ang`
# t = 0.25583, df = 41, p-value = 0.7994


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Fw01,d_AdaptType_Fw01, 
               AdaptType_Fw01b, d_AdaptType_Fw01b,
               AdaptType_Fw01b2, d_AdaptType_Fw01b2,
               file = "output/Exp3_Resp_post-hoc_Fw01.txt")

#keep environment tidy
rm(AdaptType_Fw01,d_AdaptType_Fw01, AdaptType_Fw01b, d_AdaptType_Fw01b, AdaptType_Fw01b2, d_AdaptType_Fw01b2)


###########
#Word Fw02:

AdaptType_Fw02 <- t.test(Fw02$`pw12_ang/pw34_fea`, Fw02$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw02 <- t_to_d(AdaptType_Fw02$statistic, AdaptType_Fw02$parameter, paired = TRUE)
##################################################
# data:  Fw02$`pw12_ang/pw34_fea` and Fw02$`pw12_fea/pw34_ang`
# t = 0.62178, df = 41, p-value = 0.5375

AdaptType_Fw02b <- t.test(Fw02$Baseline, Fw02$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Fw02b <- t_to_d(AdaptType_Fw02b$statistic, AdaptType_Fw02b$parameter, paired = TRUE)
##################################################
# data:  Fw02$Baseline and Fw02$`pw12_ang/pw34_fea`
# t = 0.22814, df = 41, p-value = 0.8207

AdaptType_Fw02b2 <- t.test(Fw02$Baseline, Fw02$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw02b2 <- t_to_d(AdaptType_Fw02b2$statistic, AdaptType_Fw02b2$parameter, paired = TRUE)
##################################################
# data:  Fw02$Baseline and Fw02$`pw12_fea/pw34_ang`
# t = 0.91553, df = 41, p-value = 0.3653


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Fw02,d_AdaptType_Fw02, 
               AdaptType_Fw02b, d_AdaptType_Fw02b,
               AdaptType_Fw02b2, d_AdaptType_Fw02b2,
               file = "output/Exp3_Resp_post-hoc_Fw02.txt")

#keep environment tidy
rm(AdaptType_Fw02,d_AdaptType_Fw02, AdaptType_Fw02b, d_AdaptType_Fw02b, AdaptType_Fw02b2, d_AdaptType_Fw02b2)

###########
#Word Fw03:

AdaptType_Fw03 <- t.test(Fw03$`pw12_ang/pw34_fea`, Fw03$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw03 <- t_to_d(AdaptType_Fw03$statistic, AdaptType_Fw03$parameter, paired = TRUE)
##################################################
# data:  Fw03$`pw12_ang/pw34_fea` and Fw03$`pw12_fea/pw34_ang`
# t = 0.30943, df = 41, p-value = 0.7586

AdaptType_Fw03b <- t.test(Fw03$Baseline, Fw03$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Fw03b <- t_to_d(AdaptType_Fw03b$statistic, AdaptType_Fw03b$parameter, paired = TRUE)
##################################################
# data:  Fw03$Baseline and Fw03$`pw12_ang/pw34_fea`
# t = -1.3768, df = 41, p-value = 0.176

AdaptType_Fw03b2 <- t.test(Fw03$Baseline, Fw03$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw03b2 <- t_to_d(AdaptType_Fw03b2$statistic, AdaptType_Fw03b2$parameter, paired = TRUE)
##################################################
# data:  Fw03$Baseline and Fw03$`pw12_fea/pw34_ang`
# t = -1.2831, df = 41, p-value = 0.2067


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Fw03,d_AdaptType_Fw03, 
               AdaptType_Fw03b, d_AdaptType_Fw03b,
               AdaptType_Fw03b2, d_AdaptType_Fw03b2,
               file = "output/Exp3_Resp_post-hoc_Fw03.txt")

#keep environment tidy
rm(AdaptType_Fw03,d_AdaptType_Fw03, AdaptType_Fw03b, d_AdaptType_Fw03b, AdaptType_Fw03b2, d_AdaptType_Fw03b2)


###########
#Word Fw04:

AdaptType_Fw04 <- t.test(Fw04$`pw12_ang/pw34_fea`, Fw04$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw04 <- t_to_d(AdaptType_Fw04$statistic, AdaptType_Fw04$parameter, paired = TRUE)
##################################################
# data:  Fw04$`pw12_ang/pw34_fea` and Fw04$`pw12_fea/pw34_ang`
# t = 0.071668, df = 41, p-value = 0.9432

AdaptType_Fw04b <- t.test(Fw04$Baseline, Fw04$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Fw04b <- t_to_d(AdaptType_Fw04b$statistic, AdaptType_Fw04b$parameter, paired = TRUE)
##################################################
# data:  Fw04$Baseline and Fw04$`pw12_ang/pw34_fea`
# t = -1.8831, df = 41, p-value = 0.0668

AdaptType_Fw04b2 <- t.test(Fw04$Baseline, Fw04$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Fw04b2 <- t_to_d(AdaptType_Fw04b2$statistic, AdaptType_Fw04b2$parameter, paired = TRUE)
##################################################
# data:  Fw04$Baseline and Fw04$`pw12_fea/pw34_ang`
# t = -1.6428, df = 41, p-value = 0.1081


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Fw04,d_AdaptType_Fw04, 
               AdaptType_Fw04b, d_AdaptType_Fw04b,
               AdaptType_Fw04b2, d_AdaptType_Fw04b2,
               file = "output/Exp3_Resp_post-hoc_Fw04.txt")

#keep environment tidy
rm(AdaptType_Fw04,d_AdaptType_Fw04, AdaptType_Fw04b, d_AdaptType_Fw04b, AdaptType_Fw04b2, d_AdaptType_Fw04b2)

#-------------------------------------------------------------------------------#
#   Analysis 2c: Post-hoc Analysis separately for male voices only            #
#-------------------------------------------------------------------------------#

E3_Resp <- mySummary(E3_Adapt, Resp, Participant, Word, AdaptType, SpSex) #does not contain Baseline
E3_RespM <- E3_Resp %>% filter(SpSex =="m")


##########################################################################
## ANOVA II: 2 x 2 x 4 within subject
# data: E3_Resp
# dv: Resp
# wid: Participant
# within: AdaptType, Word, SpSex

a<-ezANOVA(data=E3_RespM, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 42 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept)  F(1,41) = 622.299, p < .001, np2 = .938
# 2      AdaptType  F(1,41) =   6.662, p = .014, np2 = .140
# 3           Word F(3,123) =  43.314, p < .001, np2 = .514
# 4 AdaptType:Word F(3,123) =   0.416, p = .742, np2 = .010
##############################################################################

capture.output(b, file= "output/Exp3_Resp_ANOVAIIc_males.txt")

# visualize data (just to check)
ezPlot(data=E3_RespM, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), split=Word, x=AdaptType)



########################################################################################
### Post-Hoc Analysis: Interaction of AdaptType und Word, also including the Baseline
########################################################################################

#aggregate Data (in this case not really necessary)
PH1 <- E3_RespM %>% group_by(Participant, Word, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, Resp, Word, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, Resp)

###separate per speaker sex
Mw01<- PH1 %>% filter(Word == "w01")
Mw02<- PH1 %>% filter(Word == "w02")
Mw03<- PH1 %>% filter(Word == "w03")
Mw04<- PH1 %>% filter(Word == "w04")

###now we add the Baseline information 
E3_Bline_Resp <- mySummary(E3_Bline, Resp, Participant, Word, SpSex) 
E3_Bline_Resp <- E3_Bline_Resp %>% filter(SpSex == "m")
E3_Bline_Resp <- E3_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E3_Bline_Resp, Baseline, Word)


BlineMw01<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w01")
Mw01 <- merge(Mw01, BlineMw01)

BlineMw02<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w02")
Mw02 <- merge(Mw02, BlineMw02)

BlineMw03<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w03")
Mw03 <- merge(Mw03, BlineMw03)

BlineMw04<- E3_Bline_Resp[,c(1,2,4)] %>% filter(Word == "w04")
Mw04 <- merge(Mw04, BlineMw04)

rm(E3_Bline_Resp, BlineMw01, BlineMw02,BlineMw03,BlineMw04)


### t-tests, separately for each Word

###########
#Word Mw01:

AdaptType_Mw01 <- t.test(Mw01$`pw12_ang/pw34_fea`, Mw01$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw01 <- t_to_d(AdaptType_Mw01$statistic, AdaptType_Mw01$parameter, paired = TRUE)
##################################################
# data:  Mw01$`pw12_ang/pw34_fea` and Mw01$`pw12_fea/pw34_ang`
# t = 2.5488, df = 41, p-value = 0.01465

AdaptType_Mw01b <- t.test(Mw01$Baseline, Mw01$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Mw01b <- t_to_d(AdaptType_Mw01b$statistic, AdaptType_Mw01b$parameter, paired = TRUE)
##################################################
# data:  Mw01$Baseline and Mw01$`pw12_ang/pw34_fea`
# t = -1.3673, df = 41, p-value = 0.179

AdaptType_Mw01b2 <- t.test(Mw01$Baseline, Mw01$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw01b2 <- t_to_d(AdaptType_Mw01b2$statistic, AdaptType_Mw01b2$parameter, paired = TRUE)
##################################################
# data:  Mw01$Baseline and Mw01$`pw12_fea/pw34_ang`
# t = 0.51058, df = 41, p-value = 0.6124


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Mw01,d_AdaptType_Mw01, 
               AdaptType_Mw01b, d_AdaptType_Mw01b,
               AdaptType_Mw01b2, d_AdaptType_Mw01b2,
               file = "output/Exp3_Resp_post-hoc_Mw01.txt")

#keep environment tidy
rm(AdaptType_Mw01,d_AdaptType_Mw01, AdaptType_Mw01b, d_AdaptType_Mw01b, AdaptType_Mw01b2, d_AdaptType_Mw01b2)


###########
#Word Mw02:

AdaptType_Mw02 <- t.test(Mw02$`pw12_ang/pw34_fea`, Mw02$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw02 <- t_to_d(AdaptType_Mw02$statistic, AdaptType_Mw02$parameter, paired = TRUE)
##################################################
# data:  Mw02$`pw12_ang/pw34_fea` and Mw02$`pw12_fea/pw34_ang`
# t = 1.1005, df = 41, p-value = 0.2775

AdaptType_Mw02b <- t.test(Mw02$Baseline, Mw02$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Mw02b <- t_to_d(AdaptType_Mw02b$statistic, AdaptType_Mw02b$parameter, paired = TRUE)
##################################################
# data:  Mw02$Baseline and Mw02$`pw12_ang/pw34_fea`
# t = -0.61636, df = 41, p-value = 0.5411

AdaptType_Mw02b2 <- t.test(Mw02$Baseline, Mw02$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw02b2 <- t_to_d(AdaptType_Mw02b2$statistic, AdaptType_Mw02b2$parameter, paired = TRUE)
##################################################
# data:  Mw02$Baseline and Mw02$`pw12_fea/pw34_ang`
# t = 0.42559, df = 41, p-value = 0.6726


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Mw02,d_AdaptType_Mw02, 
               AdaptType_Mw02b, d_AdaptType_Mw02b,
               AdaptType_Mw02b2, d_AdaptType_Mw02b2,
               file = "output/Exp3_Resp_post-hoc_Mw02.txt")

#keep environment tidy
rm(AdaptType_Mw02,d_AdaptType_Mw02, AdaptType_Mw02b, d_AdaptType_Mw02b, AdaptType_Mw02b2, d_AdaptType_Mw02b2)

###########
#Word Mw03:

AdaptType_Mw03 <- t.test(Mw03$`pw12_ang/pw34_fea`, Mw03$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw03 <- t_to_d(AdaptType_Mw03$statistic, AdaptType_Mw03$parameter, paired = TRUE)
##################################################
# data:  Mw03$`pw12_ang/pw34_fea` and Mw03$`pw12_fea/pw34_ang`
# t = 1.1032, df = 41, p-value = 0.2764

AdaptType_Mw03b <- t.test(Mw03$Baseline, Mw03$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Mw03b <- t_to_d(AdaptType_Mw03b$statistic, AdaptType_Mw03b$parameter, paired = TRUE)
##################################################
# data:  Mw03$Baseline and Mw03$`pw12_ang/pw34_fea`
# t = -1.2041, df = 41, p-value = 0.2354

AdaptType_Mw03b2 <- t.test(Mw03$Baseline, Mw03$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw03b2 <- t_to_d(AdaptType_Mw03b2$statistic, AdaptType_Mw03b2$parameter, paired = TRUE)
##################################################
# data:  Mw03$Baseline and Mw03$`pw12_fea/pw34_ang`
# t = -0.20874, df = 41, p-value = 0.8357


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Mw03,d_AdaptType_Mw03, 
               AdaptType_Mw03b, d_AdaptType_Mw03b,
               AdaptType_Mw03b2, d_AdaptType_Mw03b2,
               file = "output/Exp3_Resp_post-hoc_Mw03.txt")

#keep environment tidy
rm(AdaptType_Mw03,d_AdaptType_Mw03, AdaptType_Mw03b, d_AdaptType_Mw03b, AdaptType_Mw03b2, d_AdaptType_Mw03b2)


###########
#Word Mw04:

AdaptType_Mw04 <- t.test(Mw04$`pw12_ang/pw34_fea`, Mw04$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw04 <- t_to_d(AdaptType_Mw04$statistic, AdaptType_Mw04$parameter, paired = TRUE)
##################################################
# data:  Mw04$`pw12_ang/pw34_fea` and Mw04$`pw12_fea/pw34_ang`
# t = 1.6813, df = 41, p-value = 0.1003

AdaptType_Mw04b <- t.test(Mw04$Baseline, Mw04$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_Mw04b <- t_to_d(AdaptType_Mw04b$statistic, AdaptType_Mw04b$parameter, paired = TRUE)
##################################################
# data:  Mw04$Baseline and Mw04$`pw12_ang/pw34_fea`
# t = -3.131, df = 41, p-value = 0.003208

AdaptType_Mw04b2 <- t.test(Mw04$Baseline, Mw04$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_Mw04b2 <- t_to_d(AdaptType_Mw04b2$statistic, AdaptType_Mw04b2$parameter, paired = TRUE)
##################################################
# data:  Mw04$Baseline and Mw04$`pw12_fea/pw34_ang`
# t = -1.4056, df = 41, p-value = 0.1674


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType_Mw04,d_AdaptType_Mw04, 
               AdaptType_Mw04b, d_AdaptType_Mw04b,
               AdaptType_Mw04b2, d_AdaptType_Mw04b2,
               file = "output/Exp3_Resp_post-hoc_Mw04.txt")

#keep environment tidy
rm(AdaptType_Mw04,d_AdaptType_Mw04, AdaptType_Mw04b, d_AdaptType_Mw04b, AdaptType_Mw04b2, d_AdaptType_Mw04b2)



##End of Script