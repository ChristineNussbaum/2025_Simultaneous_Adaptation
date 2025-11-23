##########################################################################
## File: 02_Exp1_data_analysis.R
## Data Analysis for Exp 1: Adaptation of Emotion - male/female voices
# author: Christine Nussbaum 
# date 02/2022, revised 03/2025

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


#load the data
load(file ="input/Exp1_without_omissions.RData")

#-------------------------------------------------------------------------------#
#                        Analysis 1: logistic regression                        #
#-------------------------------------------------------------------------------#

## declare tML as numerical variable
E1_Adapt$tML <- as.numeric(as.character(E1_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E1_Adapt$tML_sc <- scale(E1_Adapt$tML)

# outcome: Resp
# predictors: ML,  SpSex, AdaptType
# random: SpID, Participant


#------------------------------------------------------------------------
# Step1: finding a suitable random effects structure

#m: assumed fixed effects structure for SpID and Participant 
m <- glmer(Resp ~ tML_sc * SpSex * AdaptType   + (1 | SpID) + (1 | Participant), data = E1_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked


#m2: assumed fixed effects structure for SpID only
m2 <- glmer(Resp ~ tML_sc * SpSex * AdaptType   + (1 | SpID), data = E1_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#comparing the two models
anova(m, m2) # m is the better modal
rm(m2)

#m3: assumed fixed effects structure for Participant only
m3 <- glmer(Resp ~ tML_sc * SpSex * AdaptType   + (1 | Participant), data = E1_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#comparing the two models
anova(m, m3) # m is the better modal
rm(m3)

#-> we keep the model with fixed effects structure for SpID and Participant 


#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E1_Adapt, method = "LRT", family = "binomial") # takes a few minutes
m_test

# Model: Resp ~ tML_sc * SpSex * AdaptType + (1 | SpID) + (1 | Participant)
# Data: E1_Adapt
# Df full model: 10
# Effect df       Chisq p.value
# 1                 tML_sc  1 4130.01 ***   <.001
# 2                  SpSex  1        0.03    .856
# 3              AdaptType  1        0.38    .536
# 4           tML_sc:SpSex  1     9.02 **    .003
# 5       tML_sc:AdaptType  1        1.72    .190
# 6        SpSex:AdaptType  1   93.59 ***   <.001
# 7 tML_sc:SpSex:AdaptType  1        1.59    .207

summary(m)

###########################################################################
## EXPECTED OUTPUT
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpSex * AdaptType + (1 | SpID) + (1 | Participant)
# Data: E1_Adapt
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC       BIC    logLik -2*log(L)  df.resid 
# 7433.4    7504.1   -3706.7    7413.4      8693 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.9071  -0.4574  -0.1587   0.4593  10.8293 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.4208   0.6487  
# SpID        (Intercept) 0.3007   0.5483  
# Number of obs: 8703, groups:  Participant, 39; SpID, 4
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                        -0.790808   0.405817  -1.949  0.05133 .  
#   tML_sc                              1.814958   0.074220  24.454  < 2e-16 ***
#   SpSexm                              0.678636   0.554600   1.224  0.22108    
#   AdaptTypef_fea/m_ang                0.615444   0.083845   7.340 2.13e-13 ***
#   tML_sc:SpSexm                       0.323329   0.109079   2.964  0.00303 ** 
#   tML_sc:AdaptTypef_fea/m_ang        -0.003524   0.101538  -0.035  0.97231    
#   SpSexm:AdaptTypef_fea/m_ang        -1.156331   0.120616  -9.587  < 2e-16 ***
#   tML_sc:SpSexm:AdaptTypef_fea/m_ang -0.187233   0.148297  -1.263  0.20675    
###########################################################################

# save models as R objects
save(m, m_test, file= "input/E1_GLMs.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp1_GLMM.txt")


# extract fitted values
E1_Adapt$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E1_Adapt, fitted, Participant, SpSex, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpSex, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp1_GLMM_descriptive.txt")

rm(GLM_descriptive, GLM_fit, m, m_test)


#-------------------------------------------------------------------------------#
#   Analysis 2: The amount of classifications (averaged across tML)             #
#-------------------------------------------------------------------------------#

#aggregate across pseudoword, speaker and tML for analysis
E1_Resp <- mySummary(E1_Adapt, Resp, Participant, SpSex, AdaptType) #does not contain Baseline

##########################################################################
## ANOVA II: 2 x 2 within subject
# data: E1_Resp
# dv: Resp
# wid: Participant
# within: AdaptType, SpSex

a<-ezANOVA(data=E1_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpSex), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 39 entered Analysis
##############################################################################
# Effect                                     Text
# 1     (Intercept) F(1, 38) = 956.246, p < .001, np2 = .962
# 2       AdaptType F(1, 38) =   0.287, p = .595, np2 < .017
# 3           SpSex F(1, 38) =   1.650, p = .207, np2 = .042
# 4 AdaptType:SpSex F(1, 38) = 106.858, p < .001, np2 = .738 -> replicates the GLMM
##############################################################################

capture.output(b, file= "output/Exp1_Resp_ANOVA.txt")

# visualize data (just to check)
ezPlot(data=E1_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpSex), x=SpSex, split=AdaptType)


########################################################################################
### Post-Hoc Analysis: Interaction of AdaptType und SpSex, also including the Baseline
########################################################################################


#aggregate Data (in this case not really necessary)
PH1 <- E1_Resp %>% group_by(Participant, SpSex, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, Resp, SpSex, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, Resp)

###separate per speaker sex
PH1f<- PH1 %>% filter(SpSex == "f")
PH1m<- PH1 %>% filter(SpSex == "m")


###now we add the Baseline information 
E1_Bline_Resp <- mySummary(E1_Bline, Resp, Participant, SpSex) 
E1_Bline_Resp <- E1_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E1_Bline_Resp, Baseline, SpSex)


Blinef<- E1_Bline_Resp[,1:3] %>% filter(SpSex == "f")
PH1f <- merge(PH1f, Blinef)

Blinem<- E1_Bline_Resp[,1:3] %>% filter(SpSex == "m")
PH1m <- merge(PH1m, Blinem)

rm(E1_Bline_Resp, Blinef, Blinem)





### t-tests, separately for each SpSex

#Female: 
AdaptTypef <- t.test(PH1f$`f_fea/m_ang`, PH1f$`f_ang/m_fea`, paired = TRUE)
d_AdaptTypef <- t_to_d(AdaptTypef$statistic, AdaptTypef$parameter, paired = TRUE)


##################################################
# data:  PH1f$`f_fea/m_ang` and PH1f$`f_ang/m_fea`
# t = 6.647, df = 38, p-value = 7.415e-08
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.05947509 0.11156745
# sample estimates:
#   mean difference 
# 0.08552127    


AdaptBasef1 <- t.test(PH1f$Baseline, PH1f$`f_ang/m_fea`, paired = TRUE)
d_AdaptBasef1 <- t_to_d(AdaptBasef1$statistic, AdaptBasef1$parameter, paired = TRUE)

##################################################
# data:  PH1f$Baseline and PH1f$`f_ang/m_fea`
# t = 1.0767, df = 38, p-value = 0.2884
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.01213047  0.03969612
# sample estimates:
#   mean difference 
# 0.01378282 

AdaptBasef2 <- t.test(PH1f$Baseline, PH1f$`f_fea/m_ang`, paired = TRUE)
d_AdaptBasef2 <- t_to_d(AdaptBasef2$statistic, AdaptBasef2$parameter, paired = TRUE)

##################################################
# data:  PH1f$Baseline and PH1f$`f_fea/m_ang`
# t = -5.7674, df = 38, p-value = 1.187e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.09691902 -0.04655788
# sample estimates:
#   mean difference 
# -0.07173845 

#Male: 

AdaptTypem <- t.test(PH1m$`f_fea/m_ang`, PH1m$`f_ang/m_fea`, paired = TRUE)
d_AdaptTypem <- t_to_d(AdaptTypem$statistic, AdaptTypem$parameter, paired = TRUE)


##################################################
# Paired t-test
# 
# data:  PH1m$`f_fea/m_ang` and PH1m$`f_ang/m_fea`
# t = -4.9156, df = 38, p-value = 1.728e-05
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.10320314 -0.04299434
# sample estimates:
#   mean difference 
# -0.07309874 
##################################################


AdaptBasem1 <- t.test(PH1m$Baseline, PH1m$`f_ang/m_fea`, paired = TRUE)
d_AdaptBasem1 <- t_to_d(AdaptBasem1$statistic, AdaptBasem1$parameter, paired = TRUE)

##################################################
# data:  PH1m$Baseline and PH1m$`f_ang/m_fea`
# t = -2.5457, df = 38, p-value = 0.01508
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.061413294 -0.007005662
# sample estimates:
#   mean difference 
# -0.03420948

AdaptBasem2 <- t.test(PH1m$Baseline, PH1m$`f_fea/m_ang`, paired = TRUE)
d_AdaptBasem2 <- t_to_d(AdaptBasem2$statistic, AdaptBasem2$parameter, paired = TRUE)

##################################################
# data:  PH1m$Baseline and PH1m$`f_fea/m_ang`
# t = 2.6202, df = 38, p-value = 0.01256
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.008842663 0.068935859
# sample estimates:
#   mean difference 
# 0.03888926



#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptTypef, d_AdaptTypef, AdaptTypem, d_AdaptTypem,
               AdaptBasef1, d_AdaptBasef1,AdaptBasef2,d_AdaptBasef2,
               AdaptBasem1, d_AdaptBasem1,AdaptBasem2,d_AdaptBasem2,
               file = "output/Exp1_Resp_post-hoc.txt")

#keep environment tidy
rm(PH2, PH2_descriptive, PH2f, PH2m, AdaptTypef, d_AdaptTypef, AdaptTypem, d_AdaptTypem,
   AdaptBasef1, d_AdaptBasef1,AdaptBasef2,d_AdaptBasef2,
   AdaptBasem1, d_AdaptBasem1,AdaptBasem2,d_AdaptBasem2)


##End of Script