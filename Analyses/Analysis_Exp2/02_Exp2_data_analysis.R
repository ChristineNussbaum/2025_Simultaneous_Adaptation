##########################################################################
## File: 02_Exp2_data_analysis.R
## Data Analysis for Exp 2: Adaptation of Emotion - speaker identities
# author: Christine Nussbaum 
# date 03/2025, updated 11/2025

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
load(file ="input/Exp2_without_omissions.RData")


#-------------------------------------------------------------------------------#
#                  Analysis 1: logistic regression                              #
#-------------------------------------------------------------------------------#
## declare tML as numerical variable
E2_Adapt$tML <- as.numeric(as.character(E2_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E2_Adapt$tML_sc <- scale(E2_Adapt$tML)


#create separate datasets for female and male voices
E2_Adapt_f <- E2_Adapt %>% filter(SpSex == "f")
E2_Adapt_m <- E2_Adapt %>% filter(SpSex == "m")

#-------------------------------------------------------------------------------#
#                         1a:  female voices  only                              #
#-------------------------------------------------------------------------------#
# outcome: Resp
# predictors: ML,  SpID, AdaptType
# random: Participant

#------------------------------------------------------------------------
# Step1: estimating the model 

# m: assumed fixed effects structure for Participant 
m <- glmer(Resp ~ tML_sc * SpID * AdaptType   + (1 | Participant), data = E2_Adapt_f, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked


#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E2_Adapt_f, method = "LRT", family = "binomial") # takes a few minutes
m_test

# Model: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E2_Adapt_f
# Df full model: 9
# Effect df       Chisq p.value
# 1                tML_sc  1 1778.33 ***   <.001
# 2                  SpID  1   42.09 ***   <.001
# 3             AdaptType  1     6.94 **    .008
# 4           tML_sc:SpID  1   31.69 ***   <.001
# 5      tML_sc:AdaptType  1      5.82 *    .016
# 6        SpID:AdaptType  1      6.49 *    .011
# 7 tML_sc:SpID:AdaptType  1        0.35    .552

summary(m)

###########################################################################
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E2_Adapt_f
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC       BIC    logLik -2*log(L)  df.resid 
# 4503.1    4561.2   -2242.5    4485.1      4695 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.5868 -0.5360 -0.2278  0.5503  6.1681 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.3956   0.629   
# Number of obs: 4704, groups:  Participant, 42
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.186459   0.123341  -1.512 0.130603    
# tML_sc                                  1.721655   0.094290  18.259  < 2e-16 ***
#   SpIDf2                               -0.301081   0.104462  -2.882 0.003949 ** 
#   AdaptTypef1_fea/f2_ang               -0.006525   0.110554  -0.059 0.952937    
# tML_sc:SpIDf2                          -0.456494   0.122466  -3.728 0.000193 ***
#   tML_sc:AdaptTypef1_fea/f2_ang         0.274603   0.139819   1.964 0.049532 *  
#   SpIDf2:AdaptTypef1_fea/f2_ang        -0.390592   0.153449  -2.545 0.010915 *  
#   tML_sc:SpIDf2:AdaptTypef1_fea/f2_ang -0.108917   0.183084  -0.595 0.551909    
###########################################################################

# save models as R objects
save(m, m_test, file= "input/E2_GLMs_female.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp2_GLMM_female.txt")

# extract fitted values
E2_Adapt_f$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E2_Adapt_f, fitted, Participant, SpID, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpID, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp2_GLMM_female_descriptive.txt")


#-------------------------------------------------------------------------------#
#                          1b:  male voices  only                               #
#-------------------------------------------------------------------------------#

# outcome: Resp
# predictors: ML,  SpID, AdaptType
# random: Participant

#------------------------------------------------------------------------
# Step1: estimating the model 


# m: assumed fixed effects structure for Participant 
m <- glmer(Resp ~ tML_sc * SpID * AdaptType   + (1 | Participant), data = E2_Adapt_m, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E2_Adapt_m, method = "LRT", family = "binomial") # takes a few minutes
m_test

# Model: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E2_Adapt_m
# Df full model: 9
# Effect df       Chisq p.value
# 1                tML_sc  1 1846.07 ***   <.001
# 2                  SpID  1  433.77 ***   <.001
# 3             AdaptType  1   36.31 ***   <.001
# 4           tML_sc:SpID  1   54.60 ***   <.001
# 5      tML_sc:AdaptType  1      2.88 +    .090
# 6        SpID:AdaptType  1        0.66    .415
# 7 tML_sc:SpID:AdaptType  1        0.93    .336

summary(m)

###########################################################################
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E2_Adapt_m
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC       BIC    logLik -2*log(L)  df.resid 
# 3855.1    3913.2   -1918.5    3837.1      4695 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -8.4188 -0.4445 -0.1857  0.3952  8.7715 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.6335   0.7959  
# Number of obs: 4704, groups:  Participant, 42
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.37954    0.15272  -9.033  < 2e-16 ***
#   tML_sc                                1.49458    0.09657  15.477  < 2e-16 ***
#   SpIDm4                                1.86560    0.12761  14.620  < 2e-16 ***
#   AdaptTypem3_fea/m4_ang               -0.46880    0.13548  -3.460  0.00054 ***
#   tML_sc:SpIDm4                         0.89303    0.15518   5.755 8.67e-09 ***
#   tML_sc:AdaptTypem3_fea/m4_ang        -0.07894    0.14018  -0.563  0.57335    
# SpIDm4:AdaptTypem3_fea/m4_ang          -0.14815    0.18148  -0.816  0.41430    
# tML_sc:SpIDm4:AdaptTypem3_fea/m4_ang   -0.20594    0.21435  -0.961  0.33669     
###########################################################################

# save models as R objects
save(m, m_test, file= "input/E2_GLMs_male.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp2_GLMM_male.txt")


# extract fitted values
E2_Adapt_m$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E2_Adapt_m, fitted, Participant, SpID, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpID, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp2_GLMM_male_descriptive.txt")


#-------------------------------------------------------------------------------#
#   Analysis 2: The amount of classifications (averaged across tML)             #
#-------------------------------------------------------------------------------#

#aggregate across pseudoword, speaker and tML for analysis
E2_Resp_f <- mySummary(E2_Adapt_f, Resp, Participant, SpID, SpSex, AdaptType) #does not contain Baseline
E2_Resp_m <- mySummary(E2_Adapt_m, Resp, Participant, SpID, SpSex, AdaptType) #does not contain Baseline


#-------------------------------------------------------------------------------#
#                         2a:  female voices  only                              #
#-------------------------------------------------------------------------------#

##########################################################################
## ANOVA IIa: 2 x 2 within subject (female voices only)
# data: E2_Resp_f
# dv: Resp
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=E2_Resp_f, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 42 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept) F(1, 41) = 742.328, p < .001, np2 = .948
# 2      AdaptType F(1, 41) =   3.817, p = .058, np2 = .085
# 3           SpID F(1, 41) =  16.213, p < .001, np2 = .283
# 4 AdaptType:SpID F(1, 41) =  10.912, p = .002, np2 = .210
##############################################################################

capture.output(b, file= "output/Exp2_Resp_ANOVAIIa_females.txt")

# visualize data (just to check)
ezPlot(data=E2_Resp_f, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)


########################### Post - Hoc Analysis ###########################################################

########################################################################################
### Post-Hoc Analysis: Interaction of AdaptType und SpID, also including the Baseline
########################################################################################

#aggregate Data (in this case not really necessary)
PH1 <- E2_Resp_f %>% group_by(Participant, SpID, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, Resp, SpID, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, Resp)

###separate per speaker sex
PH1f1<- PH1 %>% filter(SpID == "f1")
PH1f2<- PH1 %>% filter(SpID == "f2")


###now we add the Baseline information 
E2_Bline_Resp <- mySummary(E2_Bline[E2_Bline$SpSex == "f",], Resp, Participant, SpID) 
E2_Bline_Resp <- E2_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E2_Bline_Resp, Baseline, SpID)


Blinef1<- E2_Bline_Resp[,1:3] %>% filter(SpID == "f1")
PH1f1 <- merge(PH1f1, Blinef1)

Blinef2<- E2_Bline_Resp[,1:3] %>% filter(SpID == "f2")
PH1f2 <- merge(PH1f2, Blinef2)

rm(E1_Bline_Resp, Blinef, Blinem)


### t-tests, separately for each SpID

names(PH1f1)

#F1: 
AdaptTypef1 <- t.test(PH1f1$`f1_ang/f2_fea`, PH1f1$`f1_fea/f2_ang`, paired = TRUE)
d_AdaptTypef1 <- t_to_d(AdaptTypef1$statistic, AdaptTypef1$parameter, paired = TRUE)


##################################################
# data:  PH1f1$`f1_ang/f2_fea` and PH1f1$`f1_fea/f2_ang`
# t = -0.094429, df = 41, p-value = 0.9252
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.03807274  0.03467138
# sample estimates:
#   mean difference 
# -0.00170068  


AdaptBasef1 <- t.test(PH1f1$Baseline, PH1f1$`f1_ang/f2_fea`, paired = TRUE)
d_AdaptBasef1 <- t_to_d(AdaptBasef1$statistic, AdaptBasef1$parameter, paired = TRUE)

##################################################
# data:  PH1f1$Baseline and PH1f1$`f1_ang/f2_fea`
# t = 0.78111, df = 41, p-value = 0.4392
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.02464754  0.05573946
# sample estimates:
#   mean difference 
# 0.01554596 

AdaptBasef1b <- t.test(PH1f1$Baseline, PH1f1$`f1_fea/f2_ang`, paired = TRUE)
d_AdaptBasef1b <- t_to_d(AdaptBasef1b$statistic, AdaptBasef1b$parameter, paired = TRUE)

##################################################
# data:  PH1f1$Baseline and PH1f1$`f1_fea/f2_ang`
# t = 0.81116, df = 41, p-value = 0.422
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.02062546  0.04831602
# sample estimates:
#   mean difference 
# 0.01384528

#F2: 

AdaptTypef2 <- t.test(PH1f2$`f1_ang/f2_fea`, PH1f2$`f1_fea/f2_ang`, paired = TRUE)
d_AdaptTypef2 <- t_to_d(AdaptTypef2$statistic, AdaptTypef2$parameter, paired = TRUE)


##################################################
# data:  PH1f2$`f1_ang/f2_fea` and PH1f2$`f1_fea/f2_ang`
# t = 3.5849, df = 41, p-value = 0.0008885
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.02561993 0.09172701
# sample estimates:
#   mean difference 
# 0.05867347 
##################################################


AdaptBasef2 <- t.test(PH1f2$Baseline, PH1f2$`f1_ang/f2_fea`, paired = TRUE)
d_AdaptBasef2 <- t_to_d(AdaptBasef2$statistic, AdaptBasef2$parameter, paired = TRUE)

##################################################
# data:  PH1f2$Baseline and PH1f2$`f1_ang/f2_fea`
# t = -2.4445, df = 41, p-value = 0.01889
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.08904428 -0.00847592
# sample estimates:
#   mean difference 
# -0.0487601 

AdaptBasef2b <- t.test(PH1f2$Baseline, PH1f2$`f1_fea/f2_ang`, paired = TRUE)
d_AdaptBasef2b <- t_to_d(AdaptBasef2b$statistic, AdaptBasef2b$parameter, paired = TRUE)

##################################################
# data:  PH1f2$Baseline and PH1f2$`f1_fea/f2_ang`
# t = 0.52013, df = 41, p-value = 0.6058
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.02857767  0.04840440
# sample estimates:
#   mean difference 
# 0.009913367



#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptTypef1, d_AdaptTypef1, AdaptTypef2, d_AdaptTypef2,
               AdaptBasef1, d_AdaptBasef1,AdaptBasef1b,d_AdaptBasef1b,
               AdaptBasef2, d_AdaptBasef2,AdaptBasef2b,d_AdaptBasef2b,
               file = "output/Exp2_Resp_post-hoc_females.txt")

#keep environment tidy
rm(PH1, PH1_descriptive, PH1f1, PH1f2, AdaptTypef1, d_AdaptTypef1, AdaptTypef2, d_AdaptTypef2,
   AdaptBasef1, d_AdaptBasef1,AdaptBasef1b,d_AdaptBasef1b,
   AdaptBasef2, d_AdaptBasef2,AdaptBasef2b,d_AdaptBasef2b)



#-------------------------------------------------------------------------------#
#                         2b:  male voices  only                              #
#-------------------------------------------------------------------------------#

##########################################################################
## ANOVA IIa: 2 x 2 within subject (female voices only)
# data: E2_Resp_m
# dv: Resp
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=E2_Resp_m, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 42 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept) F(1, 41) = 596.077, p < .001, np2 = .936
# 2      AdaptType F(1, 41) =  22.432, p < .001, np2 = .354
# 3           SpID F(1, 41) = 249.007, p < .001, np2 = .859
# 4 AdaptType:SpID F(1, 41) =   0.064, p = .801, np2 < .012
##############################################################################

capture.output(b, file= "output/Exp2_Resp_ANOVAIIb_males.txt")

# visualize data (just to check)
ezPlot(data=E2_Resp_m, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)


########################### Post - Hoc Analysis ###########################################################

########################################################################################
### Post-Hoc Analysis: Interaction of AdaptType und SpID, also including the Baseline
########################################################################################

#aggregate Data (in this case not really necessary)
PH2 <- E2_Resp_m %>% group_by(Participant, SpID, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH2_descriptive <- mySummary(PH2, Resp, SpID, AdaptType)

#convert partly into wide format
PH2 <- spread(PH2, AdaptType, Resp)

###separate per speaker sex
PH2m3<- PH2 %>% filter(SpID == "m3")
PH2m4<- PH2 %>% filter(SpID == "m4")


###now we add the Baseline information 
E2_Bline_Resp <- mySummary(E2_Bline[E2_Bline$SpSex == "m",], Resp, Participant, SpID) 
E2_Bline_Resp <- E2_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E2_Bline_Resp, Baseline, SpID)


Blinem3<- E2_Bline_Resp[,1:3] %>% filter(SpID == "m3")
PH2m3 <- merge(PH2m3, Blinem3)

Blinem4<- E2_Bline_Resp[,1:3] %>% filter(SpID == "m4")
PH2m4 <- merge(PH2m4, Blinem4)

rm(E2_Bline_Resp, Blinem3, Blinem4)


### t-tests, separately for each SpID



#m3: 
AdaptTypem3 <- t.test(PH2m3$`m3_ang/m4_fea`, PH2m3$`m3_fea/m4_ang`, paired = TRUE)
d_AdaptTypem3 <- t_to_d(AdaptTypem3$statistic, AdaptTypem3$parameter, paired = TRUE)


##################################################
# data:  PH2m3$`m3_ang/m4_fea` and PH2m3$`m3_fea/m4_ang`
# t = 3.7958, df = 41, p-value = 0.0004775
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.03183357 0.10422085
# sample estimates:
#   mean difference 
# 0.06802721 


AdaptBasem3 <- t.test(PH2m3$Baseline, PH2m3$`m3_ang/m4_fea`, paired = TRUE)
d_AdaptBasem3 <- t_to_d(AdaptBasem3$statistic, AdaptBasem3$parameter, paired = TRUE)

##################################################
# data:  PH2m3$Baseline and PH2m3$`m3_ang/m4_fea`
# t = -0.75313, df = 41, p-value = 0.4557
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.04668175  0.02132175
# sample estimates:
#   mean difference 
# -0.01268 

AdaptBasem3b <- t.test(PH2m3$Baseline, PH2m3$`m3_fea/m4_ang`, paired = TRUE)
d_AdaptBasem3b <- t_to_d(AdaptBasem3b$statistic, AdaptBasem3b$parameter, paired = TRUE)

##################################################
# t = 2.9361, df = 41, p-value = 0.005428
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.01727776 0.09341666
# sample estimates:
#   mean difference 
# 0.05534721 

#m4: 

AdaptTypem4 <- t.test(PH2m4$`m3_ang/m4_fea`, PH2m4$`m3_fea/m4_ang`, paired = TRUE)
d_AdaptTypem4 <- t_to_d(AdaptTypem4$statistic, AdaptTypem4$parameter, paired = TRUE)


##################################################
# data:  PH2m4$`m3_ang/m4_fea` and PH2m4$`m3_fea/m4_ang`
# t = 4.4956, df = 41, p-value = 5.586e-05
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.03980938 0.10474844
# sample estimates:
#   mean difference 
# 0.07227891 
##################################################


AdaptBasem4 <- t.test(PH2m4$Baseline, PH2m4$`m3_ang/m4_fea`, paired = TRUE)
d_AdaptBasem4 <- t_to_d(AdaptBasem4$statistic, AdaptBasem4$parameter, paired = TRUE)

##################################################
# t = -0.035393, df = 41, p-value = 0.9719
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.03150743  0.03042210
# sample estimates:
#   mean difference 
# -0.0005426672 

AdaptBasem4b <- t.test(PH2m4$Baseline, PH2m4$`m3_fea/m4_ang`, paired = TRUE)
d_AdaptBasem4b <- t_to_d(AdaptBasem4b$statistic, AdaptBasem4b$parameter, paired = TRUE)

##################################################
# data:  PH2m4$Baseline and PH2m4$`m3_fea/m4_ang`
# t = 3.6962, df = 41, p-value = 0.0006414
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.03254055 0.11093194
# sample estimates:
#   mean difference 
# 0.07173624



#save results
capture.output(as.matrix(PH2_descriptive), as.matrix(Baseline_descriptive),
               AdaptTypem3, d_AdaptTypem3, AdaptTypem4, d_AdaptTypem4,
               AdaptBasem3, d_AdaptBasem3,AdaptBasem3b,d_AdaptBasem3b,
               AdaptBasem4, d_AdaptBasem4,AdaptBasem4b,d_AdaptBasem4b,
               file = "output/Exp2_Resp_post-hoc_males.txt")

#keep environment tidy
rm(PH2, PH2_descriptive, PH2m3, PH2m4, AdaptTypem3, d_AdaptTypem3, AdaptTypem4, d_AdaptTypem4,
   AdaptBasem3, d_AdaptBasem3,AdaptBasem3b,d_AdaptBasem3b,
   AdaptBasem4, d_AdaptBasem4,AdaptBasem4b,d_AdaptBasem4b)




##End of Script