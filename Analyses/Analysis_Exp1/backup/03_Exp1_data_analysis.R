##########################################################################
## File: 03_Exp1_data_analysis.R
## Data Analysis for Exp 1: Adaptation of Emotion - male/female voices, Cumulative Gaussian
# author: Christine Nussbaum 
# date 02/2022, revised 03/2025

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library("tidyverse")
library("ez")
library("effectsize")
library(lme4)           # version 1.1-35.1
library(ggeffects)      # version 1.6.0
library(afex)           # version 1.3-1
#library("ggplot2")

# load relevant functions
source("functions/mySummary.R") 
source("functions/tracedEzOut.R")



#---------------------------------------------------------------------------------
#get the CG input data:

load(file ="input/Exp1_without_omissions.RData")
load(file ="input/Exp1_CG_estimates.RData")



#-------------------------------------------------------------------------------#
#                      Analysis 1: Via Cumulative Gaussians                     #
#-------------------------------------------------------------------------------#

#remove Baseline for data analysis
CGdata_Adapt <- CGdata %>% filter(AdaptType != "Baseline")

##########################################################################
## ANOVA I: 2 x 2 within subject
# data: CGdata_Adapt (without Baseline)
# dv: PSE
# wid: Participant
# within: AdaptType, SpSex

a<-ezANOVA(data=CGdata_Adapt, dv=.(PSE), wid=.(Participant), within = .(AdaptType, SpSex), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 35 entered Analysis
##############################################################################
#Output
# 1     (Intercept) F(1, 34) = 2981.959, p < .001, np2 = .989
# 2       AdaptType F(1, 34) =    0.021, p = .887, np2 < .011
# 3           SpSex F(1, 34) =    1.543, p = .223, np2 = .043
# 4 AdaptType:SpSex F(1, 34) =  111.707, p < .001, np2 = .767
##############################################################################

capture.output(b, file= "output/Exp1_CG_ANOVAI.txt")

# visualize data (just to check)
ezPlot(data=CGdata_Adapt, dv=.(PSE), wid=.(Participant), within = .(AdaptType, SpSex), x=SpSex, split=AdaptType)


########################### Post - Hoc Analysis ###########################################################

##########################################################################
### PH1: Interaction of AdaptType und SpSex
##########################################################################


#aggregate Data (in this case not really necessary)
PH1 <- CGdata_Adapt %>% group_by(Participant, SpSex, AdaptType) %>% summarise(PSE = mean(PSE))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, PSE, SpSex, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, PSE)

### t-tests, separately for each SpSex
PH1f<- PH1 %>% filter(SpSex == "f")
AdaptType1 <- t.test(PH1f$`f_fea/m_ang`, PH1f$`f_ang/m_fea`, paired = TRUE)

PH1m<- PH1 %>% filter(SpSex == "m")
AdaptType2 <- t.test(PH1m$`f_fea/m_ang`, PH1m$`f_ang/m_fea`, paired = TRUE)


##################################################
# data:  PH1f$`f_fea/m_ang` and PH1f$`f_ang/m_fea`
# t = -5.8427, df = 34, p-value = 1.379e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -8.158983 -3.947873
# sample estimates:
#   mean difference 
# -6.053428 


# data:  PH1m$`f_fea/m_ang` and PH1m$`f_ang/m_fea`
# t = 5.8506, df = 34, p-value = 1.346e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   3.792349 7.829178
# sample estimates:
#   mean difference 
# 5.810763 
##################################################

#save results
capture.output(as.matrix(PH1_descriptive), AdaptType1, AdaptType2, file = "output/Exp1_CG_post-hoc.txt")

#keep environment tidy
rm(PH1, PH1_descriptive, PH1f, PH1m, AdaptType1, AdaptType2, a, b)

rm(badFit, CG_agg_input, CG_input, CGagg, CGdata, CGdata_Adapt)

#-------------------------------------------------------------------------------#
#   Analysis 2: Simply the amount of classifications (averaged across tML)      #
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

#N= 40 entered Analysis
##############################################################################
# Effect                                     Text
# 1     (Intercept) F(1, 39) = 971.887, p < .001, np2 = .961
# 2       AdaptType F(1, 39) =   0.124, p = .727, np2 < .013
# 3           SpSex F(1, 39) =   2.025, p = .163, np2 = .049
# 4 AdaptType:SpSex F(1, 39) =  97.801, p < .001, np2 = .715
##############################################################################

capture.output(b, file= "output/Exp1_Resp_ANOVAII.txt")

# visualize data (just to check)
ezPlot(data=E1_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpSex), x=SpSex, split=AdaptType)


########################### Post - Hoc Analysis ###########################################################
##########################################################################
### PH1: Interaction of AdaptType und SpSex
##########################################################################

#aggregate Data (in this case not really necessary)
PH2 <- E1_Resp %>% group_by(Participant, SpSex, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH2_descriptive <- mySummary(PH2, Resp, SpSex, AdaptType)

#convert partly into wide format
PH2 <- spread(PH2, AdaptType, Resp)

### t-tests, separately for each SpSex
PH2f<- PH2 %>% filter(SpSex == "f")
AdaptType1 <- t.test(PH2f$`f_fea/m_ang`, PH2f$`f_ang/m_fea`, paired = TRUE)

PH2m<- PH2 %>% filter(SpSex == "m")
AdaptType2 <- t.test(PH2m$`f_fea/m_ang`, PH2m$`f_ang/m_fea`, paired = TRUE)


##################################################
# data:  PH2f$`f_fea/m_ang` and PH2f$`f_ang/m_fea`
# t = 6.1108, df = 39, p-value = 3.634e-07
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.05428967 0.10801252
# sample estimates:
#   mean difference 
# 0.08115109   

# data:  PH2m$`f_fea/m_ang` and PH2m$`f_ang/m_fea`
# t = -5.0404, df = 39, p-value = 1.102e-05
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.10237456 -0.04373941
# sample estimates:
#   mean difference 
# -0.07305698 
##################################################

#save results
capture.output(as.matrix(PH2_descriptive), AdaptType1, AdaptType2, file = "output/Exp1_Resp_post-hoc.txt")

#keep environment tidy
rm(PH2, PH2_descriptive, PH2f, PH2m, AdaptType1, AdaptType2)


#-------------------------------------------------------------------------------#
#                        Analysis 3: logistic regression                        #
#-------------------------------------------------------------------------------#
## declare tML as numerical variable
E1_Adapt$tML <- as.numeric(as.character(E1_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E1_Adapt$tML_sc <- scale(E1_Adapt$tML)

# outcome: Resp
# predictors: ML,  SpSex, AdaptType
# random: SpID, Participant


#------------------------------------------------------------------------
# Step1: estimating the model 


# m: assumed fixed effects structure for SpID and Participant 
m <- glmer(Resp ~ tML_sc * SpSex * AdaptType   + (1 | SpID) + (1 | Participant), data = E1_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E1_Adapt, method = "LRT", family = "binomial") # takes a few minutes
m_test

# Model: Resp ~ tML_sc * SpSex * AdaptType + (1 | SpID) + (1 | Participant)
# Data: E1_Adapt
# Df full model: 10
# Effect df       Chisq p.value
# 1                 tML_sc  1 4247.13 ***   <.001
# 2                  SpSex  1        0.04    .846
# 3              AdaptType  1        0.14    .711
# 4           tML_sc:SpSex  1    10.40 **    .001
# 5       tML_sc:AdaptType  1        1.96    .161
# 6        SpSex:AdaptType  1   91.16 ***   <.001
# 7 tML_sc:SpSex:AdaptType  1        1.39    .239

summary(m)

###########################################################################
## EXPECTED OUTPUT
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpSex * AdaptType + (1 | SpID) + (1 | Participant)
# Data: E1_Adapt
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 7600.4   7671.4  -3790.2   7580.4     8917 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -11.0820  -0.4564  -0.1604   0.4585  10.9862 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.4224   0.6499  
# SpID        (Intercept) 0.3110   0.5577  
# Number of obs: 8927, groups:  Participant, 40; SpID, 4
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -0.79880    0.41141  -1.942  0.05218 .  
# tML_sc                              1.82157    0.07349  24.785  < 2e-16 ***
#   SpSexm                              0.67462    0.56329   1.198  0.23105    
# AdaptTypef_fea/m_ang                0.58730    0.08293   7.082 1.42e-12 ***
#   tML_sc:SpSexm                       0.33102    0.10815   3.061  0.00221 ** 
#   tML_sc:AdaptTypef_fea/m_ang        -0.01635    0.10021  -0.163  0.87037    
# SpSexm:AdaptTypef_fea/m_ang        -1.13049    0.11943  -9.465  < 2e-16 ***
#   tML_sc:SpSexm:AdaptTypef_fea/m_ang -0.17296    0.14683  -1.178  0.23882  
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

##End of Script