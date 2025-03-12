##########################################################################
## File: 03_Exp5_data_analysis.R
## Data Analysis for Exp 5: Adaptation of Emotion - speaker identities, JAVMEPS
# author: Christine Nussbaum 
# date 03/2025

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

load(file ="input/Exp5_without_omissions.RData")



#-------------------------------------------------------------------------------#
#                Analysis 1: Via Cumulative Gaussians   - ToDo                  #
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
#   Analysis 2: Simply the amount of classifications (averaged across tML)      #
#-------------------------------------------------------------------------------#

#aggregate across pseudoword, speaker and tML for analysis
E5_Resp <- mySummary(E5_Adapt, Resp, Participant, SpID, SpSex, AdaptType) #does not contain Baseline

#create separate datasets for female and male voices
E5_Resp_f <- E5_Resp %>% filter(SpSex == "f")
E5_Resp_m <- E5_Resp %>% filter(SpSex == "m")

##########################################################################
## ANOVA IIa: 2 x 2 within subject (female voices only)
# data: E5_Resp_f
# dv: Resp
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=E5_Resp_f, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 43 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept) F(1, 42) = 605.219, p < .001, np2 = .935
# 2      AdaptType F(1, 42) =   2.256, p = .141, np2 = .051
# 3           SpID F(1, 42) =  14.579, p < .001, np2 = .258
# 4 AdaptType:SpID F(1, 42) =   1.089, p = .303, np2 = .025
##############################################################################

capture.output(b, file= "output/Exp5_Resp_ANOVAIIa_females.txt")

# visualize data (just to check)
ezPlot(data=E5_Resp_f, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)



##########################################################################
## ANOVA IIb: 2 x 2 within subject (male voices only)
# data: E5_Resp_m
# dv: Resp
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=E5_Resp_m, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 43 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept) F(1, 42) = 1142.508, p < .001, np2 = .965
# 2      AdaptType F(1, 42) =   14.507, p < .001, np2 = .257
# 3           SpID F(1, 42) =  102.966, p < .001, np2 = .710
# 4 AdaptType:SpID F(1, 42) =    3.221, p = .080, np2 = .071
##############################################################################

capture.output(b, file= "output/Exp5_Resp_ANOVAIIb_males.txt")

# visualize data (just to check)
ezPlot(data=E5_Resp_m, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)


#-------------------------------------------------------------------------------#
#                        Analysis 3: logistic regression                        #
#-------------------------------------------------------------------------------#
## declare tML as numerical variable
E5_Adapt$tML <- as.numeric(as.character(E5_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E5_Adapt$tML_sc <- scale(E5_Adapt$tML)


#create separate datasets for female and male voices
E5_Adapt_f <- E5_Adapt %>% filter(SpSex == "f")
E5_Adapt_m <- E5_Adapt %>% filter(SpSex == "m")



###########################
##  Female voices only

# outcome: Resp
# predictors: ML,  SpSex, AdaptType
# random: Participant

#------------------------------------------------------------------------
# Step1: estimating the model 


# m: assumed fixed effects structure for Participant 
m <- glmer(Resp ~ tML_sc * SpID * AdaptType   + (1 | Participant), data = E5_Adapt_f, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E5_Adapt_f, method = "LRT", family = "binomial") # takes a few minutes
m_test

# Model: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E5_Adapt_f
# Df full model: 9
# Effect df      Chisq p.value
# 1                tML_sc  1 726.63 ***   <.001
# 2                  SpID  1  92.13 ***   <.001
# 3             AdaptType  1    6.66 **    .010
# 4           tML_sc:SpID  1  33.25 ***   <.001
# 5      tML_sc:AdaptType  1       0.87    .352
# 6        SpID:AdaptType  1       2.43    .119
# 7 tML_sc:SpID:AdaptType  1       0.01    .908


# save models as R objects
save(m, m_test, file= "input/E5_GLMs_female.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp5_GLMM_female.txt")


# extract fitted values
E5_Adapt_f$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E5_Adapt_f, fitted, Participant, SpID, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpID, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp5_GLMM_female_descriptive.txt")


###########################
##  Male voices only

# outcome: Resp
# predictors: ML,  SpSex, AdaptType
# random: Participant

#------------------------------------------------------------------------
# Step1: estimating the model 


# m: assumed fixed effects structure for Participant 
m <- glmer(Resp ~ tML_sc * SpID * AdaptType   + (1 | Participant), data = E5_Adapt_m, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E5_Adapt_m, method = "LRT", family = "binomial") # takes a few minutes
m_test

# Model: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E5_Adapt_m
# Df full model: 9
# Effect df      Chisq p.value
# 1                tML_sc  1 877.48 ***   <.001
# 2                  SpID  1 642.83 ***   <.001
# 3             AdaptType  1  53.87 ***   <.001
# 4           tML_sc:SpID  1 197.35 ***   <.001
# 5      tML_sc:AdaptType  1     3.45 +    .063
# 6        SpID:AdaptType  1  11.10 ***   <.001
# 7 tML_sc:SpID:AdaptType  1    7.35 **    .007

summary(m)

###########################################################################
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E5_Adapt_m
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 3932.7   3991.0  -1957.4   3914.7     4784 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -8.6315 -0.4417 -0.1870  0.3995  8.5251 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.6218   0.7885  
# Number of obs: 4793, groups:  Participant, 43
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.36532    0.15003  -9.100  < 2e-16 ***
#   tML_sc                                  1.50382    0.09574  15.707  < 2e-16 ***
#   SpIDnm04                                1.85302    0.12668  14.627  < 2e-16 ***
#   AdaptTypem3_fea/m4_ang                 -0.44887    0.13309  -3.373 0.000744 ***
#   tML_sc:SpIDnm04                         0.92990    0.15556   5.978 2.26e-09 ***
#   tML_sc:AdaptTypem3_fea/m4_ang          -0.09816    0.13822  -0.710 0.477575    
# SpIDnm04:AdaptTypem3_fea/m4_ang        -0.12709    0.17916  -0.709 0.478091    
# tML_sc:SpIDnm04:AdaptTypem3_fea/m4_ang -0.25745    0.21264  -1.211 0.226012  
###########################################################################

# save models as R objects
save(m, m_test, file= "input/E5_GLMs_male.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp5_GLMM_male.txt")


# extract fitted values
E5_Adapt_m$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E5_Adapt_m, fitted, Participant, SpID, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpID, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp5_GLMM_male_descriptive.txt")

##End of Script