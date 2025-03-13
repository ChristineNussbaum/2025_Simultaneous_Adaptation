##########################################################################
## File: 03_Exp3_data_analysis.R
## Data Analysis for Exp 3: Adaptation of Emotion - pseudowords
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

load(file ="input/Exp3_without_omissions.RData")
load(file ="input/Exp3_CG_estimates.RData")

#remove that we dont need for the analysis
rm(badFit, CG_agg_input, CG_input, CG_agg, CGagg) # we just need CGdata

#-------------------------------------------------------------------------------#
#                      Analysis 1: Via Cumulative Gaussians                     #
#-------------------------------------------------------------------------------#

#remove Baseline for data analysis
CGdata_Adapt <- CGdata %>% filter(AdaptType != "Baseline")



##########################################################################
## ANOVA I: 2 x 2 x 2  within subject 
# data: CGdata_Adapt (without Baseline)
# dv: PSE
# wid: Participant
# within: AdaptType, PW_Group, SpSex

a<-ezANOVA(data=CGdata_Adapt, dv=.(PSE), wid=.(Participant), within = .(AdaptType, PW_Group, SpSex), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 41 entered Analysis
##############################################################################
#Output                                     Text
# 1              (Intercept) F(1,40) = 2924.298, p < .001, np2 = .987
# 2                AdaptType F(1,40) =    3.428, p = .071, np2 = .079
# 3                 PW_Group F(1,40) =   11.237, p = .002, np2 = .219
# 4                    SpSex F(1,40) =    0.005, p = .946, np2 < .010
# 5       AdaptType:PW_Group F(1,40) =    3.156, p = .083, np2 = .073
# 6          AdaptType:SpSex F(1,40) =    2.761, p = .104, np2 = .065
# 7           PW_Group:SpSex F(1,40) =   26.383, p < .001, np2 = .397
# 8 AdaptType:PW_Group:SpSex F(1,40) =    0.021, p = .886, np2 < .011
##############################################################################

capture.output(b, file= "output/Exp3_CG_ANOVAI.txt")

# visualize data (just to check)
ezPlot(data=CGdata_Adapt, dv=.(PSE), wid=.(Participant), within = .(AdaptType, PW_Group), x=PW_Group, split=AdaptType)

#this suggests an effect but I am very suspicious!
rm(a,b, CG_data, CG_data_Adapt)

#-------------------------------------------------------------------------------#
#   Analysis 2: Simply the amount of classifications (averaged across tML)      #
#-------------------------------------------------------------------------------#

#aggregate across pseudoword, speaker and tML for analysis
E3_Resp <- mySummary(E3_Adapt, Resp, Participant, Word, SpSex, AdaptType) #does not contain Baseline



##########################################################################
## ANOVA IIa: 2 x 2 x 4 within subject
# data: E3_Resp
# dv: Resp
# wid: Participant
# within: AdaptType, Word, SpSex

a<-ezANOVA(data=E3_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpSex, Word), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 47 entered Analysis
##############################################################################
# Effect                                     Text
# 1          (Intercept)  F(1,46) = 1113.151, p < .001, np2 = .960
# 2            AdaptType  F(1,46) =    3.869, p = .055, np2 = .078
# 3                SpSex  F(1,46) =    1.299, p = .260, np2 = .027
# 4                 Word F(3,138) =  118.685, p < .001, np2 = .721
# 5      AdaptType:SpSex  F(1,46) =    1.683, p = .201, np2 = .035
# 6       AdaptType:Word F(3,138) =    0.380, p = .768, np2 < .018
# 7           SpSex:Word F(3,138) =   15.412, p < .001, np2 = .251
# 8 AdaptType:SpSex:Word F(3,138) =    0.268, p = .849, np2 < .016
##############################################################################

capture.output(b, file= "output/Exp3_Resp_ANOVAII.txt")

# visualize data (just to check)
ezPlot(data=E3_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), split=Word, x=AdaptType)



#-------------------------------------------------------------------------------#
#                        Analysis 3: logistic regression                        #
#-------------------------------------------------------------------------------#
## declare tML as numerical variable
E3_Adapt$tML <- as.numeric(as.character(E3_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E3_Adapt$tML_sc <- scale(E3_Adapt$tML)



#------------------------------------------------------------------------
# Step1: estimating the model 


# m: assumed fixed effects structure for Participant 
m <- glmer(Resp ~ tML_sc * Word * AdaptType * SpSex + (1 | SpID)   + (1 | Participant), data = E3_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked

#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E3_Adapt, method = "LRT", family = "binomial") # takes a few minutes
m_test

Mixed Model Anova Table (Type 3 tests, LRT-method)

# Model: Resp ~ tML_sc * Word * AdaptType * SpSex + (1 | SpID) + (1 | 
#                                                                   Model:     Participant)
# Data: E3_Adapt
# Df full model: 34
# Effect df       Chisq p.value
# 1                       tML_sc  1 4489.67 ***   <.001
# 2                         Word  3  482.58 ***   <.001
# 3                    AdaptType  1      6.61 *    .010
# 4                        SpSex  1        0.03    .857
# 5                  tML_sc:Word  3   44.12 ***   <.001
# 6             tML_sc:AdaptType  1        0.86    .353
# 7               Word:AdaptType  3        0.65    .885
# 8                 tML_sc:SpSex  1     6.75 **    .009
# 9                   Word:SpSex  3   58.49 ***   <.001
# 10             AdaptType:SpSex  1        0.42    .516
# 11       tML_sc:Word:AdaptType  3     10.81 *    .013
# 12           tML_sc:Word:SpSex  3   46.97 ***   <.001
# 13      tML_sc:AdaptType:SpSex  1        2.66    .103
# 14        Word:AdaptType:SpSex  3        2.62    .454
# 15 tML_sc:Word:AdaptType:SpSex  3        6.23    .101


# save models as R objects
save(m, m_test, file= "input/E3_GLMs.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp3_GLMM.txt")


# extract fitted values
E3_Adapt$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E3_Adapt, fitted, Participant, Word, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, Word, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp3_GLMM_female_descriptive.txt")

##End of Script