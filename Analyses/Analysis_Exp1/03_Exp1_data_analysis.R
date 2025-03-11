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

load(file ="input/Exp1_raw_data.RData")
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
# 1     (Intercept) F(1, 34) = 3064.514, p < .001, np2 = .989
# 2       AdaptType F(1, 34) =    0.049, p = .825, np2 < .011
# 3           SpSex F(1, 34) =    1.361, p = .251, np2 = .038
# 4 AdaptType:SpSex F(1, 34) =  108.110, p < .001, np2 = .761
##############################################################################

capture.output(b, file= "output/Exp1_CG_ANOVAI.txt")

# Daten visualisieren (just f)
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
# t = -5.822, df = 34, p-value = 1.466e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -8.105509 -3.911016
# sample estimates:
#   mean difference 
# -6.008262 


# data:  PH1m$`f_fea/m_ang` and PH1m$`f_ang/m_fea`
# t = 5.7372, df = 34, p-value = 1.89e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   3.639417 7.632014
# sample estimates:
#   mean difference 
# 5.635715
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
# 1     (Intercept) F(1, 39) = 978.239, p < .001, np2 = .962
# 2       AdaptType F(1, 39) =   0.124, p = .726, np2 < .013
# 3           SpSex F(1, 39) =   2.028, p = .162, np2 = .049
# 4 AdaptType:SpSex F(1, 39) =  94.714, p < .001, np2 = .708
##############################################################################

capture.output(b, file= "output/Exp1_Resp_ANOVAII.txt")

# Daten visualisieren (just f)
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
# t = 6.0342, df = 39, p-value = 4.643e-07
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.05312418 0.10669725
# sample estimates:
#   mean difference 
# 0.07991071  

# data:  PH2m$`f_fea/m_ang` and PH2m$`f_ang/m_fea`
# t = -5.0048, df = 39, p-value = 1.234e-05
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.10092321 -0.04282679
# sample estimates:
#   mean difference 
# -0.071875 
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
# 1                 tML_sc  1 4231.47 ***   <.001
# 2                  SpSex  1        0.04    .844
# 3              AdaptType  1        0.09    .761
# 4           tML_sc:SpSex  1   10.86 ***   <.001
# 5       tML_sc:AdaptType  1        2.07    .151
# 6        SpSex:AdaptType  1   88.57 ***   <.001
# 7 tML_sc:SpSex:AdaptType  1        0.86    .355

summary(m)

###########################################################################
## EXPECTED OUTPUT
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpSex * AdaptType + (1 | SpID) + (1 | Participant)
# Data: E1_Adapt
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 7657.3   7728.3  -3818.6   7637.3     8950 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.6252  -0.4577  -0.1656   0.4611  11.1056 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.4133   0.6429  
# SpID        (Intercept) 0.3094   0.5562  
# Number of obs: 8960, groups:  Participant, 40; SpID, 4
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -0.80314    0.41016  -1.958   0.0502 .  
# tML_sc                              1.81776    0.07334  24.784  < 2e-16 ***
#   SpSexm                              0.66547    0.56162   1.185   0.2361    
# AdaptTypef_fea/m_ang                0.57341    0.08263   6.940 3.93e-12 ***
#   tML_sc:SpSexm                       0.31590    0.10750   2.939   0.0033 ** 
#   tML_sc:AdaptTypef_fea/m_ang        -0.03724    0.09960  -0.374   0.7085    
# SpSexm:AdaptTypef_fea/m_ang        -1.11069    0.11903  -9.331  < 2e-16 ***
#   tML_sc:SpSexm:AdaptTypef_fea/m_ang -0.13513    0.14595  -0.926   0.3545
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