##########################################################################
## File: 03_Exp2_data_analysis.R
## Data Analysis for Exp 2: Adaptation of Emotion - speaker identities
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

load(file ="input/Exp2_without_omissions.RData")
load(file ="input/Exp2_CG_estimates.RData")

#remove that we dont need for the analysis
rm(badFit, CG_agg_input, CG_input, CG_agg) # we just need CG_data

#-------------------------------------------------------------------------------#
#                      Analysis 1: Via Cumulative Gaussians                     #
#-------------------------------------------------------------------------------#

#remove Baseline for data analysis
CGdata_Adapt <- CGdata %>% filter(AdaptType != "Baseline")

#create separate datasets for female and male voices
CGdata_Adapt_f <- CGdata_Adapt %>% filter(SpSex == "f")
CGdata_Adapt_m <- CGdata_Adapt %>% filter(SpSex == "m")

##########################################################################
## ANOVA Ia: 2 x 2  within subject - female voices only 
# data: CGdata_Adapt_f (without Baseline)
# dv: PSE
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=CGdata_Adapt_f, dv=.(PSE), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 28 entered Analysis
##############################################################################
#Output                                     Text
# 1    (Intercept) F(1, 27) = 1744.715, p < .001, np2 = .985
# 2      AdaptType F(1, 27) =    2.127, p = .156, np2 = .073
# 3           SpID F(1, 27) =    9.771, p = .004, np2 = .266
# 4 AdaptType:SpID F(1, 27) =    9.736, p = .004, np2 = .265  
##############################################################################

capture.output(b, file= "output/Exp2_CG_ANOVAIa_females.txt")

# visualize data (just to check)
ezPlot(data=CGdata_Adapt_f, dv=.(PSE), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)


########################### Post - Hoc Analysis ###########################################################

##########################################################################
### PH1: Interaction of AdaptType und SpID
##########################################################################


#aggregate Data (in this case not really necessary)
PH1 <- CGdata_Adapt_f %>% group_by(Participant, SpID, AdaptType) %>% summarise(PSE = mean(PSE))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, PSE, SpID, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, PSE)

### t-tests, separately for each SpID
PH1_nf01<- PH1 %>% filter(SpID == "nf01")
AdaptType1 <- t.test(PH1_nf01$`f1_fea/f2_ang`, PH1_nf01$`f1_ang/f2_fea`, paired = TRUE)

PH1_nf03<- PH1 %>% filter(SpID == "nf03")
AdaptType2 <- t.test(PH1_nf03$`f1_fea/f2_ang`, PH1_nf03$`f1_ang/f2_fea`, paired = TRUE)


##################################################
# data:  PH1_nf01$`f1_fea/f2_ang` and PH1_nf01$`f1_ang/f2_fea`
# t = -0.55137, df = 27, p-value = 0.5859
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -4.989198  2.875719
# sample estimates:
#   mean difference 
# -1.056739 


# data:  PH1_nf03$`f1_fea/f2_ang` and PH1_nf03$`f1_ang/f2_fea`
# t = 2.8033, df = 27, p-value = 0.009252
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   1.599962 10.337571
# sample estimates:
#   mean difference 
# 5.968766  
##################################################

#save results
capture.output(as.matrix(PH1_descriptive), AdaptType1, AdaptType2, file = "output/Exp2_CG_female_post-hoc.txt")

#keep environment tidy
rm(PH1, PH1_descriptive, PH1_nf01, PH1_nf03, AdaptType1, AdaptType2, a, b)


##########################################################################
## ANOVA Ib: 2 x 2  within subject - male voices only 
# data: CGdata_Adapt_m (without Baseline)
# dv: PSE
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=CGdata_Adapt_m, dv=.(PSE), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 28 entered Analysis
##############################################################################
#Output                                     Text
# 1    (Intercept) F(1, 27) = 1923.638, p < .001, np2 = .986
# 2      AdaptType F(1, 27) =   16.709, p < .001, np2 = .382
# 3           SpID F(1, 27) =  203.128, p < .001, np2 = .883
# 4 AdaptType:SpID F(1, 27) =    0.464, p = .502, np2 = .017
##############################################################################

capture.output(b, file= "output/Exp2_CG_ANOVAIb_males.txt")

# visualize data (just to check)
ezPlot(data=CGdata_Adapt_m, dv=.(PSE), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)



#keep environment tidy
rm(a, b, CGagg, CGdata, CGdata_Adapt, CGdata_Adapt_m, CGdata_Adapt_f)


#-------------------------------------------------------------------------------#
#   Analysis 2: Simply the amount of classifications (averaged across tML)      #
#-------------------------------------------------------------------------------#

#aggregate across pseudoword, speaker and tML for analysis
E2_Resp <- mySummary(E2_Adapt, Resp, Participant, SpID, SpSex, AdaptType) #does not contain Baseline

#create separate datasets for female and male voices
E2_Resp_f <- E2_Resp %>% filter(SpSex == "f")
E2_Resp_m <- E2_Resp %>% filter(SpSex == "m")

##########################################################################
## ANOVA IIa: 2 x 2 within subject (female voices only)
# data: E2_Resp_f
# dv: Resp
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=E2_Resp_f, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 43 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept) F(1, 42) = 763.713, p < .001, np2 = .948
# 2      AdaptType F(1, 42) =   5.073, p = .030, np2 = .108
# 3           SpID F(1, 42) =  16.532, p < .001, np2 = .282
# 4 AdaptType:SpID F(1, 42) =  13.523, p = .001, np2 = .244
##############################################################################

capture.output(b, file= "output/Exp2_Resp_ANOVAIIa_females.txt")

# visualize data (just to check)
ezPlot(data=E2_Resp_f, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)


########################### Post - Hoc Analysis ###########################################################

##########################################################################
### PH2: Interaction of AdaptType und SpID
##########################################################################

#aggregate Data (in this case not really necessary)
PH2 <- E2_Resp_f %>% group_by(Participant, SpID, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH2_descriptive <- mySummary(PH2, Resp, SpID, AdaptType)

#convert partly into wide format
PH2 <- spread(PH2, AdaptType, Resp)

### t-tests, separately for each SpSex
PH2_nf01<- PH2 %>% filter(SpID == "nf01")
AdaptType1 <- t.test(PH2_nf01$`f1_fea/f2_ang`, PH2_nf01$`f1_ang/f2_fea`, paired = TRUE)

PH2_nf03<- PH2 %>% filter(SpID == "nf03")
AdaptType2 <- t.test(PH2_nf03$`f1_fea/f2_ang`, PH2_nf03$`f1_ang/f2_fea`, paired = TRUE)


##################################################
# data:  PH2_nf01$`f1_fea/f2_ang` and PH2_nf01$`f1_ang/f2_fea`
# t = -0.0040768, df = 42, p-value = 0.9968
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.03497698  0.03483595
# sample estimates:
#   mean difference 
# -7.051519e-05    

# data:  PH2_nf03$`f1_fea/f2_ang` and PH2_nf03$`f1_ang/f2_fea`
# t = -3.9765, df = 42, p-value = 0.0002702
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.09688836 -0.03165353
# sample estimates:
#   mean difference 
# -0.06427095 
##################################################

#save results
capture.output(as.matrix(PH2_descriptive), AdaptType1, AdaptType2, file = "output/Exp2_Resp_female_post-hoc.txt")

#keep environment tidy
rm(PH2, PH2_descriptive, PH2_nf01, PH2_nf03, AdaptType1, AdaptType2)


##########################################################################
## ANOVA IIb: 2 x 2 within subject (male voices only)
# data: E2_Resp_m
# dv: Resp
# wid: Participant
# within: AdaptType, SpID

a<-ezANOVA(data=E2_Resp_m, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#N= 43 entered Analysis
##############################################################################
# Effect                                     Text
# 1    (Intercept) F(1, 42) = 630.593, p < .001, np2 = .938
# 2      AdaptType F(1, 42) =  21.353, p < .001, np2 = .337
# 3           SpID F(1, 42) = 249.642, p < .001, np2 = .856
# 4 AdaptType:SpID F(1, 42) =   0.004, p = .949, np2 < .010
##############################################################################

capture.output(b, file= "output/Exp2_Resp_ANOVAIIb_males.txt")

# visualize data (just to check)
ezPlot(data=E2_Resp_m, dv=.(Resp), wid=.(Participant), within = .(AdaptType, SpID), x=SpID, split=AdaptType)


#-------------------------------------------------------------------------------#
#                        Analysis 3: logistic regression                        #
#-------------------------------------------------------------------------------#
## declare tML as numerical variable
E2_Adapt$tML <- as.numeric(as.character(E2_Adapt$tML))

#scale tML, because this is recommended for mixed-effects modelling
E2_Adapt$tML_sc <- scale(E2_Adapt$tML)


#create separate datasets for female and male voices
E2_Adapt_f <- E2_Adapt %>% filter(SpSex == "f")
E2_Adapt_m <- E2_Adapt %>% filter(SpSex == "m")



###########################
##  Female voices only

# outcome: Resp
# predictors: ML,  SpSex, AdaptType
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
# 1                tML_sc  1 1804.56 ***   <.001
# 2                  SpID  1   43.41 ***   <.001
# 3             AdaptType  1     7.82 **    .005
# 4           tML_sc:SpID  1   32.75 ***   <.001
# 5      tML_sc:AdaptType  1      4.45 *    .035
# 6        SpID:AdaptType  1     7.02 **    .008
# 7 tML_sc:SpID:AdaptType  1        0.54    .461

summary(m)

###########################################################################
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E2_Adapt_f
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 4594.9   4653.1  -2288.4   4576.9     4780 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -6.0638 -0.5401 -0.2301  0.5493  5.7843 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.3928   0.6267  
# Number of obs: 4789, groups:  Participant, 43
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.16755    0.12176  -1.376 0.168789    
# tML_sc                                  1.72622    0.09371  18.421  < 2e-16 ***
#   SpIDnf03                               -0.29745    0.10360  -2.871 0.004090 ** 
#   AdaptTypef1_fea/f2_ang                 -0.01097    0.10945  -0.100 0.920178    
# tML_sc:SpIDnf03                        -0.44647    0.12184  -3.664 0.000248 ***
#   tML_sc:AdaptTypef1_fea/f2_ang           0.25704    0.13835   1.858 0.063179 .  
# SpIDnf03:AdaptTypef1_fea/f2_ang        -0.40166    0.15170  -2.648 0.008103 ** 
#   tML_sc:SpIDnf03:AdaptTypef1_fea/f2_ang -0.13340    0.18094  -0.737 0.460943    
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


###########################
##  Male voices only

# outcome: Resp
# predictors: ML,  SpSex, AdaptType
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
# 1                tML_sc  1 1896.62 ***   <.001
# 2                  SpID  1  443.83 ***   <.001
# 3             AdaptType  1   33.12 ***   <.001
# 4           tML_sc:SpID  1   57.20 ***   <.001
# 5      tML_sc:AdaptType  1      4.56 *    .033
# 6        SpID:AdaptType  1        0.50    .479
# 7 tML_sc:SpID:AdaptType  1        1.47    .225

summary(m)

###########################################################################
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
# Family: binomial  ( logit )
# Formula: Resp ~ tML_sc * SpID * AdaptType + (1 | Participant)
# Data: E2_Adapt_m
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
save(m, m_test, file= "input/E2_GLMs_male.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp2_GLMM_male.txt")


# extract fitted values
E2_Adapt_m$fitted <- fitted(m)

#aggregate values of fitted
GLM_fit <- mySummary(E2_Adapt_m, fitted, Participant, SpID, AdaptType)
GLM_descriptive <- mySummary(GLM_fit, fitted, SpID, AdaptType)
capture.output(GLM_descriptive, file= "output/Exp2_GLMM_male_descriptive.txt")

##End of Script