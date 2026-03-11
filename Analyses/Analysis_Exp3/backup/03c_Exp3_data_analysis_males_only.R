##########################################################################
## File: 03c_Exp3_data_analysis_males_only.R
## Data Analysis for Exp 3: Adaptation of Emotion - pseudowords
# male voices only
# author: Christine Nussbaum 
# date 03/2025, updated 02/2026

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
library(brms)           # for Bayesian estimation

# load relevant functions
source("functions/mySummary.R") 
source("functions/tracedEzOut.R")

#set seed
set.seed(42)

#---------------------------------------------------------------------------------
#get the input data:

#load the data
load(file ="input/Exp3_without_omissions.RData")

#select female voices only

E3_Adapt_m <- E3_Adapt %>% filter(SpSex == "m")
E3_Bline_m <- E3_Bline %>% filter(SpSex == "m")

#-------------------------------------------------------------------------------#
#                  Analysis 1: logistic regression                              #
#-------------------------------------------------------------------------------#


## declare tML as numerical variable
E3_Adapt_m$tML <- as.numeric(as.character(E3_Adapt_m$tML))

#scale tML, because this is recommended for mixed-effects modelling
E3_Adapt_m$tML_sc <- scale(E3_Adapt_m$tML)[,1]

#rename "w5" into "w4"

E3_Adapt_m$Word <- ifelse(E3_Adapt_m$Word == "w05", "w04", E3_Adapt_m$Word)
E3_Bline_m$Word <- ifelse(E3_Bline_m$Word == "w05", "w04", E3_Bline_m$Word)

# outcome: Resp
# predictors: ML, Word, AdaptType 
# random: Participant, SpID


#------------------------------------------------------------------------
# Step1: estimating the model 

# m: assumed fixed effects structure for Participant and SpID
m <- glmer(Resp ~ tML_sc * Word * AdaptType  + (1|SpID) + (1 | Participant), data = E3_Adapt_m, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked


#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E3_Adapt_m, method = "LRT", family = "binomial") # takes a few minutes to estimate

m_test

# Model: Resp ~ tML_sc * Word * AdaptType+ (1 | SpID) + (1 | Participant)
# Data: E3_Adapt_m
# Df full model: 18
# Effect df       Chisq p.value
# 1                tML_sc  1 2231.06 ***   <.001
# 2                  Word  3  136.73 ***   <.001
# 3             AdaptType  1      6.58 *    .010
# 4           tML_sc:Word  3   83.62 ***   <.001
# 5      tML_sc:AdaptType  1        2.67    .102
# 6        Word:AdaptType  3        1.28    .733
# 7 tML_sc:Word:AdaptType  3        3.71    .295

summary(m) 


# save models as R objects
save(m, m_test, file= "input/E3_GLMs_males.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp3_GLMM_males.txt")




#-------------------------------------------------------------------------------#
#   Analysis 1b: run the same with brms for estimation of the Baysian effect    #
#-------------------------------------------------------------------------------#

############
#Full Model
#note: specified with all possible interaction combinations
#note: we have convergence problems, therefore I estimated a much simpler Model

##get default priors first
default_prior <- default_prior(Resp ~ tML_sc +  Word + AdaptType + Word:AdaptType + (1|SpID) + (1 | Participant),
                               data = E3_Adapt_m,  family = bernoulli(link = "logit"))



fullmodel <- brm(Resp ~ tML_sc +  Word + AdaptType + Word:AdaptType + (1|SpID) + (1 | Participant),
                         data = E3_Adapt_m,
                         family = bernoulli(link = "logit"),  # Defines Logistic Regression
  prior = default_prior,
  chains = 4, iter = 2000, warmup = 1000, cores = 4, #set to recommended values
  save_pars = save_pars(all = TRUE)) # to allow Bayes factor estimation later

#Note: the model gives me 39 divergent transitions after warmup. 
#this is a problem that I need to attend to... meh...


######################################################
#Reduced Model without the interaction of Word and AdaptType

#note: the three-way interaction of tML_sc:Word:AdaptType remained in the model, because it was significant

##get default priors again
default_prior2 <- default_prior(Resp ~ tML_sc +  Word + AdaptType + (1|SpID) + (1 | Participant),
                               data = E3_Adapt_m,  family = bernoulli(link = "logit"))


#all effects are estimated, except Word:AdaptType
redmodel <- brm(Resp ~ tML_sc +  Word + AdaptType +  (1|SpID) + (1 | Participant),
                 data = E3_Adapt_m,
                 family = bernoulli(link = "logit"),  
                 prior = default_prior2,
                 chains = 4, iter = 2000, warmup = 1000, cores = 4, 
                save_pars = save_pars(all = TRUE))

#61 divergent transitions
# thats acceptable as well

BF <-bayes_factor(fullmodel, redmodel)


#save the models and the bayes_factors
save(fullmodel, redmodel, file = "input/bayesmodels_male.RData")

#capture output

capture.output(BF, file = "output/bayesfactor_male.txt")


#-----------------------------------------------------------------------------------------------------#
#   Analysis 2: The amount of classifications (averaged across tML, Pseudoword and SpSex)             #
#-----------------------------------------------------------------------------------------------------#


#aggregate across pseudoword, AdaptType for analysis
E3_Resp <- mySummary(E3_Adapt_m, Resp, Participant, Word, AdaptType) #does not contain Baseline
#just to confirm again that there is no interaction

##########################################################################
## ANOVA II: 2 x 2 x 4 within subject
# data: E3_Resp
# dv: Resp
# wid: Participant
# within: AdaptType, Word

a<-ezANOVA(data=E3_Resp, dv=.(Resp), wid=.(Participant), within = .(AdaptType, Word), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

##############################################################################
# Effect                                     Text
# 1    (Intercept)  F(1,41) = 622.299, p < .001, np2 = .938
# 2      AdaptType  F(1,41) =   6.662, p = .014, np2 = .140
# 3           Word F(3,123) =  43.314, p < .001, np2 = .514
# 4 AdaptType:Word F(3,123) =   0.416, p = .742, np2 = .010
##############################################################################


########################################################################################
### Post-Hoc Analysis: Comparing AdaptType to the Baseline
########################################################################################

#aggregate Data (in this case not really necessary)
PH1 <- E3_Resp %>% group_by(Participant, AdaptType) %>% summarise(Resp = mean(Resp))

#get descriptive data 
PH1_descriptive <- mySummary(PH1, Resp, AdaptType)

#convert partly into wide format
PH1 <- spread(PH1, AdaptType, Resp)

###now we add the Baseline information 
E3_Bline_m_Resp <- mySummary(E3_Bline_m, Resp, Participant) 
E3_Bline_m_Resp <- E3_Bline_m_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E3_Bline_m_Resp, Baseline)

#merge
PH1 <- merge(PH1, E3_Bline_m_Resp)

rm(E3_Bline_m_Resp)

#########
#ttests
AdaptType <- t.test(PH1$`pw12_ang/pw34_fea`, PH1$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType <- t_to_d(AdaptType$statistic, AdaptType$parameter, paired = TRUE)
##################################################
# data:  PH1$`pw12_ang/pw34_fea` and PH1$`pw12_fea/pw34_ang`
#t = 2.581, df = 41, p-value = 0.01353

AdaptType_b <- t.test(PH1$Baseline, PH1$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_b <- t_to_d(AdaptType_b$statistic, AdaptType_b$parameter, paired = TRUE)
##################################################
# data:  PH1$Baseline and PH1$`pw12_ang/pw34_fea`
#t = -2.2099, df = 41, p-value = 0.03275

AdaptType_c <- t.test(PH1$Baseline, PH1$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_c <- t_to_d(AdaptType_c$statistic, AdaptType_c$parameter, paired = TRUE)
##################################################
# data:  PH1$Baseline and PH1$`pw12_fea/pw34_ang`
# t = -0.26456, df = 41, p-value = 0.7927


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType,d_AdaptType, 
               AdaptType_b, d_AdaptType_b,
               AdaptType_c, d_AdaptType_c,
               file = "output/Exp3_Resp_post-hoc_PH1_males.txt")

#keep environment tidy
rm(PH1_descriptive, Baseline_descriptive, AdaptType,d_AdaptType, AdaptType_b, d_AdaptType_b,
   AdaptType_c, d_AdaptType_c)

##End of Script