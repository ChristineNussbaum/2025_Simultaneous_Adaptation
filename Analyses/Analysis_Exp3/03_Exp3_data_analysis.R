##########################################################################
## File: 03_Exp3_data_analysis.R
## Data Analysis for Exp 3: Adaptation of Emotion - pseudowords
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
# predictors: ML, Word, AdaptType 
# random: Participant, SpID


#------------------------------------------------------------------------
# Step1: estimating the model 

# m: assumed fixed effects structure for Participant and SpID
m <- glmer(Resp ~ tML_sc * Word * AdaptType  + (1|SpID) + (1 | Participant), data = E3_Adapt, family = binomial,
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # it was recommended to try out different optimizers to avoid converging failure and this worked


#------------------------------------------------------------------------
# Step2: calculating the p-values of the effects with afex
m_test<- mixed(m, data = E3_Adapt, method = "LRT", family = "binomial") # takes a few minutes to estimate

m_test

# Model: Resp ~ tML_sc * Word * AdaptType+ (1 | SpID) + (1 | Participant)
# Data: E3_Adapt
# Df full model: 18
# Effect df       Chisq p.value
# 1                tML_sc  1 4111.70 ***   <.001
# 2                  Word  3  452.31 ***   <.001
# 3             AdaptType  1      6.52 *    .011
# 4           tML_sc:Word  3   40.61 ***   <.001
# 5      tML_sc:AdaptType  1        0.79    .375
# 6        Word:AdaptType  3        0.73    .866
# 7 tML_sc:Word:AdaptType  3      9.54 *    .023

summary(m) 


# save models as R objects
save(m, m_test, file= "input/E3_GLMs.RData")


# capture output of model "m" 
capture.output(summary(m), m_test, file= "output/Exp3_GLMM.txt")




#-------------------------------------------------------------------------------#
#   Analysis 1b: run the same with brms for estimation of the Baysian effect    #
#-------------------------------------------------------------------------------#

############
#Full Model
#note: specified with all possible interaction combinations


##get default priors first
default_prior <- default_prior(Resp ~ tML_sc +  Word + AdaptType + tML_sc:Word  + tML_sc:AdaptType +
                                 Word:AdaptType + tML_sc:Word:AdaptType + (1|SpID) + (1 | Participant),
                               data = E3_Adapt,  family = bernoulli(link = "logit"))



fullmodel <- brm(Resp ~ tML_sc +  Word + AdaptType + tML_sc:Word  + tML_sc:AdaptType +
                   Word:AdaptType + tML_sc:Word:AdaptType + (1|SpID) + (1 | Participant),
                         data = E3_Adapt,
                         family = bernoulli(link = "logit"),  # Defines Logistic Regression
  prior = default_prior,
  chains = 4, iter = 2000, warmup = 1000, cores = 4, #set to recommended values
  save_pars = save_pars(all = TRUE)) # to allow Bayes factor estimation later

#Note: the model gives me 22 divergent transitions after warmup. 
# I checked here: https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# it says that a few divergent trasitions are not too problematic, if I dont insist on a completely reliable inference
# therefore, I decided to accept the model as is. 22/1000 is less than 0.5%

######################################################
#Reduced Model without the interaction of Word and AdaptType

#note: the three-way interaction of tML_sc:Word:AdaptType remained in the model, because it was significant

##get default priors again
default_prior2 <- default_prior(Resp ~ tML_sc + AdaptType + Word + tML_sc:Word  + tML_sc:AdaptType + tML_sc:Word:AdaptType 
                               + (1|SpID) + (1 | Participant),
                               data = E3_Adapt,  family = bernoulli(link = "logit"))


#all effects are estimated, except Word:AdaptType
redmodel <- brm(Resp ~ tML_sc + AdaptType + Word + tML_sc:Word  + tML_sc:AdaptType 
                + tML_sc:Word:AdaptType  + (1|SpID) + (1 | Participant), 
                 data = E3_Adapt,
                 family = bernoulli(link = "logit"),  
                 prior = default_prior2,
                 chains = 4, iter = 2000, warmup = 1000, cores = 4, 
                save_pars = save_pars(all = TRUE))

#20 divergent transitions
# thats acceptable as well

BF <-bayes_factor(fullmodel, redmodel)


#save the models and the bayes_factors
save(fullmodel, redmodel, file = "input/bayesmodels.RData")

#capture output

capture.output(BF, file = "output/bayesfactor.txt")


#-----------------------------------------------------------------------------------------------------#
#   Analysis 2: The amount of classifications (averaged across tML, Pseudoword and SpSex)             #
#-----------------------------------------------------------------------------------------------------#


#aggregate across pseudoword, AdaptType for analysis
E3_Resp <- mySummary(E3_Adapt, Resp, Participant, Word, AdaptType) #does not contain Baseline
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
# 1    (Intercept)  F(1,41) = 945.709, p < .001, np2 = .958
# 2      AdaptType  F(1,41) =   4.829, p = .034, np2 = .105
# 3           Word F(3,123) = 110.770, p < .001, np2 = .730
# 4 AdaptType:Word F(3,123) =   0.508, p = .678, np2 = .012
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
E3_Bline_Resp <- mySummary(E3_Bline, Resp, Participant) 
E3_Bline_Resp <- E3_Bline_Resp %>% rename(Baseline = Resp)
Baseline_descriptive <- mySummary(E3_Bline_Resp, Baseline)

#merge
PH1 <- merge(PH1, E3_Bline_Resp)

rm(E3_Bline_Resp)

#########
#ttests
AdaptType <- t.test(PH1$`pw12_ang/pw34_fea`, PH1$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType <- t_to_d(AdaptType$statistic, AdaptType$parameter, paired = TRUE)
##################################################
# data:  PH1$`pw12_ang/pw34_fea` and PH1$`pw12_fea/pw34_ang`
# t = 2.1976, df = 41, p-value = 0.03368

AdaptType_b <- t.test(PH1$Baseline, PH1$`pw12_ang/pw34_fea`, paired = TRUE)
d_AdaptType_b <- t_to_d(AdaptType_b$statistic, AdaptType_b$parameter, paired = TRUE)
##################################################
# data:  PH1$Baseline and PH1$`pw12_ang/pw34_fea`
#t = -2.2085, df = 41, p-value = 0.03286

AdaptType_c <- t.test(PH1$Baseline, PH1$`pw12_fea/pw34_ang`, paired = TRUE)
d_AdaptType_c <- t_to_d(AdaptType_c$statistic, AdaptType_c$parameter, paired = TRUE)
##################################################
# data:  PH1$Baseline and PH1$`pw12_fea/pw34_ang`
# t = -0.68114, df = 41, p-value = 0.4996


#save results
capture.output(as.matrix(PH1_descriptive), as.matrix(Baseline_descriptive),
               AdaptType,d_AdaptType, 
               AdaptType_b, d_AdaptType_b,
               AdaptType_c, d_AdaptType_c,
               file = "output/Exp3_Resp_post-hoc_PH1.txt")

#keep environment tidy
rm(PH1_descriptive, Baseline_descriptive, AdaptType,d_AdaptType, AdaptType_b, d_AdaptType_b,
   AdaptType_c, d_AdaptType_c)

##End of Script