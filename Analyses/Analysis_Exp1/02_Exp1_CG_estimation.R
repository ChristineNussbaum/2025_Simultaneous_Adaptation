##########################################################################
## File: 02_Exp1_CG_estimation.R
## Data CG estimation for Exp 1: Adaptation of Emotion - male/female voices, Cumulative Gaussian
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
#library("ggplot2")

# load relevant functions
source("functions/mySummary.R") 
source("functions/fitCumulativeGaussian.R")


#---------------------------------------------------------------------------------
#get the CG input data:

load(file ="input/Exp1_raw_data.RData")


#-------------------------------------------------------------------------------#
#            Estimating cumulative gaussians for each participant               #
#-------------------------------------------------------------------------------#

##################################################################
##average data across speaker and pseudoword (for Adaptation and Baselin block individually, and them combine the two)
CG_input <- mySummary(E1_Adapt, Resp, Participant, AdaptType, SpSex, tML) # Input for each participant, Adaptation Blocks
CG_input_BL <- mySummary(E1_Bline, Resp, Participant, Block, SpSex, tML) # Input for each participant, Baseline Blocks
CG_input_BL <- CG_input_BL %>% rename(AdaptType= Block)
CG_input <- rbind(CG_input, CG_input_BL)
rm(CG_input_BL)
#CG input has 40 participants * 2 SpSex * 3 (AdaptTypes+Baseline) * 7 tML = 1680 rows

## declare tML as numerical variable
CG_input$tML <- as.numeric(as.character(CG_input$tML))


#Fit a Cumulative Gaussian Function (CG) for each Subject x SpSex * AdaptType
CGdata <- myCGfit(CG_input, x = unique(CG_input$tML), Resp, Participant, AdaptType, SpSex)
### There were 102 warnings -> this happens when the algorithm tried to fit numbers that create a NaN in the pnorm()-command
warnings()

#CG_data has 40 participants * 2 SpSex * 3 (AdaptTypes+Baseline) = 240

view(CGdata)

##################################################################
## plot values of the fits to identify outlier
summary(CGdata[,4:6]) # yep... some fits went wrong

#PSE - point of subjective equality
dotchart(CGdata$PSE)
hist(CGdata$PSE)
boxplot(CGdata$PSE)
#Note: a PSE higher than 80 or lower than 20 is outside the measured target-range (20-80%) and might be excluded from the analysis

#R2 - fit of the function
dotchart(CGdata$R2)
hist(CGdata$R2)
boxplot(CGdata$R2)
#Note: Ideally, fits should be greater than 80, but there is not clear cutoff, when a fit should be excluded

# inspect subjects that have a PSE > 80 and a R2 < 70
badFitR2 <- CGdata %>% filter(R2 < 0.6)
badFitPSE <- CGdata %>% filter(PSE > 80 | PSE < 20)
badFit <- rbind(badFitR2, badFitPSE)
unique(badFit$Participant) # 5 Subjects have bad fits and should be excluded in a second analysis
rm(badFitR2, badFitPSE)

###remove 4 participants with bad fit
CGdata <- CGdata %>% filter(!(Participant %in% badFit$Participant))
#now CG_data has 35 participants * 2 SpSex * 3 (AdaptTypes+Baseline) = 210

# now check again PSE values and R2
dotchart(CGdata$PSE) # all beautiful
dotchart(CGdata$R2)  # not all ideal, but acceptable


#-------------------------------------------------------------------------------#
#       Estimating cumulative gaussians averaged across all participant         #
#-------------------------------------------------------------------------------#

##create a new input variable, becaue we just have N=36 now
CG_input <- CG_input %>% filter(!(Participant %in% badFit$Participant))
CG_agg_input <- mySummary(CG_input, Resp, AdaptType, SpSex, tML) # Input averaged across all participant

## declare tML as numerical variable
CG_agg_input$tML <- as.numeric(as.character(CG_agg_input$tML))

#estimate the averaged cumulative gaussian
CGagg <- myCGfit(CG_agg_input, x = unique(CG_agg_input$tML), Resp, AdaptType, SpSex)


#save the datasets
save(CGagg, CGdata, CG_input, CG_agg_input, badFit, file="input/Exp1_CG_estimates.RData")


##End of Script