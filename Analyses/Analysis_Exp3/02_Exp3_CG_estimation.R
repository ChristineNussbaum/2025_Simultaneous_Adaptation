##########################################################################
## File: 02_Exp3_CG_estimation.R
## Data CG estimation for Exp 3: Adaptation of Emotion - pseudowords
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

# load relevant functions
source("functions/mySummary.R") 
source("functions/fitCumulativeGaussian.R")


#---------------------------------------------------------------------------------
#get the CG input data:

load(file ="input/Exp3_without_omissions.RData")


#-------------------------------------------------------------------------------#
#            Estimating cumulative gaussians for each participant               #
#-------------------------------------------------------------------------------#

#code Pseudowords into groups 
E3_Adapt$PW_Group <- ifelse(E3_Adapt$Word == "w01" | E3_Adapt$Word == "w02", "pw12", "pw34")
E3_Bline$PW_Group <- ifelse(E3_Bline$Word == "w01" | E3_Bline$Word == "w02", "pw12", "pw34")


##################################################################
##average data across SpeakerID (for Adaptation and Baseline block individually, and them combine the two)
CG_input <- mySummary(E3_Adapt, Resp, Participant, AdaptType, PW_Group, SpSex, tML) # Input for each participant, Adaptation Blocks
CG_input_BL <- mySummary(E3_Bline, Resp, Participant, Block, SpSex, PW_Group, tML) # Input for each participant, Baseline Blocks
CG_input_BL <- CG_input_BL %>% rename(AdaptType= Block)
CG_input <- rbind(CG_input, CG_input_BL)
rm(CG_input_BL)
#CG input has 47 participants * 2 PW_Group * 3 (AdaptTypes+Baseline) * 2 SpID (perSpSex) * 7 tML = 3948 rows

## declare tML as numerical variable
CG_input$tML <- as.numeric(as.character(CG_input$tML))


#Fit a Cumulative Gaussian Function (CG) for each Subject x SpSex * AdaptType
CGdata <- myCGfit(CG_input, x = unique(CG_input$tML), Resp, Participant, AdaptType, PW_Group, SpSex)
### There were 1278 warnings -> this happens when the algorithm tried to fit numbers that create a NaN in the pnorm()-command
warnings()

#CG_data has 47 participants * 2 PW_Group * 3 (AdaptTypes+Baseline) * 2 SpID (perSpSex) = 546


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

# inspect subjects that have a PSE > 100 and 0 
badFit <- CGdata %>% filter(PSE > 100 | PSE < 0)
unique(badFit$Participant) # 6 Subjects have bad fits and should be excluded in a second analysis


###remove 6 participants with bad fit
CGdata <- CGdata %>% filter(!(Participant %in% badFit$Participant))
#now CG_data has 41 participants * 2 SpSex * 3 (AdaptTypes+Baseline) * 2 pW_Group = 492

# now check again PSE values and R2
dotchart(CGdata$PSE) # all beautiful
dotchart(CGdata$R2)  # really not ideal


#-------------------------------------------------------------------------------#
#       Estimating cumulative gaussians averaged across all participant         #
#-------------------------------------------------------------------------------#

##create a new input variable, because we just have N=28 now
CG_input <- CG_input %>% filter(!(Participant %in% badFit$Participant))
CG_agg_input <- mySummary(CG_input, Resp, AdaptType, SpSex, PW_Group, tML) # Input averaged across all participant

## declare tML as numerical variable
CG_agg_input$tML <- as.numeric(as.character(CG_agg_input$tML))

#estimate the averaged cumulative gaussian
CGagg <- myCGfit(CG_agg_input, x = unique(CG_agg_input$tML), Resp, AdaptType, PW_Group, SpSex)


#save the datasets
save(CGagg, CGdata, CG_input, CG_agg_input, badFit, file="input/Exp3_CG_estimates.RData")


##End of Script