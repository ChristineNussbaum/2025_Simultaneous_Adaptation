####################################################################
##Baseline_CG Rechnung
#Bachelorarbeit Dorothea Berges
#Datum: 02/2022

# clear directory
rm(list=ls())

# set working directory
setwd("~/Studium_Psychologie/Bachelorarbeit/Experiment/Code/Ergebnisse/Auswertung/data_analysis_Doro")

# load required packages

library("tidyverse")
library("ez")
library("effectsize")
library("ggplot2")

# load relevant functions
source("functions/mySummary.R") 
source("functions/fitCumulativeGaussian.R")
source("functions/tracedEzOut.R")


#---------------------------------------------------------------------------------
#get the CG input data:

load(file ="input/CG_input_data.RData")

#40*2*7 = 560 

CG_input_Baseline <- CG_input_Baseline %>% group_by(Subject, tML, SpSex) %>% summarise(Resp = mean(Resp))

#TOdO:
#CG_input <- CG_input %>% filter(!(Subject %in% badFit$Subject))

#-------------------------------------------------------------------------------#
#                     CG-BASELINE Schätzung für jeden Probanden einzeln                  #
#-------------------------------------------------------------------------------#

##################################################################
## declare tML and Resp as numerical variable
CG_input_Baseline$tML <- as.numeric(as.character(CG_input_Baseline$tML))

#CG input has 40 participants * 2 SpSex * 7 tML = 560 rows

#view(CG_input_Baseline)

#Fit a Cumulative Gaussian Function (CG) for each Subject x SpSex 
CGdata <- myCGfit(CG_input_Baseline, x = unique(CG_input_Baseline$tML), Resp, Subject, SpSex)

CGdata2 <- myCGfit(CG_input_Baseline, x = unique(CG_input_Baseline$tML), Resp, Subject)
### There were 36 warnings -> this happens when the algorithm tried to fit numbers that create a NaN in the pnorm()-command

#CGdata has 40 participants * 2 SpSex = 80


##################################################################
## plot values of the fits to identify outlier
summary(CGdata[,4:5]) # yep... some fits went wrong

#PSE - point of subjective equality
dotchart(CGdata_BL$PSE)
hist(CGdata_BL$PSE)
boxplot(CGdata_BL$PSE)
#Note: a PSE higher than 80 or lower than 20 is outside the measured target-range (20-80%) and might be excluded from the analysis

#R2 - fit of the function
dotchart(CGdata_BL$R2)
hist(CGdata_BL$R2)
boxplot(CGdata-BL$R2)
#Note: Ideally, fits should be greater than 80, but there is not clear cutoff, when a fit should be excluded


# inspect subjects that have a PSE > 80 and a R2 < 70
badFitR2BL <- CGdata %>% filter(R2 < 0.7)
badFitPSEBL <- CGdata %>% filter(PSE > 80 | PSE < 20)
badFitBL <- rbind(badFitR2, badFitPSE)
unique(badFitBL$Subject) # 4 Subjects have bad fits and should be excluded in a second analysis
rm(badFitR2BL, badFitPSEBL)

###remove participants with bad fit
CGdata_BL <- CGdata_BL %>% filter(!(Subject %in% badFitBL$Subject))

# now check again PSE values and R2
dotchart(CGdata_BL$PSE) # all beautiful
dotchart(CGdata_BL$R2)  # very nice


#-------------------------------------------------------------------------------#
#                     CG-BASELINE Schätzung über alle Probanden gemittelt                #
#-------------------------------------------------------------------------------#

## evtl sollte man die Probanden mit den bad Fits aus den average data rausnehmen - kannst du ja später mal machen


#Zuerst: wir sagen R wieder, dass tML numerisch ist!
CG_agg_input$tML <- as.numeric(as.character(CG_agg_input$tML))

#Schätzung für jeden Probanden
CGagg <- myCGfit(CG_agg_input, x = unique(CG_agg_input$tML), Resp, AdaptType, SpSex)
