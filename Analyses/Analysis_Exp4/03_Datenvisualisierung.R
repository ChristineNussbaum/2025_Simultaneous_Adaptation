##########################################################################
## File: 03_Datenvisualisierung.R
## Einlesen und Aufbereiten der Experimentaldaten 
# author: Christine Nussbaum
# date 02/2025 

# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"


# clear directory
rm(list=ls())


# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# load required packages
library(tidyverse)

# load relevant functions
source("functions/loadPTKExperimentData.R")
source("functions/mySummary.R") 


#---------------------------------------------------------------------------------
#laden der Datensätze
load(file="input/D_raw.RData")
load(file="input/B_signaldetection.RData")

#---------------------------------------------------------------------------------
#[1] Signal Detection Parameters

#aggregate
B_dprime <- mySummary(B, dprime, Dataset, SpSex)
B_dprime <- B_dprime[,c(1:3, 7)]
names(B_dprime) <- c("Dataset", "SpSex", "value", "CI")
B_dprime$parameter <- "D-Prime"

B_crit <- mySummary(B, crit, Dataset, SpSex)
B_crit <- B_crit[,c(1:3, 7)]
names(B_crit) <- c("Dataset", "SpSex", "value", "CI")
B_crit$parameter <- "Criterion"

B_agg <- rbind(B_dprime, B_crit)


B_agg$parameter <- factor(B_agg$parameter, levels= c("D-Prime", "Criterion"))
#-----------------------------------------------------------------------------------#
#                 Plot 1: Signal Detection Parameters                      #
#-----------------------------------------------------------------------------------#



title = "Signal Detection Parameters"

filename = paste0("plots/1_signal_detection.png")

B_agg$parameter

#[1]
p<-(ggplot(data= B_agg, aes(x = SpSex, y=value, group = Dataset)) +
      geom_point(shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (value-CI), ymax = (value+CI)), width = 0.1 ) + 
      labs(x = "Speaker Sex" , y = "") +       #, title = title
      facet_grid(rows= vars(Dataset), cols = vars(parameter), scales = "free_x", switch = "y") +
      geom_hline(yintercept = 0, linetype = 4) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14))) #+ 
      #scale_y_continuous(limits=c(-1,1.5), breaks = c(-1,0, -0.5, 0.0,  0.5, 1.0, 1,5)))
ggsave(filename, width = 6, height = 6, dpi =300)



## End of Script