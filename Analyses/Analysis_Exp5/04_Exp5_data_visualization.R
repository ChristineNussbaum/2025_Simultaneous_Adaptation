##########################################################################
## File: 04_Exp5_data_visualization.R
## Data Visualization for Exp 5: Adaptation of Emotion - speaker identities JAVMEPS
# author: Christine Nussbaum 
# date 03/2025

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load required packages
library("tidyverse")

# load relevant functions
source("functions/mySummary.R") 


#---------------------------------------------------------------------------------
#get the raw input data

load(file ="input/Exp5_without_omissions.RData")

#-------------------------------------------------------------------------------#
#                            1 - Plot simple response data                      #
#-------------------------------------------------------------------------------# 

#average raw data (for both Adapt and Baseline Blocks)
E5_Adapt_plot <- mySummary(E5_Adapt, Resp, Participant, tML, SpSex, SpID, AdaptType)
E5_Bline_plot <- mySummary(E5_Bline, Resp, Participant, tML, SpSex, SpID, Block)
E5_Bline_plot <- E5_Bline_plot %>% rename(AdaptType= Block)
E5_plot <- rbind(E5_Adapt_plot, E5_Bline_plot)
rm(E5_Adapt_plot, E5_Bline_plot)
E5_plot_agg<- mySummary(E5_plot, Resp, tML,  SpSex, SpID, AdaptType)



#[1a] Response per tML, SpID, SpSex and AdaptType
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpID and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 43)")

filename = paste0("plots/01a_Resp_Exp5.png")

#Plot
p<-(ggplot(data= E5_plot_agg, aes(x = tML, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      facet_wrap(~ SpID + SpSex, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("grey", "darkorange", "darkgreen", "darkorange", "darkgreen")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 10, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#[1b] Response per tML, SpSex and AdaptType (plotted the other way around and without Baseline)
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpID and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 43)")

filename = paste0("plots/01b_Resp_Exp5.png")

#Plot
p<-(ggplot(data= E5_plot_agg[E5_plot_agg$AdaptType != "Baseline",], aes(x = tML, y=Resp, color = SpID, group=SpID)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker Sex", title = title) +
      facet_wrap(~ SpSex + AdaptType, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "darkblue", "darkred", "darkblue")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)



#[1c] Plot the interaction averaged across tML
yTitleStr = "Proportion of angry responses"
xTitleStr =  "AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N = 43)")
filename = paste0("plots/01c_Resp_Exp5.png")

E5_plot_agg2<- mySummary(E5_plot, Resp,SpID, SpSex, AdaptType)


#Plot
p<-(ggplot(data= E5_plot_agg2[E5_plot_agg2$AdaptType != "Baseline",], aes(x = AdaptType, y=Resp, color = SpID, group=SpID)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "darkblue", "darkred", "darkblue")) + 
      facet_wrap(~ SpSex, ncol = 2, drop = TRUE, scales="free") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 12, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, title, filename, p)



#[1d] Plot the interaction averaged across tML, the other way around and with Baseline
yTitleStr = "Proportion of angry responses"
xTitleStr =  "SpSex"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N = 43)")
filename = paste0("plots/01d_Resp_Exp5.png")

E5_plot_agg2<- mySummary(E5_plot, Resp, SpID, SpSex, AdaptType)


#Plot
p<-(ggplot(data= E5_plot_agg2, aes(x = SpID, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("grey", "darkorange", "darkgreen", "darkorange", "darkgreen")) +
      facet_grid( cols= vars(SpSex), drop = TRUE, scales="free") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 12, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, title, filename, p)


#remove all files to keep environment tidy
rm(E5_Adapt, E5_Bline, E5_plot, E5_plot_agg, E5_plot_agg2)


#-------------------------------------------------------------------------------#
#                  3 - Plot data of logistic regression                         #
#-------------------------------------------------------------------------------#

#load the adaptation data
load(file ="input/Exp5_without_omissions.RData")

rm(E5_Bline)

#split dataset into male and femake
E5_Adapt_f <- E5_Adapt %>% filter(SpSex == "f")
E5_Adapt_m <- E5_Adapt %>% filter(SpSex == "m")

#load the model for female speakers only
load(file= "input/E5_GLMs_female.RData")
# extract fitted values
E5_Adapt_f$fitted <- fitted(m)
#remove the model data
rm(m_test, m) 

#load the model for male speakers only
load(file= "input/E5_GLMs_male.RData")
# extract fitted values
E5_Adapt_m$fitted <- fitted(m)
#remove the model data
rm(m_test, m)

#glue them back together
E5_Adapt <- rbind(E5_Adapt_f, E5_Adapt_m)
rm(E5_Adapt_f, E5_Adapt_m)

#average data
E5_Adapt_plot <- mySummary(E5_Adapt, fitted, Participant, tML, SpID, SpSex, AdaptType)
E5_Adapt_plot_agg <- mySummary(E5_Adapt_plot, fitted, tML, SpID, SpSex, AdaptType)


#[3a] Response per tML, SpSex and AdaptType - without Baseline
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 43)")

filename = paste0("plots/03a_Resp_Exp5.png")

#Plot
p<-(ggplot(data= E5_Adapt_plot_agg, aes(x = tML, y=fitted, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (fitted-CI), ymax = (fitted+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      facet_wrap(~ SpID + SpSex, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkorange", "darkgreen", "darkorange", "darkgreen")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#[3b] Response per tML, SpSex and AdaptType (plotted the other way around)
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 43)")

filename = paste0("plots/03b_Resp_Exp5.png")

#Plot
p<-(ggplot(data= E5_Adapt_plot_agg, aes(x = tML, y=fitted, color = SpID, group=SpID)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (fitted-CI), ymax = (fitted+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) +
      facet_wrap(~ SpSex + AdaptType, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "darkblue", "darkred", "darkblue")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#[3c] Plots for each individual participant
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 43)")

filename = paste0("plots/03c_Resp_Exp5.png")

#for the original response info: 
E5_Adapt_plot_resp <- mySummary(E5_Adapt, Resp, Participant, tML, SpID, SpSex, AdaptType)

#Plot
p<-(ggplot(data= E5_Adapt_plot, aes(x = tML, y=fitted, color = SpID, group=SpID)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (fitted-CI), ymax = (fitted+CI)), width = 0.1 ) + 
      geom_point(data= E5_Adapt_plot_resp, aes(x = tML, y=Resp, group=SpSex), color = "grey") +
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) +
      facet_wrap(~Participant + AdaptType + SpSex, ncol = 16)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "darkblue", "darkred", "darkblue")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 20, height = 20, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)

##End of Script