##########################################################################
## File: 04_Exp1_data_visualization.R
## Data Preparatation for Emotion-Adaptation-Experiment in Voices, BA Berges
# author: Christine Nussbaum und Dorothea Berges
# date 02/2022, revised 05/2024

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

load(file ="input/Exp1_raw_data.RData")


#-------------------------------------------------------------------------------#
#                            1 - Plot simple response data                      #
#-------------------------------------------------------------------------------# 

#average raw data (for both Adapt and Baseline Blocks)
E1_Adapt_plot <- mySummary(E1_Adapt, Resp, Participant, tML, SpSex, AdaptType)
E1_Bline_plot <- mySummary(E1_Bline, Resp, Participant, tML, SpSex, Block)
E1_Bline_plot <- E1_Bline_plot %>% rename(AdaptType= Block)
E1_plot <- rbind(E1_Adapt_plot, E1_Bline_plot)
rm(E1_Adapt_plot, E1_Bline_plot)
E1_plot_agg<- mySummary(E1_plot, Resp, tML,  SpSex, AdaptType)


#[1a] Response per tML, SpSex and AdaptType
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 40)")

filename = paste0("plots/01a_Resp_Exp1.png")

#Plot
p<-(ggplot(data= E1_plot_agg, aes(x = tML, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      facet_wrap(~ SpSex, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("grey", "darkorange", "darkgreen")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#[1b] Response per tML, SpSex and AdaptType (plotted the other way around and without Baseline)
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 40)")

filename = paste0("plots/01b_Resp_Exp1.png")

#Plot
p<-(ggplot(data= E1_plot_agg[E1_plot_agg$AdaptType != "Baseline",], aes(x = tML, y=Resp, color = SpSex, group=SpSex)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker Sex", title = title) +
      facet_wrap(~ AdaptType, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "darkblue")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)



#[1c] Plot the interaction averaged across tML
yTitleStr = "Proportion of angry responses"
xTitleStr =  "AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N = 40)")
filename = paste0("plots/01c_Resp_Exp1.png")

E1_plot_agg2<- mySummary(E1_plot, Resp,SpSex, AdaptType)


#Plot
p<-(ggplot(data= E1_plot_agg2[E1_plot_agg2$AdaptType != "Baseline",], aes(x = AdaptType, y=Resp, color = SpSex, group=SpSex)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker Sex", title = title) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "darkblue")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 8, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)



#[1d] Plot the interaction averaged across tML, the other way around and with Baseline
yTitleStr = "Proportion of angry responses"
xTitleStr =  "SpSex"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N = 40)")
filename = paste0("plots/01d_Resp_Exp1.png")

E1_plot_agg2<- mySummary(E1_plot, Resp,SpSex, AdaptType)


#Plot
p<-(ggplot(data= E1_plot_agg2, aes(x = SpSex, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("grey", "darkorange", "darkgreen")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 8, height = 8, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#remove all files to keep environment tidy
rm(E1_Adapt, E1_Bline, E1_plot, E1_plot_agg, E1_plot_agg2)

#-------------------------------------------------------------------------------#
#                                2 - Plot CG data                               #
#-------------------------------------------------------------------------------# 

#load the data of the CG estimates
load(file="input/Exp1_CG_estimates.RData")

#we dont need badfit
rm(badFit)


#[2a] Plot the interaction, using the CG data, averaged across participants


#Beschriftung
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 35)")

filename = paste0("plots/02a_CG_Exp1.png")


# Data sampling: 
plot_data <- 
  pmap_df(CGagg[,1:4],
          function(AdaptType, SpSex, PSE, SD) {
            tibble(AdaptType = AdaptType,
                   SpSex = SpSex,
                   x = seq(20, 80, by = 1),
                   y = pnorm(x, PSE, SD))
          })
# Check: 6 *61 = 366



#plot
p<-(ggplot(CG_agg_input, aes(x=tML, y=Resp, colour=AdaptType)) + 
      geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
      geom_point(mapping = aes(colour = AdaptType) ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) + 
      scale_colour_manual(values=c("grey", "darkorange", "darkgreen")) +
      facet_wrap(~SpSex)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      geom_line(data = plot_data, aes(colour = AdaptType, x = x, y = y)) +
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
 ggsave(filename, width = 10, height = 4, dpi =300)	



remove(p, xTitleStr, yTitleStr, filename, title, plot_data)



#[2b] Plot the interaction, using the CG data, averaged across participants, the other way around and without Baseline


#Beschriftung
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  " splitted per SpSex and AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 35)")

filename = paste0("plots/02b_CG_Exp1.png")




#plot
p<-(ggplot(CG_agg_input[CG_agg_input$AdaptType != "Baseline",], aes(x=tML, y=Resp, colour=SpSex)) + 
      geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
      geom_point(mapping = aes(colour = SpSex) ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker Sex", title = title) + 
      scale_colour_manual(values=c("darkred", "darkblue")) + 
      facet_wrap(~AdaptType)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      geom_line(data = plot_data[plot_data$AdaptType!="Baseline",], aes(colour = SpSex, x = x, y = y)) +
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 10, height = 4, dpi =300)	



remove(p, xTitleStr, yTitleStr, filename, title, plot_data)


#[2c] Plots for each individual participant
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  " splitted per SpSex and AdaptType, individual participants"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 35)")

filename = paste0("plots/02c_CG_Exp1.png")

#generate the data to plot
plot_data <- 
  pmap_df(CGdata[,1:5],
          function(AdaptType,SpSex, Participant, PSE, SD) {
            tibble(AdaptType = AdaptType,
                   SpSex = SpSex,
                   Participant = Participant,
                   x = seq(20, 80, by = 1),
                   y = pnorm(x, PSE, SD))
          })

#check: 6 * 35 * 61 = 12810


#plot
p<-(ggplot(CG_input[CG_input$AdaptType != "Baseline",], aes(x=tML, y=Resp, colour=SpSex)) + 
      geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
      geom_point(mapping = aes(colour = SpSex) ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker Sex", title = title) + 
      scale_colour_manual(values=c("darkred", "darkblue")) + 
      facet_wrap(~Participant + AdaptType, ncol = 10)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      geom_line(data = plot_data[plot_data$AdaptType!="Baseline",], aes(colour = SpSex, x = x, y = y)) +
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 16, height = 12, dpi =300)	


remove(p, xTitleStr, yTitleStr, filename, title, plot_data)

#remove all files to keep environment tidy
rm(CG_agg_input, CG_input, CGagg, CGdata)


#-------------------------------------------------------------------------------#
#                  2 - Plot data of logistic regression                         #
#-------------------------------------------------------------------------------#


