##########################################################################
## File: 03_Exp1_data_visualization.R
## Data Preparatation for Emotion-Adaptation-Experiment in Voices, BA Berges
# author: Christine Nussbaum und Dorothea Berges
# date 02/2022, revised 11/2025

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load required packages
library("tidyverse")
library(ggsignif)

# load relevant functions
source("functions/mySummary.R") 


#---------------------------------------------------------------------------------
#get the raw input data

load(file ="input/Exp1_without_omissions.RData")


#-------------------------------------------------------------------------------#
#                      1 - Plot response data, for each tML                     #
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


#-------------------------------------------------------------------------------#
#                      1 - Boxplots, for each                                   #
#-------------------------------------------------------------------------------# 

E1_plot_agg2<- mySummary(E1_plot, Resp, Participant, SpSex, AdaptType)
E1_plot_agg2<- mySummary(E1_plot_agg2, Resp, SpSex, AdaptType)


yTitleStr = "Proportion of angry responses"

filename = paste0("plots/02_Resp_Barplot.png")


#add significance values
annotation_df <- data.frame(
  SpSex= c("f","f","f", "m", "m","m"),
  start = c("f_ang/m_fea", "Baseline", "Baseline","f_ang/m_fea", "Baseline", "Baseline"),
  end = c("f_fea/m_ang", "f_ang/m_fea", "f_fea/m_ang", "f_fea/m_ang", "f_ang/m_fea", "f_fea/m_ang"),
  y = c(0.65, 0.6, 0.55, 0.65, 0.6, 0.55),
  label = c("*", "*","*","*","*","*")
)

p<-ggplot(data= E1_plot_agg2) +
      geom_bar(aes(x = AdaptType, y=Resp, fill = AdaptType),stat = "identity") +
      geom_errorbar(aes(x = AdaptType, ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = "Condition" , y = yTitleStr, color = "Condition") +
      geom_signif(data = annotation_df,
                  aes(xmin = start, xmax = end, annotations = label, y_position = y),
                  textsize = 3, vjust = -0.2, manual = TRUE) + 
      facet_wrap(~ SpSex, ncol = 2) +
      scale_fill_manual(values=c("grey", "darkorange", "darkgreen")) + 
      coord_cartesian(ylim=c(0.3, 0.7)) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(breaks = c(0.3, 0.4, 0.6))

p


#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)



##End of Script