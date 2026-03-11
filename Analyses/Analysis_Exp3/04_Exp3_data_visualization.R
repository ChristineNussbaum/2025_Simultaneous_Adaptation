##########################################################################
## File: 03_Exp2_data_visualization.R
## Data Analysis for Exp 2: Adaptation of Emotion  - pseudowords
# author: Christine Nussbaum 
# date 03/2026

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load required packages
library("tidyverse")
library(ggsignif)
library(gridExtra) # version 2.3
library(ggpubr)    # version 0.6.0
library(cowplot)   # version 1.1.1

# load relevant functions
source("functions/mySummary.R") 


#---------------------------------------------------------------------------------
#get the raw input data

load(file ="input/Exp3_without_omissions.RData")

#meaning of variables -> refer to Script "02_Exp3_data_analysis.R"


#rename "w5" into "w4"
E3_Adapt$Word <- ifelse(E3_Adapt$Word == "w05", "w04", E3_Adapt$Word)
E3_Bline$Word <- ifelse(E3_Bline$Word == "w05", "w04", E3_Bline$Word)


#-------------------------------------------------------------------------------#
#        1 - Plot response data, for each tML                                   #
#-------------------------------------------------------------------------------# 

#average raw data (for both Adapt and Baseline Blocks)
E3_Adapt_plot <- mySummary(E3_Adapt, Resp, Participant, tML, Word, AdaptType)
E3_Bline_plot <- mySummary(E3_Bline, Resp, Participant, tML, Word)
E3_Bline_plot$Block <- "Baseline"
E3_Bline_plot <- E3_Bline_plot %>% rename(AdaptType= Block)
E3_plot <- rbind(E3_Adapt_plot, E3_Bline_plot)
rm(E3_Adapt_plot, E3_Bline_plot)
E3_plot_agg<- mySummary(E3_plot, Resp, tML,  Word, AdaptType)


#[1a] Response per tML, SpID and AdaptType , females only
yTitleStr = "Proportion of angry responses"
xTitleStr = "Target Morphlevel (tML)"
facetStr =  " splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr)

filename = paste0("plots/01a_Resp_Exp2.png")

# New facet label names
Word.labs <- c("PW1: /belam/", "PW2: /molen/", "PW3: /namil/", "PW4: /loman/")
names(Word.labs) <- c("w01", "w02", "w03", "w04")

#Plot
p1<-(ggplot(data= E3_plot_agg, aes(x = tML, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) + #, title = title
      facet_wrap(~ Word, ncol = 4, labeller = labeller(Word = Word.labs)) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("grey", "skyblue4", "goldenrod3")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=18),
            axis.title=element_text(size=18), 
            axis.text.x = element_text(color = "black", size = 18), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 18), 
            strip.text.x = element_text(size = 18), 
            legend.position = "none") + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#save plot
ggsave(filename, width = 15, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename)

#-------------------------------------------------------------------------------#
#                         2a - Barplots                                         #
#-------------------------------------------------------------------------------# 

E3_plot_agg2<- mySummary(E3_plot, Resp, Participant, Word, AdaptType)
E3_plot_agg2<- mySummary(E3_plot_agg2, Resp, Word, AdaptType)

table(E3_plot_agg2$AdaptType)

yTitleStr = "Proportion of angry responses"

filename = paste0("plots/02_Resp_Barplot.png")

# New facet label names
Word.labs <- c("PW1: /belam/", "PW2: /molen/", "PW3: /namil/", "PW4: /loman/")
names(Word.labs) <- c("w01", "w02", "w03", "w04")



p2<-ggplot(data= E3_plot_agg2) +
      geom_bar(aes(x = AdaptType, y=Resp, fill = AdaptType),stat = "identity") +
      geom_errorbar(aes(x = AdaptType, ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(y = yTitleStr, fill = NULL, x = NULL) + # x = "Condition" ,
      # geom_signif(data = annotation_df,
      #             aes(xmin = start, xmax = end, annotations = label, y_position = y),
      #             textsize = 6, vjust = -0.2, manual = TRUE) + 
      facet_wrap(~ Word, ncol = 4, labeller = labeller(Word = Word.labs)) +
      scale_fill_manual(values=c("grey", "skyblue4", "goldenrod3"), 
                        labels=c("Baseline" = "Baseline", 
                                 "pw12_ang/pw34_fea" = "AdaptCond1: PW1/PW2 ang | PW3/PW4 fea",
                                 "pw12_fea/pw34_ang" = "AdaptCond2: PW1/PW2 fea | PW3/PW4 ang")) + 
      coord_cartesian(ylim=c(0.2, 0.7)) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=18),
            axis.title=element_text(size=18), 
            axis.text.x=element_blank(), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 18), 
            axis.ticks.x=element_blank(),
            strip.text.x = element_text(size = 18), 
            legend.text = element_text(size = 18), 
            legend.position='bottom') + 
      scale_y_continuous(breaks = c(0.3, 0.4, 0.6))


p2

#save plot
ggsave(filename, width = 15, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename)


#----------------------------------------------#
#                Combining plots               #
#----------------------------------------------#

#-----------------------------------
# Now we arrange these plots
filename = paste0("plots/03_plots_combined.png")


p <- plot_grid(p1, p2, nrow = 2, rel_widths = c(1,1), labels = c("A", "B"))
ggsave(filename, width = 17, height = 17, dpi =300)



##End of Script