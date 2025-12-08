##########################################################################
## File: 03_Exp2_data_visualization.R
## Data Analysis for Exp 2: Adaptation of Emotion - speaker identities
# author: Christine Nussbaum und Dorothea Berges
# date 02/2022, revised 11/2025

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

load(file ="input/Exp2_without_omissions.RData")


#-------------------------------------------------------------------------------#
#        1 - Plot response data, for each tML - females only                    #
#-------------------------------------------------------------------------------# 

#average raw data (for both Adapt and Baseline Blocks)
E2_Adapt_plot <- mySummary(E2_Adapt[E2_Adapt$SpSex == "f",], Resp, Participant, tML, SpID, AdaptType)
E2_Bline_plot <- mySummary(E2_Bline[E2_Bline$SpSex == "f",], Resp, Participant, tML, SpID, Block)
E2_Bline_plot <- E2_Bline_plot %>% rename(AdaptType= Block)
E2_plot <- rbind(E2_Adapt_plot, E2_Bline_plot)
rm(E2_Adapt_plot, E2_Bline_plot)
E2_plot_agg<- mySummary(E2_plot, Resp, tML,  SpID, AdaptType)


#[1a] Response per tML, SpID and AdaptType , females only
yTitleStr = "Proportion of angry responses"
xTitleStr = "Target Morphlevel (tML)"
facetStr =  " splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr)

filename = paste0("plots/01a_Resp_Exp2_female.png")

# New facet label names
SpID.labs <- c("F1", "F2")
names(SpID.labs) <- c("f1", "f2")

#Plot
p1<-(ggplot(data= E2_plot_agg, aes(x = tML, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) + #, title = title
      facet_wrap(~ SpID, ncol = 2, labeller = labeller(SpID = SpID.labs)) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("grey", "darkred", "chocolate4")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14), 
            legend.position = "none") + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename)


E2_plot_f <- E2_plot
E2_plot_agg_f <- E2_plot_agg

#-------------------------------------------------------------------------------#
#        1b - Plot response data, for each tML - males only                    #
#-------------------------------------------------------------------------------# 

#average raw data (for both Adapt and Baseline Blocks)
E2_Adapt_plot <- mySummary(E2_Adapt[E2_Adapt$SpSex == "m",], Resp, Participant, tML, SpID, AdaptType)
E2_Bline_plot <- mySummary(E2_Bline[E2_Bline$SpSex == "m",], Resp, Participant, tML, SpID, Block)
E2_Bline_plot <- E2_Bline_plot %>% rename(AdaptType= Block)
E2_plot <- rbind(E2_Adapt_plot, E2_Bline_plot)
rm(E2_Adapt_plot, E2_Bline_plot)
E2_plot_agg<- mySummary(E2_plot, Resp, tML,  SpID, AdaptType)


#[1a] Response per tML, SpID and AdaptType , males only
yTitleStr = "Proportion of angry responses"
xTitleStr = "Target Morphlevel (tML)"
facetStr =  " splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr)

filename = paste0("plots/01b_Resp_Exp2_male.png")

# New facet label names 
SpID.labs <- c("M3", "M4")
names(SpID.labs) <- c("m3", "m4")

#Plot
p1b<-(ggplot(data= E2_plot_agg, aes(x = tML, y=Resp, color = AdaptType, group=AdaptType)) +
       geom_point() +
       geom_line() + 
       geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
       labs(x = xTitleStr , y = yTitleStr) + #, title = title
       facet_wrap(~ SpID, ncol = 2, labeller = labeller(SpID = SpID.labs)) +
       geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
       scale_colour_manual(values=c("grey", "purple", "darkblue")) + 
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             axis.text=element_text(size=14),
             axis.title=element_text(size=14), 
             axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
             axis.text.y = element_text(color = "black", size = 14), 
             strip.text.x = element_text(size = 14), 
             legend.position = "none") + 
       scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename)


E2_plot_m <- E2_plot
E2_plot_agg_m <- E2_plot_agg



#-------------------------------------------------------------------------------#
#                      2a - Barplots, female                                     #
#-------------------------------------------------------------------------------# 

E2_plot_agg2<- mySummary(E2_plot_f, Resp, Participant, SpID, AdaptType)
E2_plot_agg2<- mySummary(E2_plot_agg2, Resp, SpID, AdaptType)


yTitleStr = "Proportion of angry responses"

filename = paste0("plots/0a_Resp_Barplot_female.png")

# New facet label names
SpID.labs <- c("F1", "F2")
names(SpID.labs) <- c("f1", "f2")

#add significance values
annotation_df <- data.frame(
  SpID= c("f1","f1","f1", "f2", "f2","f2"),
  start = c("f1_ang/f2_fea", "Baseline", "Baseline","f1_ang/f2_fea", "Baseline", "Baseline"),
  end = c("f1_fea/f2_ang", "f1_ang/f2_fea", "f1_fea/f2_ang", "f1_fea/f2_ang", "f1_ang/f2_fea", "f1_fea/f2_ang"),
  y = c(0.55, 0.6, 0.65, 0.55, 0.6, 0.65),
  label = c("n.s.", "n.s.","n.s.","**","*","n.s.")
)

p2<-ggplot(data= E2_plot_agg2) +
      geom_bar(aes(x = AdaptType, y=Resp, fill = AdaptType),stat = "identity") +
      geom_errorbar(aes(x = AdaptType, ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(y = yTitleStr, fill = NULL, x = NULL) + # x = "Condition" ,
      geom_signif(data = annotation_df,
                  aes(xmin = start, xmax = end, annotations = label, y_position = y),
                  textsize = 6, vjust = -0.2, manual = TRUE) + 
      facet_wrap(~ SpID, ncol = 2, labeller = labeller(SpID = SpID.labs)) +
      scale_fill_manual(values=c("grey", "darkred", "chocolate4"), 
                        labels=c("Baseline" = "Baseline", 
                                 "f1_ang/f2_fea" = "AdaptCond1: F1 ang | F2 fea",
                                 "f1_fea/f2_ang" = "AdaptCond2: F1 fea | F2 ang")) + 
      coord_cartesian(ylim=c(0.2, 0.7)) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x=element_blank(), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            axis.ticks.x=element_blank(),
            strip.text.x = element_text(size = 14), 
            legend.text = element_text(size = 14), 
            legend.position='bottom') + 
      scale_y_continuous(breaks = c(0.3, 0.4, 0.6))


p2

#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename)

#-------------------------------------------------------------------------------#
#                      2b - Barplots, male                                     #
#-------------------------------------------------------------------------------# 

E2_plot_agg2<- mySummary(E2_plot_m, Resp, Participant, SpID, AdaptType)
E2_plot_agg2<- mySummary(E2_plot_agg2, Resp, SpID, AdaptType)


yTitleStr = "Proportion of angry responses"

filename = paste0("plots/0a_Resp_Barplot_male.png")


#add significance values
annotation_df <- data.frame(
  SpID= c("m3","m3","m3", "m4", "m4","m4"),
  start = c("m3_ang/m4_fea", "Baseline", "Baseline","m3_ang/m4_fea", "Baseline", "Baseline"),
  end = c("m3_fea/m4_ang", "m3_ang/m4_fea", "m3_fea/m4_ang", "m3_fea/m4_ang", "m3_ang/m4_fea", "m3_fea/m4_ang"),
  y = c(0.36, 0.4, 0.44, 0.62, 0.66, 0.7),
  label = c("***", "n.s.","**","***","n.s.","**")
)

# New facet label names 
SpID.labs <- c("M3", "M4")
names(SpID.labs) <- c("m3", "m4")

p2b<-ggplot(data= E2_plot_agg2) +
  geom_bar(aes(x = AdaptType, y=Resp, fill = AdaptType),stat = "identity") +
  geom_errorbar(aes(x = AdaptType, ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
  labs(y = yTitleStr, fill = NULL, x = NULL) + # x = "Condition" ,
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 6, vjust = -0.2, manual = TRUE) + 
  facet_wrap(~ SpID, ncol = 2, labeller = labeller(SpID = SpID.labs)) +
  scale_fill_manual(values=c("grey", "purple", "darkblue"), 
                    labels=c("Baseline" = "Baseline", 
                             "m3_ang/m4_fea" = "AdaptCond3: M3 ang | M4 fea",
                             "m3_fea/m4_ang" = "AdaptCond4: M3 fea | M4 ang")) + 
  coord_cartesian(ylim=c(0.2, 0.7)) +
  geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14), 
        axis.text.x=element_blank(), # angle = 45, hjust = 1.2, vjust =  1.2
        axis.text.y = element_text(color = "black", size = 14), 
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14), 
        legend.text = element_text(size = 14), 
        legend.position='bottom') + 
  scale_y_continuous(breaks = c(0.3, 0.4, 0.6))


p2b

#abspeichern
ggsave(filename, width = 10, height = 4, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename)


#----------------------------------------------#
#                Combining plots               #
#----------------------------------------------#

#-----------------------------------
# Now we arrange these plots
filename = paste0("plots/03_plots_combined.png")


p <- plot_grid(p1, p1b, p2, p2b, nrow = 2, rel_widths = c(1,1), labels = c("A", "B", "C", "D"))
ggsave(filename, width = 17, height = 17, dpi =300)



##End of Script