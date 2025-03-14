##########################################################################
## File: 04_Exp3_data_visualization.R
## Data Visualization for Exp 3: Adaptation of Emotion - pseudowords
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

load(file ="input/Exp3_without_omissions.RData")


#-------------------------------------------------------------------------------#
#                            1 - Plot simple response data                      #
#-------------------------------------------------------------------------------# 

#average raw data (for both Adapt and Baseline Blocks)
E3_Adapt_plot <- mySummary(E3_Adapt, Resp, Participant, tML, SpSex, Word, AdaptType)
E3_Bline_plot <- mySummary(E3_Bline, Resp, Participant, tML, SpSex, Word, Block)
E3_Bline_plot <- E3_Bline_plot %>% rename(AdaptType= Block)
E3_plot <- rbind(E3_Adapt_plot, E3_Bline_plot)
rm(E3_Adapt_plot, E3_Bline_plot)
E3_plot_agg<- mySummary(E3_plot, Resp, tML,  SpSex, Word, AdaptType)



#[1a] Response per tML, Word, SpSex and AdaptType
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per Word and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 47)")

filename = paste0("plots/01a_Resp_Exp3.png")

#Plot
p<-(ggplot(data= E3_plot_agg, aes(x = tML, y=Resp, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      facet_wrap(~ Word + SpSex, ncol = 4) +
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
ggsave(filename, width = 12, height = 6, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#[1b] Response per tML, Word, SpSex and AdaptType (plotted the other way around and without Baseline)
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per Word and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 47)")

filename = paste0("plots/01b_Resp_Exp3.png")

#Plot
p<-(ggplot(data= E3_plot_agg[E3_plot_agg$AdaptType != "Baseline",], aes(x = tML, y=Resp, color = Word, group=Word)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker Sex", title = title) +
      facet_wrap(~ SpSex + AdaptType, ncol = 4) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "red","darkblue", "blue")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 12, height = 6, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)



#[1c] Plot the interaction averaged across tML
yTitleStr = "Proportion of angry responses"
xTitleStr =  "AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N = 47)")
filename = paste0("plots/01c_Resp_Exp3.png")

E3_plot_agg2<- mySummary(E3_plot, Resp, Word, SpSex, AdaptType)


#Plot
p<-(ggplot(data= E3_plot_agg2[E3_plot_agg2$AdaptType != "Baseline",], aes(x = AdaptType, y=Resp, color = Word, group=Word)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred","red", "darkblue", "blue")) + 
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
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N = 47)")
filename = paste0("plots/01d_Resp_Exp3.png")

E3_plot_agg2<- mySummary(E3_plot, Resp, Word, SpSex, AdaptType)


#Plot
p<-(ggplot(data= E3_plot_agg2, aes(x = Word, y=Resp, color = AdaptType, group=AdaptType)) +
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
rm(E3_Adapt, E3_Bline, E3_plot, E3_plot_agg, E3_plot_agg2)

#-------------------------------------------------------------------------------#
#                                2 - Plot CG data                               #
#-------------------------------------------------------------------------------# 

#load the data of the CG estimates
load(file="input/Exp3_CG_estimates.RData")

#we dont need badfit
rm(badFit)


#[2a] Plot the interaction, using the CG data, averaged across participants


#Beschriftung
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 41)")

filename = paste0("plots/02a_CG_Exp3.png")


# Data sampling: 
plot_data <- 
  pmap_df(CGagg[,1:5],
          function(AdaptType, PW_Group, SpSex, PSE, SD) {
            tibble(AdaptType = AdaptType,
                   PW_Group = PW_Group,
                   SpSex = SpSex,
                   x = seq(20, 80, by = 1),
                   y = pnorm(x, PSE, SD))
          })
# Check: 12 *61 = 732



#plot
p<-(ggplot(CG_agg_input, aes(x=tML, y=Resp, colour=AdaptType)) + 
      geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
      geom_point(mapping = aes(colour = AdaptType) ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) + 
      scale_colour_manual(values=c("grey", "darkorange", "darkgreen", "darkorange", "darkgreen")) +
      facet_wrap(~PW_Group + SpSex)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      geom_line(data = plot_data, aes(colour = AdaptType, x = x, y = y)) +
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
 ggsave(filename, width = 10, height =  8, dpi =300)	



remove(p, xTitleStr, yTitleStr, filename, title)



#[2b] Plot the interaction, using the CG data, averaged across participants, the other way around and without Baseline


#Beschriftung
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  " splitted per SpSex and AdaptType"
title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 28)")

filename = paste0("plots/02b_CG_Exp3.png")



#plot
p<-(ggplot(CG_agg_input[CG_agg_input$AdaptType != "Baseline",], aes(x=tML, y=Resp, colour=PW_Group)) + 
      geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
      geom_point(mapping = aes(colour = PW_Group) ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) + 
      scale_colour_manual(values=c("darkred", "darkblue", "darkred", "darkblue")) + 
      facet_wrap(~ SpSex + AdaptType)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      geom_line(data = plot_data[plot_data$AdaptType!="Baseline",], aes(colour = PW_Group, x = x, y = y)) +
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 10, height = 8, dpi =300)	



remove(p, xTitleStr, yTitleStr, filename, title, plot_data)

rm(CG_agg_input, CG_input, CGagg, CGdata)


#-------------------------------------------------------------------------------#
#                  3 - Plot data of logistic regression                         #
#-------------------------------------------------------------------------------#

#load the adaptation data
load(file ="input/Exp3_without_omissions.RData")

rm(E3_Bline)

#load the model for female speakers only
load(file= "input/E3_GLMs.RData")

# extract fitted values
E3_Adapt$fitted <- fitted(m)

#remove the model data
rm(m_test, m) 

#average data
E3_Adapt_plot <- mySummary(E3_Adapt, fitted, Participant, tML, Word, SpSex, AdaptType)
E3_Adapt_plot_agg <- mySummary(E3_Adapt_plot, fitted, tML, Word, SpSex, AdaptType)


#[3a] Response per tML, SpSex and AdaptType - without Baseline
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 47)")

filename = paste0("plots/03a_Resp_Exp3.png")

#Plot
p<-(ggplot(data= E3_Adapt_plot_agg, aes(x = tML, y=fitted, color = AdaptType, group=AdaptType)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (fitted-CI), ymax = (fitted+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Condition", title = title) +
      facet_wrap(~ Word + SpSex, ncol = 4) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkorange", "darkgreen")) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 12, height = 6, dpi =300)

#keep environment tidy
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)


#[3b] Response per tML, SpSex and AdaptType (plotted the other way around)
yTitleStr = "Proportion of angry responses"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 43)")

filename = paste0("plots/03b_Resp_Exp3.png")

#Plot
p<-(ggplot(data= E3_Adapt_plot_agg, aes(x = tML, y=fitted, color = Word, group=Word)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (fitted-CI), ymax = (fitted+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) +
      facet_wrap(~ SpSex + AdaptType, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "red","darkblue", "blue")) + 
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

filename = paste0("plots/03c_Resp_Exp3.png")

#for the original response info: 
E3_Adapt_plot_resp <- mySummary(E3_Adapt, Resp, Participant, tML, Word, SpSex, AdaptType)

#Plot
p<-(ggplot(data= E3_Adapt_plot, aes(x = tML, y=fitted, color = Word, group=Word)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (fitted-CI), ymax = (fitted+CI)), width = 0.1 ) + 
      geom_point(data= E3_Adapt_plot_resp, aes(x = tML, y=Resp, group=SpSex), color = "grey") +
      labs(x = xTitleStr , y = yTitleStr, color = "Speaker ID", title = title) +
      facet_wrap(~Participant + AdaptType + SpSex, ncol = 16)+
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      scale_colour_manual(values=c("darkred", "red","darkblue", "blue")) + 
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