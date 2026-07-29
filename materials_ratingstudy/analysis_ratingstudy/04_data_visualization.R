#############################################################################################
### file: 04_data_visualization.R
### author: Christine Nussbaum
### contact: christine.nussbaum@uni-jena.de
### date: 16.2021

# clear directory
rm(list=ls())



# set working directory

setwd("~/Arbeit/Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # dienstlicher PC
setwd("~/Christine/Arbeit/01_Promotion/02_Experiment1_EEGEmotion/materials_ratingstudy/analysis_ratingstudy") # privater PC


###2: load relevant packages
library(tidyverse)  # version 1.3.0

###2: load relevant functions
source("functions/mySummary.R")


###4: load data: ClassTask.RData
load(file = "input/ratingdata_prep.RData")


##################################################################################################################
# aggregate dataset, for Emo only 

D$Int <- paste0(D$Int, "%")
D$Int <- factor(D$Int,
                levels = c("100%","75%","50%","25%"))

B <- mySummary(D, Emo_ACC, Subject, Emo, Int) # n subjects * 7 emotions * 4 intensities

A <- mySummary(B, Emo_ACC, Emo, Int) # 7 emotions * 4 intensities



##################################################################################################################


##################################################################################################################
# First plot

yTitleStr = "Proportion of Correct Classifications"
xTitleStr = "Morph Level"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 20)")

filename = paste0("plots/01_EMO_prop_correct.png")


p<-(ggplot(data= A, aes(x = Int, y=Emo_ACC)) +
      geom_jitter(data= B, aes(x = Int, y=Emo_ACC),  shape = 16, position = position_jitter(0.1), color="grey")+
      geom_point(color = "black", shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (Emo_ACC-CI), ymax = (Emo_ACC+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      facet_wrap(~ Emo, ncol = 4) +
      geom_hline(yintercept = 0.143, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=16),
            axis.title=element_text(size=16), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 10, height = 6, dpi =300)

remove(yTitleStr, xTitleStr, facetStr, title, filename, p)

##################################################################################################################
# Second Plot
D$Int_RESP <- as.numeric(as.character(D$Int_RESP))
D$Int_RESP <- D$Int_RESP/25

B <- mySummary(D, Int_RESP, Subject, Emo, Int) # n subjects * 7 emotions * 4 intensities
A <- mySummary(B, Int_RESP, Emo, Int) # 7 emotions * 4 intensities


yTitleStr = "Mean Rating of Emotional Intensity (1-4)"
xTitleStr = "Morph Level"
filename = paste0("plots/02_INT_ratings.png")

p<-(ggplot(data= A, aes(x = Int, y=Int_RESP)) +
      geom_jitter(data= B, aes(x = Int, y=Int_RESP),  shape = 16, position = position_jitter(0.1), color="grey")+
      geom_point(color = "black", shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (Int_RESP-CI), ymax = (Int_RESP+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      facet_wrap(~ Emo, ncol = 4) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=16),
            axis.title=element_text(size=16), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,4), breaks = c( 1, 2,3,4)))
ggsave(filename, width = 10, height = 6, dpi =300)

remove(yTitleStr, xTitleStr, facetStr, title, filename, p)

##################################################################################################################
# Third Plot - ML100 only 

D<- D %>% filter(Int =="100%")
B <- mySummary(D, Emo_ACC, Subject, Emo) # n subjects * 7 emotions
A <- mySummary(B, Emo_ACC, Emo) # 7 emotions

yTitleStr = "Proportion of Correct Classifications"
xTitleStr = "Emotion"


filename = paste0("plots/03_EMO_prop_correct.png")


p<-(ggplot(data= A, aes(x = Emo, y=Emo_ACC)) +
      geom_jitter(data= B, aes(x = Emo, y=Emo_ACC),  shape = 16, position = position_jitter(0.1), color="grey")+
      geom_point(color = "black", shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (Emo_ACC-CI), ymax = (Emo_ACC+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      geom_hline(yintercept = 0.143, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 12, height = 4, dpi =300)

remove(yTitleStr, xTitleStr, filename, p)


##################################################################################################################
# Fourth Plot - ML100 only 

B <- mySummary(D, Int_RESP, Subject, Emo) # n subjects * 7 emotions * 4 intensities
A <- mySummary(B, Int_RESP, Emo) # 7 emotions * 4 intensities


yTitleStr = "Mean Rating of Emotional Intensity (1-4)"
xTitleStr = "Emotion"


filename = paste0("plots/04_INT_ratings.png")


p<-(ggplot(data= A, aes(x = Emo, y=Int_RESP)) +
      geom_jitter(data= B, aes(x = Emo, y=Int_RESP),  shape = 16, position = position_jitter(0.1), color="grey")+
      geom_point(color = "black", shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (Int_RESP-CI), ymax = (Int_RESP+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      geom_hline(yintercept = 0.143, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(2,4), breaks = c( 2,3,4)))
ggsave(filename, width = 12, height = 4, dpi =300)

remove(yTitleStr, xTitleStr, filename, p)

rm(A, B, D)

##################################################################################################################
# Fifth Plot - confusion data

# load complete data again: 

load(file = "input/ratingdata_prep.RData")

D$Int <- paste0(D$Int, "%")
D$Int <- factor(D$Int,
                levels = c("100%","75%","50%","25%"))
# remove this on missing case: 
D <- D %>% filter(!is.na(Emo_RESP))

# calculate confusion data: 

B <- D %>% group_by(Subject, Int, Emo, Emo_RESP,  .drop = FALSE) %>% summarise(N = length(Emo_ACC))

B$P <- B$N /32 # Max is always 32 (8 Speakers * 4 words)

A <- B %>% group_by(Emo, Int, Emo_RESP) %>% summarise(P = mean(P))

A$P <- round(A$P *100, 0)

yTitleStr = "Mean Proportion of Responses in %"
xTitleStr = "Emotion"


filename = paste0("plots/05b_confusion_ratings.png")


# as heatmap
p <- (ggplot(data = A, aes(x=Emo, y=Emo_RESP, fill=P)) + 
  geom_tile(show.legend = FALSE) +
  labs(x = xTitleStr , y = yTitleStr) +
  scale_fill_gradient(low = "white", high = "blue") +
  facet_wrap(~ Int, ncol = 2) +
  theme(panel.background = element_rect(fill = 'white'),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12), 
          axis.text.x = element_text(color = "black", size = 10), 
          axis.text.y = element_text(color = "black", size = 10), 
          strip.text.x = element_text(size = 14)) + 
  geom_text(aes(label=round(P,2)), color = "black", size = 4))

ggsave(filename, width = 6, height = 6, dpi =300)

remove(yTitleStr, xTitleStr, filename, p)