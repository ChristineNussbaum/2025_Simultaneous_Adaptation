# set working directory
setwd()

imageDirectory<-"C:/Users/allgremote/Desktop/full_morphs_rating/analysis/plots"

# load required packages

library("dplyr")
library("ggplot2")
library("stringr")
library("RColorBrewer")
library("tidyr")

# load data

load(file="input/20Vpn_aggregated.RData")
load(file="input/full_morphs_raw_20Vpn_tidy.RData")

######################################################################################################

# Plots with tML = 100 only

# (1) Boxplot
p <- ggplot (data = D_subj[D_subj$tML =="100",],aes(y = mEmo_Acc, x = emo) ) +
    geom_boxplot() + 
    geom_point() +
    labs( x = "Emotion", y = "Mean Accuracy",
        title ="Full Morphs (Morphlevel 100 only)")
    

p

imageFile <- file.path(imageDirectory, "boxplot_tML100only.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)	


# (2) ViolinPlot

p <- ggplot (data = D_subj[D_subj$tML =="100",], aes(y = mEmo_Acc, x = emo) ) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=95, size=4) + 
    labs( x = "Emotion", y = "Mean Accuracy",
        title ="Full Morphs (Morphlevel 100 only)")


p

imageFile <- file.path(imageDirectory, "violinplot_tML100only.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)	

# (3) plot split for different speakers

D_sp <- D %>% filter(tML =="100") %>% group_by(emo, speaker) %>% summarise(mEmo_Acc = mean(Emo_Acc),
                                                                               N = length(Emo_Acc),
                                                                               sdEmoAcc = sd(Emo_Acc))


p <- ggplot (data = D_sp, aes(y = mEmo_Acc, x = emo, fill=speaker)) +
     geom_col() + 
     facet_grid(rows = vars(speaker)) +
    scale_fill_brewer(palette="Dark2") +
     labs( x = "Emotion", y = "Mean Accuracy per Speaker",
      title ="Full Morphs (Morphlevel 100 only)")
  

p

###########################################################################################

# Plots with all Morphlevels (tML)

# reorder Factor tML (quick and dirty)
D_subj$tML = as.character(D_subj$tML)
D_subj$tML = as.numeric(D_subj$tML)
D_subj$tML = as.factor(D_subj$tML)

D_all$tML = as.character(D_all$tML)
D_all$tML = as.numeric(D_all$tML)
D_all$tML = as.factor(D_all$tML)

# (1) Barplot

#https://mycolor.space/?hex=%23D09191&sub=1

p <- ggplot (data = D_all,aes(y = mEmo_Acc, x = emo, fill= tML) ) +
    geom_col(position=position_dodge()) + 
    geom_errorbar(aes(ymin=mEmo_Acc-1.96*seEmoAcc,ymax=mEmo_Acc+1.96*seEmoAcc),position=position_dodge(.9), width=.3) +
  scale_fill_manual(values=c("#d09191", "#b47f92", "#91718d", "#6c6482")) + 
    geom_hline(yintercept = 0.14, linetype = 4)  + 
    labs( x = "Emotion", y = "Mean Accuracy",
        title ="Accuracy per Emotion and Morphlevel")


p

imageFile <- file.path(imageDirectory, "barplot_allTML.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)	


###################### With the raw data ##########################



#(1) confusion matrix

#100% only
cm <- D[D$tML =="100",] %>% group_by(emo, Emo_Resp, tML) %>% summarise(N = length(Emo_Acc))



# in total 128 trials per Emotion and Subject, here with 20 Subjects: 128 * 20 = 2560 Trials per Emotion

cm$P <- cm$N /640 # in Prozent


# as barplot
p <- ggplot(data=cm, aes(y= P, x = emo, fill=Emo_Resp)) +
    geom_col() +
  scale_fill_manual(values=c("#2e1aca", "#ca00a1", "#ff0070", "#ff7049", "#ffba41", "#f9f871", "#00d3a7")) +
    labs( x = "Emotion", y = "Proportion of Answers",
        title ="Confusion data of 100%-Morphs only")


p

imageFile <- file.path(imageDirectory, "confusion_barplot100.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)

# as heatmap
p <- ggplot(data = cm, aes(x=emo, y=Emo_Resp, fill=P)) + 
     geom_tile() +
     labs( x = "Emotion", y = "Proportion of Answers",
        title ="Confusion data of 100%-Morphs only") +
    scale_fill_gradient(low = "white", high = "darkblue") +
    theme(panel.background = element_rect(fill = 'white')) +
    geom_text(aes(label=round(P,2)), color = "black", size = 4)

p

imageFile <- file.path(imageDirectory, "confusion_matrix100.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)

############################################################################################

#50% only
cm <- D[D$tML =="50",] %>% group_by(emo, Emo_Resp, tML) %>% summarise(N = length(Emo_Acc))



# in total 128 trials per Emotion and Subject, here with 20 Subjects: 128 * 20 = 2560 Trials per Emotion

cm$P <- cm$N /640 # in Prozent


# as barplot
p <- ggplot(data=cm, aes(y= P, x = emo, fill=Emo_Resp)) +
  geom_col() +
  scale_fill_manual(values=c("#2e1aca", "#ca00a1", "#ff0070", "#ff7049", "#ffba41", "#f9f871", "#00d3a7")) +
  labs( x = "Emotion", y = "Proportion of Answers",
        title ="Confusion data of 50%-Morphs only")


p

imageFile <- file.path(imageDirectory, "confusion_barplot50.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)

# as heatmap
p <- ggplot(data = cm, aes(x=emo, y=Emo_Resp, fill=P)) + 
  geom_tile() +
  labs( x = "Emotion", y = "Proportion of Answers",
        title ="Confusion data of 50%-Morphs only") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme(panel.background = element_rect(fill = 'white')) +
  geom_text(aes(label=round(P,2)), color = "black", size = 4)

p

imageFile <- file.path(imageDirectory, "confusion_matrix50.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)

############################################################################################

#25% only
cm <- D[D$tML =="25",] %>% group_by(emo, Emo_Resp, tML) %>% summarise(N = length(Emo_Acc))



# in total 128 trials per Emotion and Subject, here with 20 Subjects: 128 * 20 = 2560 Trials per Emotion

cm$P <- cm$N /640 # in Prozent


# as barplot
p <- ggplot(data=cm, aes(y= P, x = emo, fill=Emo_Resp)) +
  geom_col() +
  scale_fill_manual(values=c("#2e1aca", "#ca00a1", "#ff0070", "#ff7049", "#ffba41", "#f9f871", "#00d3a7")) +
  labs( x = "Emotion", y = "Proportion of Answers",
        title ="Confusion data of 25%-Morphs only")


p

imageFile <- file.path(imageDirectory, "confusion_barplot25.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)

# as heatmap
p <- ggplot(data = cm, aes(x=emo, y=Emo_Resp, fill=P)) + 
  geom_tile() +
  labs( x = "Emotion", y = "Proportion of Answers",
        title ="Confusion data of 25%-Morphs only") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme(panel.background = element_rect(fill = 'white')) +
  geom_text(aes(label=round(P,2)), color = "black", size = 4)

p

imageFile <- file.path(imageDirectory, "confusion_matrix25.png")
ggsave(imageFile, width = 12, height = 8, dpi = 400)

