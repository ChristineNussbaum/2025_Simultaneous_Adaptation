##########################################################################
## File: 02_data_visualization.R
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
#get the CG input data:

load(file ="input/CG_input_data.RData")



#-------------------------------------------------------------------------------
# plot average data
# DORO: ich habe die Beschriftungen angepasst, weil meine BA ja auch deutsch ist
#ist noch nicht perfekt, gerade bei der Beschriftung f?rs Geschlecht h?nge ich etwas, wo ich das anpassen kann

#TINE: die einfachste Variante ist es, die Faktoren direkt nochmal umzubenennen: 
CG_agg_input$SpSex <- recode(CG_agg_input$SpSex , m = "m?nnlich", f = "weiblich")
#TINE: das gleiche kannst du ggf auch f?r AdaptType machen

#Beschriftung
yTitleStr = "Anteil der w?tend-Antworten"
xTitleStr = "Morphlevel"
facetStr =  "splitted per SpSex and AdaptType"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, facetStr, " (N = 12)")

filename = paste0("plots/01_CG_rough_plots.png")


#Plot
p<-(ggplot(data= CG_agg_input, aes(x = tML, y=Resp, color = SpSex, group=SpSex)) +
      geom_point() +
      geom_line() + 
      geom_errorbar(aes(ymin = (Resp-CI), ymax = (Resp+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr, color = "Sprechergeschlecht") +       #TINE: hier habe ich auch was angepasst
      facet_wrap(~ AdaptType, ncol = 2) +
      geom_hline(yintercept = 0.5, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
#abspeichern
ggsave(filename, width = 12, height = 4, dpi =300)

#aufr?umen
remove(yTitleStr, xTitleStr, facetStr, title, filename, p)
