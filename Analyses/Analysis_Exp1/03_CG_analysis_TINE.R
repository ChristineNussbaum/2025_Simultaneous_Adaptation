##########################################################################
## File: 03_CG_analysis.R
## Data Preparatation for Emotion-Adaptation-Experiment in Voices, BA Berges
# author: Christine Nussbaum und Dorothea Berges
# date 02/2022, revised 05/2024

# clear directory
rm(list=ls())

#set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library("tidyverse")
library("ez")
library("effectsize")
#library("ggplot2")

# load relevant functions
source("functions/mySummary.R") 
source("functions/fitCumulativeGaussian.R")
source("functions/tracedEzOut.R")


#---------------------------------------------------------------------------------
#get the CG input data:

load(file ="input/CG_input_data.RData")


#-------------------------------------------------------------------------------#
#                     CG Sch?tzung f?r jeden Probanden einzeln                  #
#-------------------------------------------------------------------------------#

##################################################################
## declare tML as numerical variable
CG_input$tML <- as.numeric(as.character(CG_input$tML))

#CG input has 40 participants * 2 SpSex * 2 AdaptTypes * 7 tML = 1120 rows


#Fit a Cumulative Gaussian Function (CG) for each Subject x SpSex * AdaptType
CGdata <- myCGfit(CG_input, x = unique(CG_input$tML), Resp, Subject, AdaptType, SpSex)
### There were 50 warnings -> this happens when the algorithm tried to fit numbers that create a NaN in the pnorm()-command
warnings()

#CG_data has 40 participants * 2 SpSex * 2 AdaptTypes = 160

view(CGdata)

##################################################################
## plot values of the fits to identify outlier
summary(CGdata[,4:6]) # yep... some fits went wrong

#PSE - point of subjective equality
dotchart(CGdata$PSE)
hist(CGdata$PSE)
boxplot(CGdata$PSE)
#Note: a PSE higher than 80 or lower than 20 is outside the measured target-range (20-80%) and might be excluded from the analysis

#R2 - fit of the function
dotchart(CGdata$R2)
hist(CGdata$R2)
boxplot(CGdata$R2)
#Note: Ideally, fits should be greater than 80, but there is not clear cutoff, when a fit should be excluded


# inspect subjects that have a PSE > 80 and a R2 < 70
badFitR2 <- CGdata %>% filter(R2 < 0.7)
badFitPSE <- CGdata %>% filter(PSE > 80 | PSE < 20)
badFit <- rbind(badFitR2, badFitPSE)
unique(badFit$Subject) # 4 Subjects have bad fits and should be excluded in a second analysis
rm(badFitR2, badFitPSE)

###remove participants with bad fit
#TINE: ich w?rde die tats?chlich generell rausnehmen. Dann hast du noch 36, das sind immernoch genug und die Daten sind sauberer. 
CGdata <- CGdata %>% filter(!(Subject %in% badFit$Subject))
#now CG_data has 36 participants * 2 SpSex * 2 AdaptTypes = 144

# now check again PSE values and R2
dotchart(CGdata$PSE) # all beautiful
dotchart(CGdata$R2)  # very nice


#-------------------------------------------------------------------------------#
#                     CG Sch?tzung ?ber alle Probanden gemittelt                #
#-------------------------------------------------------------------------------#

##ich mache hier nochmal einen neuen CG_agg_input, weil jetzt ja nur noch 36 Probanden drin sind. Da ist N = 36#
CG_input <- CG_input %>% filter(!(Subject %in% badFit$Subject))
CG_agg_input <- mySummary(CG_input, Resp, AdaptType, SpSex, tML) # input ?ber alle Probanden gemittelt

#Zuerst: wir sagen R wieder, dass tML numerisch ist!
CG_agg_input$tML <- as.numeric(as.character(CG_agg_input$tML))

#Sch?tzung f?r jeden Probanden
CGagg <- myCGfit(CG_agg_input, x = unique(CG_agg_input$tML), Resp, AdaptType, SpSex)


###############################################################################
###############################################################################
# Datenvisualisierung: ?ber alle gemittelt und einzeln

##########################################################################
## Datenvisualisierung: Plots ?ber alle gemittelt

#TINE: ich hab hier mal nen Plot gemacht, kannst du dann einfach noch ?bersetzen bzw anpassen
xTitleStr = 'test voice morph level (Prop. of "angry" in the voice)'
yTitleStr = 'Prop. of "angry" responses'
filename = "plots/CG-tML x AdaptType x SpSex.png"
title = "Cumulative Fits: Emotion AE per adaptor modality - Final data (N = 36)"

# Data sampling: (TINE: das ist der recht komplizierte Teil. Schau da einfach nochmal ins Video, das ist einfach schon sehr komplex)
plot_data <- 
        pmap_df(CGagg[,1:4],
                function(AdaptType, SpSex, PSE, SD) {
                        tibble(AdaptType = AdaptType,
                               SpSex = SpSex,
                               x = seq(20, 80, by = 1),
                               y = pnorm(x, PSE, SD))
                })
# Check: 4 *61 = 244

#TINE: schau einfach ein bisschen durch, dass du halbwegs verstehst, was in dem Code passiert, dann hast du viel geschafft. :)
# und dann kannst du einfach mal probieren, den noch "sch?n" zu machen, also Beschriftungen anpassen, Hintergrund wei? und vllt nicht rot und gr?n als Farben

p<-(ggplot(CG_agg_input, aes(x=tML, y=Resp, colour=SpSex)) + 
            geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
            geom_point(mapping = aes(colour = SpSex, shape = SpSex) ) + 
            labs(x = xTitleStr , y = yTitleStr) + 
            scale_colour_manual(values=c("red", "darkgreen")) + 
            scale_linetype_manual(values=c("solid", "dotdash"))+ 
            scale_size_manual(values=c(2,2)) +  
            scale_shape_manual(values=c(19, 15))+
            scale_y_continuous(limits=c(0,1), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0))  + 
            facet_wrap(~AdaptType)+
            ggtitle(title))+
        geom_line(data = plot_data, aes(colour = SpSex, x = x, y = y)) +
        geom_hline(yintercept = 0.5, lty = 2)
ggsave(filename, width = 12, height = 4, dpi =300)	

remove(p, xTitleStr, yTitleStr, filename, title, plot_data)


##############################################################################
## Datenvisualisierung: Plots f?r die einzelnen Probanden

#CG_input <- mySummary(Adapt, Resp, Subject, AdaptType, SpSex, tML) # Input f?r jeden Probanden einzeln
#view(CG_input)
xTitleStr = 'Morphlevel'
yTitleStr = 'Anteil von w?tend-Antworten'
#filename = "imgs/BAProbandenEinzeln.png"  #TINE: du brauchst den richtigen Unterordner, der hei?t bei dir "plots"
filename = "plots/BAProbandenEinzeln.png"
title = "Einfluss des Geschlechtes auf die Adaptation von Emotionen " 



#TINE: diese kleine Schleife hier hat dir gefehlt: 

plot_data <- 
        pmap_df(CGdata[,1:5],
                function(AdaptType,SpSex, Subject, PSE, SD) {
                        tibble(AdaptType = AdaptType,
                               SpSex = SpSex,
                               Subject = Subject,
                               x = seq(20, 80, by = 1),
                               y = pnorm(x, PSE, SD))
                })
# check: 144 *61 = 8784



p<-(ggplot(CG_input, aes(x=tML, y=Resp, colour=SpSex)) + 
            geom_errorbar(aes(ymin=Resp-SE, ymax=Resp+SE), width=2) +
            geom_point(mapping = aes(colour = SpSex, shape = SpSex) ) + 
            labs(x = xTitleStr , y = yTitleStr) + 
            scale_colour_manual(values=c("red", "darkgreen")) + 
            scale_linetype_manual(values=c("solid", "dotdash"))+ 
            scale_size_manual(values=c(2,2)) +  
            scale_shape_manual(values=c(19, 15))+
            scale_y_continuous(limits=c(0,1), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0))  + 
            facet_wrap(~Subject + AdaptType, ncol = 10)+
            theme(legend.position="none") +
            ggtitle(title)) +
        geom_line(data = plot_data, aes(colour = SpSex, x = x, y = y)) +
        geom_hline(yintercept = 0.5, lty = 2)
ggsave(filename, width = 16, height = 12, dpi =300)	
 

remove(p, xTitleStr, yTitleStr, filename, title, plot_data)

#irgendwie will es nicht speichern
#aber ich kann es anzeigen lassen, aber keine Ahnung, wieso es jetzt nicht funktioniert
#und ich schaffe es nicht die Punkte miteinander zu verbinden, dass es eine sch?ne Kurve gibt



##############################################################################################################################
##############################################################################################################################
## Statistische Analyse
##########################################################################


## ANOVA I: 2 x 2 within subject

# data: CGdata
# dv: PSE
# wid: Subject
# within: AdaptType, SpSex


a<-ezANOVA(data=CGdata, dv=.(PSE), wid=.(Subject), within = .(AdaptType, SpSex), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#TINE: hier sind jetzt ?berall die Daten mit N=40 Probanden drin, Mit N=36 ?ndert sich das jetzt leicht. 

#Output
#$`--- ANOVA RESULTS     ------------------------------------`
#Effect      MSE df1 df2        F     p petasq getasq
#1     (Intercept) 330.0377   1  39 1513.881 0.000  0.975  0.897
#2       AdaptType 466.8654   1  39    0.822 0.370  0.021  0.007
#3           SpSex 415.6114   1  39    1.745 0.194  0.043  0.012
#4 AdaptType:SpSex 264.9676   1  39   10.634 0.002  0.214  0.047

#$`--- SPHERICITY TESTS  ------------------------------------`
#[1] "N/A"

#$`--- FORMATTED RESULTS ------------------------------------`
#Effect                                      Text
#1     (Intercept) F(1, 39) = 1513.881, p < .001, np2 = .975
#2       AdaptType F(1, 39) =    0.822, p = .370, np2 = .021
#3           SpSex F(1, 39) =    1.745, p = .194, np2 = .043
#4 AdaptType:SpSex F(1, 39) =   10.634, p = .002, np2 = .214    * signifikante Interaktion zwischen Geschlecht des Stimulus und dem Typ der Adaptation 
# -> geschlechterspezifische Adaptation

#$`NOTE:`
#[1] "Reporting unadjusted p-values."

capture.output(b, file= "output/ANOVAI.txt")

# Daten visualisieren (nur f?r einen selbst, nicht f?r Berichte oder Publikationen)

ezPlot(data=CGdata, dv=.(PSE), wid=.(Subject), within = .(AdaptType, SpSex), x=SpSex, split=AdaptType)


########################### Post - Hoc Analyse ###########################################################
#TINE: beide Haupteffekte sind nicht signifikant, daher ist es nicht n?tig, die nachzutesten. Es reicht die Interaktion. 

##########################################################################
### PH1: Haupteffekt von AdaptType
##########################################################################

# Daten aggregieren f?r die erste post-hoc Analyse

PH1 <- CGdata %>% group_by(Subject, AdaptType) %>% summarise(PSE = mean(PSE))

# Descriptive Daten auslesen (und anschauen!)

PH1_descriptive <- mySummary(PH1, PSE, AdaptType)

# ins Wide-Format unwandeln 

PH1 <- spread(PH1, AdaptType, PSE)

view(PH1)


### t-Tests rechnen, ACHTUNG: Bonferroni Korrektur beachten

# t-Test 1: AdaptType1

A1_A2 <- t.test(PH1$`w_angry/m_fearful`, PH1$`w_fearful/m_angry`, paired = TRUE)

# t-Test 2: AdaptType2

A2_A1 <- t.test(PH1$`w_fearful/m_angry`, PH1$`w_angry/m_fearful`, paired = TRUE)

#TINE: die beiden Tests sind identisch. (und nicht notwendig ;)

# Ergebnisse wegschreiben
capture.output(PH1_descriptive, A2_A1, A1_A2, file = "output/ph1_AdaptType.txt")

#TINE: seit dem neuen R-update m?ssen die Datens?tze leider mit as.matrix() drumrum abgespeichert werden: 

capture.output(as.matrix(PH1_descriptive), A2_A1, A1_A2, file = "output/ph1_AdaptType.txt")

#Output#######################
#AdaptType           PSE    SD     N    SE    CI
#<chr>             <dbl> <dbl> <int> <dbl> <dbl>
#   1 w_angry/m_fearful  57.4 17.5     40  2.77  5.61
#2 w_fearful/m_angry  54.3  9.55    40  1.51  3.05

#Paired t-test

#data:  PH1$`w_fearful/m_angry` and PH1$`w_angry/m_fearful`
#t = -0.90667, df = 39, p-value = 0.3702
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#   -10.007795   3.812756
#sample estimates:
#   mean of the differences 
#-3.097519 


#Paired t-test

#data:  PH1$`w_angry/m_fearful` and PH1$`w_fearful/m_angry`
#t = 0.90667, df = 39, p-value = 0.3702
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#   -3.812756 10.007795
#sample estimates:
#   mean of the differences 
#3.097519

#########################################################


# Aufr?umen
rm(PH1, PH1_descriptive, A1_A2, A2_A1)


##########################################################################
### PH1: Haupteffekt von SpSex
##########################################################################
#TINE: auch nicht notwendig. ;)

# Daten aggregieren f?r die erste post-hoc Analyse

PH2 <- CGdata %>% group_by(Subject, SpSex) %>% summarise(PSE = mean(PSE))

# Descriptive Daten auslesen (und anschauen!)

PH2_descriptive <- mySummary(PH2, PSE, SpSex)

# ins Wide-Format unwandeln 

PH2 <- spread(PH2, SpSex, PSE)

view(PH2)


### t-Tests rechnen, ACHTUNG: Bonferroni Korrektur beachten

# t-Test 1: SpSex1

fm <- t.test(PH2$f, PH2$m, paired = TRUE)

# t-Test 2: SpSex2

mf <- t.test(PH2$m, PH2$f, paired = TRUE) # TINE: auch wieder die Exakt gleiche Analyse. 

# Ergebnisse wegschreiben
capture.output(PH2_descriptive, fm, mf, file = "output/ph2_SpSex.txt")

#Output#######################
# A tibble: 2 x 6
#SpSex   PSE    SD     N    SE    CI
#<chr> <dbl> <dbl> <int> <dbl> <dbl>
#   1 f      58.0 17.3     40  2.73  5.52
#2 m      53.8  8.63    40  1.36  2.76

#Paired t-test

#data:  PH2$f and PH2$m
#t = 1.321, df = 39, p-value = 0.1942
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#   -2.261809 10.778061
#sample estimates:
#   mean of the differences 
#4.258126 

#Paired t-test

#data:  PH2$m and PH2$f
#t = -1.321, df = 39, p-value = 0.1942
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#   -10.778061   2.261809
#sample estimates:
#   mean of the differences 
#-4.258126 

#########################################################

# Aufr?umen
rm(PH2, PH2_descriptive, fm, mf)



##########################################################################
### PH3: Interaktion von AdaptType und SpSex
##########################################################################

# Vorgehen: einen Faktor konstant halten, anderen testen
# abh?ngig von den Hypothesen und Fragestellungen

#konstanter Faktor: SpSex, testen: AdaptType

#TINE: ich w?rde es genau anders rum machen. AdaptType konstant halten, und SpSex testen. 

# Daten aggregieren

PH3 <- CGdata %>% group_by(Subject, SpSex, AdaptType) %>% summarise(PSE = mean(PSE))


# Descriptive Daten auslesen (f?r alle gemeinsam)

PH3_descriptive <- mySummary(PH3, PSE, SpSex, AdaptType)

# ins Wide-Format unwandeln (zumindest teilweise)

PH3 <- spread(PH3, AdaptType, PSE)


### t-Tests rechnen, vorher immer ein Geschlecht ausw?hlen

PH3f<- PH3 %>% filter(SpSex == "f")
AdaptType1 <- t.test(PH3f$`w_fearful/m_angry`, PH3f$`w_angry/m_fearful`, paired = TRUE)

PH3m<- PH3 %>% filter(SpSex == "m")
AdaptType2 <- t.test(PH3m$`w_fearful/m_angry`, PH3m$`w_angry/m_fearful`, paired = TRUE)


##################################################
## Erwarteter Output (Ausschnitt):
# A tibble: 4 x 7
#SpSex AdaptType           PSE    SD     N    SE    CI
#<chr> <chr>             <dbl> <dbl> <int> <dbl> <dbl>
#1 f     w_angry/m_fearful  63.8 34.6     40  5.48 11.1 
#2 f     w_fearful/m_angry  52.3  9.80    40  1.55  3.13
#3 m     w_angry/m_fearful  51.1  7.88    40  1.25  2.52
#4 m     w_fearful/m_angry  56.4 11.0     40  1.73  3.51
#
#Paired t-test
#
#data:  PH3f$`w_fearful/m_angry` and PH3f$`w_angry/m_fearful`
#t = -1.9445, df = 39, p-value = 0.05907
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#   -23.442727   0.462133
#sample estimates:
#   mean of the differences 
#-11.4903 


#Paired t-test

#data:  PH3m$`w_fearful/m_angry` and PH3m$`w_angry/m_fearful`
#t = 4.0935, df = 39, p-value = 0.0002068
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#   2.678763 7.911753
#sample estimates:
#   mean of the differences 
#5.295258 
##################################################

# Ergebnisse wegschreiben
capture.output(as.matrix(PH3_descriptive), AdaptType1, AdaptType2, file = "output/ph3_Int_AdaptType_SpSex.txt")

# Aufr?umen
rm(PH3, PH3_descriptive, PH3f, PH3m, AdaptType1, AdaptType2)

