####################################################################################
# Author: Verena Skuk
# File: DFG-Personality\04_Experiments\E1_analysis\functions\loadPTKDataDFGRatingE1
# 
####################################################################################

library(stringr)  # für datenaufbereitung
library(readxl)

loadPTKSurveyDataEmpraSoSe <- function(relPathDirFile, REVERSEITEMS = FALSE) {
  
    #relPathDirFile = "./input/data.xlsx"
    dfQuestRaw <- read_xlsx(relPathDirFile, sheet ="data", skip = 0)
    
    dfQuestRaw$participant = substr(dfQuestRaw$participant, 3, 10)
    
    dfDemo     = dfQuestRaw[1:12]
    dfPostExp  = dfQuestRaw[c(1,13:26)]
    dfAQ       = dfQuestRaw[c(1,27:76)]
    dfEnd      = dfQuestRaw[c(1,77:86)]
    
    ##############################################################################################
    # rename all Variables of the demographic questionnaire part
    
    names(dfDemo) <- c("Subject",
                      "intro_quest1", #to be removed
                      "LAge", 
                      "LSex",    #1= female, 2 = male
                      "LHand",
                      "LMLang",  #1= German, 2 = English, 3 = sonstiges
                      "LEdu",
                      "LStudyOrWork", 
                      "LHDknown", #1: no, 2: yes, 3: I don't know
                      "LHDKind", 
                      "LHDConstraint",   #empty, to be removed
                      "ExpID")
    
    dfDemo$intro_quest1<-NULL

    dfDemo$LSex[dfDemo$LSex == "1"] <- "female"
    dfDemo$LSex[dfDemo$LSex == "2"] <- "male"
    dfDemo$LSex[dfDemo$LSex == "3"] <- "diverse"
    
    dfDemo$LHand[dfDemo$LHand == "1"] <- "right"
    dfDemo$LHand[dfDemo$LHand == "2"] <- "left"
    dfDemo$LHand[dfDemo$LHand == "3"] <- "both"
    
    dfDemo$LMLang[dfDemo$LMLang == "1"] <- "German"
    dfDemo$LMLang[dfDemo$LMLang == "2"] <- "English"
    dfDemo$LMLang[dfDemo$LMLang == "3"] <- "other"

    dfDemo$LEdu[dfDemo$LEdu == "1"] <- "Hauptschulabschluss"
    dfDemo$LEdu[dfDemo$LEdu == "2"] <- "Realschule (or comparable)"
    dfDemo$LEdu[dfDemo$LEdu == "3"] <- "Abitur (or comparable)"
    dfDemo$LEdu[dfDemo$LEdu == "4"] <- "Bachelor"
    dfDemo$LEdu[dfDemo$LEdu == "5"] <- "Master/Diplom"
    dfDemo$LEdu[dfDemo$LEdu == "6"] <- "Promotion"
    dfDemo$LEdu[dfDemo$LEdu == "7"] <- "Habilitation"
    dfDemo$LEdu[dfDemo$LEdu == "8"] <- "sonstiges"

    dfDemo$LHDknown[dfDemo$LHDknown == "1"] <- "NO. No known Hear Dis"
    dfDemo$LHDknown[dfDemo$LHDknown == "2"] <- "YES. Known Hear Dis"
    dfDemo$LHDknown[dfDemo$LHDknown == "3"] <- "I don't know."

    dfDemo$LHDConstraintText = NA
    dfDemo$LHDConstraintText[dfDemo$LHDConstraint == "1"] <- "1- Gar nicht eingeschraenkt"
    dfDemo$LHDConstraintText[dfDemo$LHDConstraint == "2"] <- "2- Ein wenig eingeschraenkt"
    dfDemo$LHDConstraintText[dfDemo$LHDConstraint == "3"] <- "3- Ziemlich eingeschraenkt"
    dfDemo$LHDConstraintText[dfDemo$LHDConstraint == "4"] <- "4- Extrem eingeschraenkt"
    
    #reorder the columns
    dfDemo <- dfDemo[, c(1:10,12,11)]
    
    ##############################################################################################
    
    names(dfEnd) <- c("Subject",
                      "Notes", 
                      "LPC1", "LPC2", "LPC3", "LPC4", "LPC5", "LPC6",
                      "TStart",
                      "TEnd",
                      "ExpDuration")
  
    dfEnd$FSUCode = paste0(dfEnd$LPC1, dfEnd$LPC2, dfEnd$LPC3, dfEnd$LPC4, dfEnd$LPC5, dfEnd$LPC6)
    dfEnd[3:8]<-NULL
    
    dfEnd$FSUCode[dfEnd$FSUCode == "NANANANANANA"] <- NA
    
    #reorder the columns
    dfEnd <- dfEnd[, c(1, 6,3,5,2)]
  
    
    ##############################################################################################
    
    dfDemoEnd = merge(dfDemo, dfEnd, by = "Subject")
    
    remove(dfDemo, dfEnd)
    
    ##############################################################################################
    
    names(dfPostExp) <- c("Subject",
                          "ALLSoundPlayed",  #1 = yes, 2 = no, 3 = I don't know
                          "InstructionsClear", 
                          "postExpQ01",   
                          "postExpQ02",
                          "postExpQ03",   
                          "postExpQ04",  
                          "postExpQ05",   
                          "postExpQ06",   
                          "postExpQ07",   
                          "postExpQ08",   
                          "postExpQ09",   
                          "postExpQ10",   
                          "postExpQ11",  
                          "postExpStrategy")
     
    dfPostExp$ALLSoundPlayed[dfPostExp$ALLSoundPlayed == "1"] <- "Yes, all sounds played"
    dfPostExp$ALLSoundPlayed[dfPostExp$ALLSoundPlayed == "2"] <- "No, not all sounds played"
    dfPostExp$ALLSoundPlayed[dfPostExp$ALLSoundPlayed == "3"] <- "I don't know, if all sounds were played"

    ##############################################################################################
    
    colnames(dfAQ)[1] <- "Subject"
    for(i in  2:10){  colnames(dfAQ)[i] <- paste0("AQ0", i-1, "_raw")}
    for(i in 11:51){  colnames(dfAQ)[i] <- paste0("AQ",  i-1, "_raw")}
    ##############################################################################################
    
    dfAQ$Subject      <- as.factor(dfAQ$Subject)
    dfPostExp$Subject <- as.factor(dfPostExp$Subject)
    dfDemoEnd$Subject <- as.factor(dfDemoEnd$Subject)
    dfDemoEnd$LSex    <- as.factor(dfDemoEnd$LSex)
    dfDemoEnd$LHand   <- as.factor(dfDemoEnd$LHand)

    #############################################################################################
    
    return(list(dfDemoEnd, dfPostExp, dfAQ))
}

