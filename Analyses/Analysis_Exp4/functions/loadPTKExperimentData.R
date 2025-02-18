####################################################################################
# Author: Verena Skuk/Christine Nussbaum
# File: C:\Users\Christine Nussbaum\Documents\Arbeit\Forschungsprojekte\2024_Simultaneus_Adaptation\Psytoolkit_Ident\Analysis
# 
####################################################################################
library(stringr)  # f?r datenaufbereitung

loadPTKExperimentData <- function(relDirPath) {
  
    file_list <- list.files(relDirPath)   # get list with all filenames

    rawDF <- data.frame()               # create empty dataset
    
    #read all the files, give it a SubjectCode and paste it together
    for (file in file_list){
  
      filename = paste0(relDirPath,file)
      printstr = paste0("load ", filename)
      print(printstr)
      expName     = str_sub(file, 1, 9)
      date        = str_sub(file, 12, 21)
      SubjectCode = str_sub(file, 61, 68)
      temp_data<-read.delim(filename, header=FALSE, sep=" ", stringsAsFactors=FALSE, col.names = paste0("V",seq_len(13)), fill = TRUE)
      temp_data$Experiment = expName
      temp_data$Date = date
      temp_data$Subject <- SubjectCode
      rawDF<-rbind(rawDF, temp_data)
      rm(temp_data)
    }
    
    # tidy up
    rm(file, file_list, filename, relDirPath, SubjectCode)
        
    # check SubjectCodes
    unique(rawDF$Subject)
    return(rawDF)
}


