####################################################################################
# Author: Verena Skuk and Christine Nussbaum
# 
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
      expName     = str_sub(file, 1, 19)
      date        = str_sub(file, 16, 25)
      SubjectCode = str_sub(file, 65, 72)
      temp_data<-read.delim(filename, header=FALSE, sep=" ", stringsAsFactors=FALSE, col.names = paste0("V",seq_len(15)), fill = TRUE)
      temp_data$Experiment = expName
      temp_data$Date = date
      temp_data$Participant <- SubjectCode
      rawDF<-rbind(rawDF, temp_data)
      rm(temp_data)
    }
    
    # tidy up
    rm(file, file_list, filename, relDirPath, SubjectCode)
        
    # check SubjectCodes
    unique(rawDF$Subject)
    return(rawDF)
}


