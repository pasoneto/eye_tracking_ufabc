library(readxl)
library(ggplot2)
library(dplyr)
#library(ggpubr)
library(data.table)

#sets the directory to where your file is
setwd('C:/Users/Lenovo/Desktop') 

#Reads data
data = 
  fread('babies_rawdata.csv')

data$anchor_time <- 0 #creates empty vectors for anchor 
data$correction_time <- 0 #creates empty vectors for new timing

#Separates data into 1 data frame for each participant and trial
criancas =
  split(data, 
        list(data$RECORDING_SESSION_LABEL, 
             data$TRIAL_LABEL))

for(i in 1:length(criancas)){ #for each trial from each participant
  #gets the index where condition is met (video_clip == video_name_end)
  inicio = match(unique(criancas[[i]]$video_clip), 
                        criancas[[i]]$VIDEO_NAME_END)
  
  #some trials are empty, so we don't do anything with them
  if(nrow(criancas[[i]]) != 0){
  
  #some trials never meet our criteria (video_clip == video_name_end)
  #so we skip them
    if(is.na(inicio) == FALSE){
      
      #now we finally perform the operation:
      
      #latencia is the time (ms) where video_clip == video_name_end
      latencia = criancas[[i]]$CURRENT_FIX_START[inicio]
      
      #we save it in a separate column, called correction_time
      criancas[[i]][["correction_time"]] = latencia
      
      #now we subtract the original time (current_fix_start) with
      #the time where things begin (video_clip == video_name_end).
      #initial time will always be 0, and previous will be negative
      criancas[[i]]$anchor_time <- criancas[[i]][["CURRENT_FIX_START"]] - latencia
  }
  
  }
}

#Reunites data frame
criancas = 
  dplyr::bind_rows(criancas)

#Reorders data frame based on participant's name
criancas = criancas[order(criancas$RECORDING_SESSION_LABEL, criancas$TRIAL_INDEX),]
data = data[order(data$RECORDING_SESSION_LABEL, data$TRIAL_INDEX),]

#writes the file to the current directory in your pc
write.csv(criancas, "dados_tratados.csv")
criancas$CURRENT_FIX_START == data$CURRENT_FIX_START
criancas$RECORDING_SESSION_LABEL == data$RECORDING_SESSION_LABEL
