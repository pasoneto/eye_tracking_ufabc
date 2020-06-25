library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)

#sets the directory to where your file is
setwd('C:/Users/Lenovo/Documents/GitHub/eye_tracking_ufabc')

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
View(criancas)
#Reunites data frame
criancas = 
  dplyr::bind_rows(criancas)

#Reorders data frame based on participant's name
criancas = criancas[order(criancas$RECORDING_SESSION_LABEL, criancas$TRIAL_INDEX),]


#################
## SECOND TASK ##
#################
data = 
  read_excel('babies_times.xlsx')

data$CRUZ_START <- c(3750, 3071, "baseline", 3740, 4237, "baseline", 3740, 3071, 3740, 3872, 3741, 3872, 4240, 3740, "baseline")
data$CRUZ_END <- c(4250, 3571, "baseline", 4240, 4737, "baseline", 4240, 3571, 4240, 4372, 4241, 4372, 4740, 4240,  "baseline")

#antes, durante depois da cruz
#data =
#  split(criancas, criancas$)

#Retirando .avi para ficar igual ao documento babies_times.xlsx
criancas$video_clip <- 
  str_replace(criancas$video_clip,".avi","")


#Adding info from babies_times.xlsx
criancas = 
  merge(criancas, data, by.x = "video_clip", by.y = "...1", all = F)

#Reorder
criancas = 
  criancas[order(criancas$RECORDING_SESSION_LABEL, criancas$TRIAL_INDEX),]

criancas$parte_trial <- 'baseline'

for(i in 1:nrow(criancas)){
  if(criancas$CRUZ_START[[i]] != 'baseline'){
    if(criancas$anchor_time[[i]] < as.numeric(criancas$CRUZ_START[[i]])){
     criancas$parte_trial[[i]] = 'pre_cruz'
    }
    if( (criancas$anchor_time[[i]] > as.numeric(criancas$CRUZ_START[[i]])) && (criancas$anchor_time[[i]] < as.numeric(criancas$CRUZ_END[[i]])) ){
      criancas$parte_trial[[i]] = 'cruz'
    }
    if(criancas$anchor_time[[i]] > as.numeric(criancas$CRUZ_END[[i]])){
      criancas$parte_trial[[i]] = 'pos_cruz'
    }
  }
}

#Printa a frequencia de trials por tipo
table(criancas$parte_trial)



#writes the file to the current directory in your pc
write.csv(criancas, "dados_tratados2.csv")

#criancas$CURRENT_FIX_START == data$CURRENT_FIX_START
#criancas$RECORDING_SESSION_LABEL == data$RECORDING_SESSION_LABEL
