library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)

#sets the directory to where your file is
setwd('/home/pasoneto/Documents/github/eye_tracking_ufabc')

#Reads data
data = 
  fread('data_inputs/babies_rawdata.csv')

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


#################
## SECOND TASK ##
#################
data = 
  read_excel('data_inputs/babies_times.xlsx')

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


#######################
## TAGGING WITH ROIS ##
#######################
rois =
  read_excel("data_inputs/roi.xlsx")

#Adding info from babies_times.xlsx
criancas = 
  merge(criancas, data, by.x = "video_clip", by.y = "...1", all = F)

criancas = 
  merge(criancas, rois, by.x = "video_clip", by.y = "Trial", all = F)

#checando se acertou olhando fundo (TRUE for right, False or NA for wrong)

criancas$acerto_fundo = criancas$`Roi cabe�a(fundo)` == criancas$ROIfundo
criancas$acerto_frente = criancas$`ROI alvo (frente)` == criancas$ROIfrente
criancas$olhou_pro_errado = criancas$`ROI Nalvo (frente)` == criancas$ROIfrente

criancas$acerto_fundo <- as.character(criancas$acerto_fundo)
criancas$acerto_fundo <- ifelse(is.na(criancas$acerto_fundo), 
                             'FALSE', criancas$acerto_fundo)

criancas$acerto_frente <- as.character(criancas$acerto_frente)
criancas$acerto_frente <- ifelse(is.na(criancas$acerto_frente), 
                                'FALSE', criancas$acerto_frente)

criancas$olhou_pro_errado <- as.character(criancas$olhou_pro_errado)
criancas$olhou_pro_errado <- ifelse(is.na(criancas$olhou_pro_errado), 
                                'FALSE', criancas$olhou_pro_errado)

verificacao =
  data.frame(crianca = criancas$RECORDING_SESSION_LABEL,
             time = criancas$anchor_time,
             parte = criancas$parte_trial,
             video = criancas$video_clip,
             cur_fix_dur = criancas$CURRENT_FIX_DURATION,
             roi_frente_empirico = criancas$ROIfrente,
             roi_fundo_empirico = criancas$ROIfundo,
             roi_alvo_fundo = criancas$`Roi cabe�a(fundo)`,
             roi_alvo_frente = criancas$`ROI alvo (frente)`,
             roi_nao_alvo = criancas$`ROI Nalvo (frente)`,
             acerto_frente = criancas$acerto_frente,
             acerto_fundo = criancas$acerto_fundo,
             olhou_pro_errado = criancas$olhou_pro_errado,
             referencia = criancas$`Est�mulo(referencia esq/dir - olhando para tela)`)

write.csv(verificacao, "dp1a.csv")

################# #Come�ar daqui
## FINAL TASKS ##
#################

rm(list=ls()) #cleans directory

data =
  fread("dp1a.csv")[, 2:ncol(fread("dp1a.csv"))]

data =
  filter(data, 
         data$parte == 'cruz')

#Creating empty vectors
data$fixou_cruz = 0

data =
  split(data, 
        list(data$crianca, data$video))

#Contando o numero de acertos por crian�a e por trial
for(i in 1:length(data)){
  
  if(  nrow(data[[i]]) != 0  ) { #tirando os casos onde n�o houve dado para trial
    
    if( nrow(filter(data[[i]], data[[i]]$acerto_fundo == 'TRUE')) != 0 ){
      acerto = 1
      }
    else{
      acerto = 0
      }
    data[[i]]$fixou_cruz = acerto

  }
}

data =
  dplyr::bind_rows(data)

fixou_cruz <- plyr::ddply(data, c('crianca', 'video', 'referencia'), summarise,
                        fixou_cruz = mean(fixou_cruz, na.rm = TRUE)
)


####################################
####################################

data =
  fread("dp1a.csv")[, 2:ncol(fread("dp1a.csv"))]

data =
  filter(data, 
         data$parte == 'pos_cruz')

#Creating empty vectors
data$acertou_frente = 0
data$olhou_errado = 0

data =
  split(data, 
        list(data$crianca, data$video))

#Contando o numero de acertos por crian�a e por trial
for(i in 1:length(data)){
  
  if(  nrow(data[[i]]) != 0  ) { #tirando os casos onde n�o houve dado para trial
    
    if( nrow(filter(data[[i]], data[[i]]$acerto_frente == 'TRUE')) != 0 ){
      acerto_frente = 1
    }
    else{
      acerto_frente = 0
    }
    data[[i]]$acertou_frente = acerto_frente


    if( nrow(filter(data[[i]], data[[i]]$olhou_pro_errado == 'TRUE')) != 0 ){
      errado = 1
    }
    else{
      errado = 0
    }
    
    data[[i]]$olhou_errado = errado
    
  }
}

data =
  dplyr::bind_rows(data)

certo_errado <- plyr::ddply(data, c('crianca', 'video', 'referencia'), summarise,
                          acertou_frente = mean(acertou_frente, na.rm = TRUE),
                          olhou_errado = mean(olhou_errado, na.rm = TRUE)
)

#Adding info from babies_times.xlsx
final = 
  merge(certo_errado, fixou_cruz, by.x =c("crianca", "video", "referencia"), by.y = c("crianca", "video", "referencia"), all = T)

library(stringr)
##################
## Visualiza��o ##
##################
final$referencia <- str_replace_all(final$referencia, " - brinquedo da esquerda se move (jovem)", '')
final$referencia <- str_replace_all(final$referencia, "olhar para esquerda (adulto)", '')
final$referencia <- str_replace_all(final$referencia, "brinquedo da direita se move (adulto)", '')
final$referencia <- str_replace_all(final$referencia, "olhar p/ esquerda (Adulto)", '')
final$referencia <- str_replace_all(final$referencia, "brinquedo da direita se move (jovem)", '')
final$referencia <- str_replace_all(final$referencia, "olhar pra direita (adulto)", '')
unique(final$referencia)

plot <- plyr::ddply(final, c("referencia"), summarise,
                    mean = mean(acertou_frente, na.rm = TRUE)
)

final = 
  melt(data = final)

ggplot(data = filter(final, final$variable == 'acertou_frente'), aes(x = as.factor(value)))+
  facet_wrap(~referencia)+
  geom_histogram(stat = "count")




write.csv(final, 'verificacao_2.csv')

################
## Parei aqui ##
################












for(i in 1:length(data)){
  
  if(  nrow(data[[i]]) != 0  ) { #tirando os casos onde n�o houve dado para trial
    
    data[[i]]$n_acerto_fundo = nrow(filter(data[[i]], data[[i]]$acerto_fundo == 'TRUE'))
    data[[i]]$n_acerto_frente = nrow(filter(data[[i]], data[[i]]$acerto_frente == 'TRUE'))
    data[[i]]$n_olhou_errado = nrow(filter(data[[i]], data[[i]]$acerto_fundo == 'TRUE'))
  }
}



#Creating empty vectors
data$acertou_frente = 0
data$olhou_errado = 0









write.csv(describe, "acertos_trial.csv")

#writes the file to the current directory in your pc
