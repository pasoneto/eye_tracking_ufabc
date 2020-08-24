library(ggplot2)
library(dplyr)
library(data.table)

##############################
## DEFINING HANDY FUNCTIONS ##
##############################

#Function tags the last appearence of element.
last_element = function(vector, element, tag){
  p_element = which(vector == element) #gets position of element
  i_last = p_element[length(p_element)] #takes the last appearence of element
  vector[i_last] = tag #tags
  return(vector)
}

#Test case
#v = c('oi','oi','oi','tchau', 'tchau', 'tchau')
#e = 'oi'
#last_element(v, e, 'ultimo_oi') #Works!


#Defining function to compute alternance between columns of vectors
alternancia = function(vector1, vector2){
  count = 0
  for(i in 1:length(vector1)-1){
    if( isTRUE((vector1[i] == 1) && (vector2[i+1] == 1)) ){
      count = count + 1
    }
  }
  return(count)
}


#fixation acerto frente
#fixation olhou pro errado
#tirar -40ms
#tirar tudo que estiver em cima da ultima_cruz

fix_dur_sum = function(var1, var2, parte, primeiro){
  
  #will return the first value of var2 if var1 is true
  if(isTRUE(primeiro)){
    for(j in 1:length(var1)){
      if( (isTRUE(var1[j])) & (parte[j] != 'ultima_cruz') ){
        return(var2[j])
      }
    }
  } else {
    
    #will sum up values of var2 every time var1 is true
    count = 0
    for(j in 1:length(var1)){
      if( (isTRUE(var1[j])) & (parte[j] != 'ultima_cruz') ){
        count = count + as.numeric(var2[j]) 
      }
    }
    return(count)
  }
}

########################
## APPLYING FUNCTIONS ##
########################

#sets the directory to where your file is
setwd('C:/Users/Lenovo/Documents/GitHub/eye_tracking_ufabc/data')

data =
  fread("dp1a.csv")[, 2:ncol(fread("dp1a.csv"))]

data =
  data[, parte := last_element(parte,'cruz','ultima_cruz'),
         by = c('crianca', 'video')
       ]

data =
  dplyr::filter(data, 
                data$parte == "ultima_cruz" | data$parte == 'pos_cruz'
               )
data =
  split(data, 
        list(data$crianca, 
             data$video))


for(i in 1:length(data)){
  if(nrow(data[[i]]) != 0){
    
    data[[i]]$frente_fundo = alternancia(data[[i]]$acerto_frente, data[[i]]$acerto_fundo)
    data[[i]]$fundo_frente = alternancia(data[[i]]$acerto_fundo, data[[i]]$acerto_frente)
    data[[i]]$fundo_errado = alternancia(data[[i]]$acerto_fundo, data[[i]]$olhou_pro_errado)
    data[[i]]$errado_fundo = alternancia(data[[i]]$olhou_pro_errado, data[[i]]$acerto_fundo)
    data[[i]]$frente_errado = alternancia(data[[i]]$acerto_frente, data[[i]]$olhou_pro_errado)
    data[[i]]$errado_frente = alternancia(data[[i]]$olhou_pro_errado, data[[i]]$acerto_frente)

    data[[i]]$indice_alternancia_rja = (data[[i]]$fundo_frente - data[[i]]$fundo_errado)/(data[[i]]$fundo_frente + data[[i]]$fundo_errado)
    data[[i]]$indice_alternancia_ija = (data[[i]]$frente_fundo - data[[i]]$errado_fundo)/(data[[i]]$frente_fundo + data[[i]]$errado_fundo)
  
    data[[i]]$indice_alternancia_objeto = (data[[i]]$frente_errado - data[[i]]$errado_frente)/(data[[i]]$frente_errado + data[[i]]$errado_frente)
        
    data[[i]]$fixation_acerto_frente = fix_dur_sum(data[[i]]$acerto_frente, data[[i]]$cur_fix_dur, data[[i]]$parte, primeiro = TRUE)
    data[[i]]$fixation_olhou_pro_errado = fix_dur_sum(data[[i]]$olhou_pro_errado, data[[i]]$cur_fix_dur, data[[i]]$parte, primeiro = TRUE)
    
    data[[i]]$total_acerto_frente = fix_dur_sum(data[[i]]$acerto_frente, data[[i]]$cur_fix_dur, data[[i]]$parte, primeiro = FALSE)
    data[[i]]$total_olhou_errado = fix_dur_sum(data[[i]]$olhou_pro_errado, data[[i]]$cur_fix_dur, data[[i]]$parte, primeiro = FALSE)
  }
}

data = 
  dplyr::bind_rows(data)

#removing NAs
data[is.na(data)] <- 0

write.csv(data, 'dp2.csv')


