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


########################
## APPLYING FUNCTIONS ##
########################

#sets the directory to where your file is
setwd('C:/Users/Lenovo/Documents/GitHub/eye_tracking_ufabc/data')

data =
  fread("complete.csv")[, 3:ncol(fread("complete.csv"))]

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
  }
}

data = 
  dplyr::bind_rows(data)

###################################
## SUMMARIZING BY BABY AND TRIAL ##
###################################

teste <- plyr::ddply(data, c('crianca', 'video'), summarise,

                    frente_fundo = mean(frente_fundo, na.rm = T),
                    fundo_frente = mean(fundo_frente, na.rm = T),
                    fundo_errado = mean(fundo_errado, na.rm = T),
                    errado_fundo = mean(errado_fundo, na.rm = T),
                    frente_errado = mean(frente_errado, na.rm = T),
                    errado_frente = mean(errado_frente, na.rm = T),
                    olhou_frente = max(acerto_frente),
                    olhou_errado = max(olhou_pro_errado)
)

write.csv(teste, 'alternancia.csv')

