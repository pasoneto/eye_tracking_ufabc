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


#fail
#teste <- plyr::ddply(data, c("video", "crianca"), summarise,
#                   frente_fundo = alternancia(acerto_frente, acerto_fundo),
#                   fundo_frente = alternancia(acerto_fundo, acerto_frente),
#                   fundo_errado = alternancia(acerto_fundo, olhou_pro_errado),
#                   errado_fundo = alternancia(olhou_pro_errado, acerto_fundo),
#                   frente_errado = alternancia(acerto_frente, olhou_pro_errado),
#                   errado_frente = alternancia(olhou_pro_errado, acerto_frente)
#                   )




#Verificando prevalencia de fixação e acerto
plot<- plyr::ddply(data, c("video", "crianca", "parte"), summarise,
                   fixacoes    = length(parte),
                   acerto_fundo = sum(acerto_fundo),
                   acerto_frente = sum(acerto_frente),
                   olhou_pro_errado = sum(olhou_pro_errado)
                   )


write.csv(plot, "fixacao.csv")

