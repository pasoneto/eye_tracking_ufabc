library(ggplot2)
library(dplyr)
library(data.table)

#sets the directory to where your file is
setwd('C:/Users/Lenovo/Documents/GitHub/eye_tracking_ufabc/data')

alternancia =
  fread("dp2.csv")[, 2: ncol(fread("dp2.csv"))]


alternancia <- plyr::ddply(alternancia, c('crianca', 'video'), summarise,
                           frente_fundo = unique(frente_fundo), 
                           fundo_frente = unique(fundo_frente), 
                           fundo_errado = unique(fundo_errado), 
                           errado_fundo = unique(errado_fundo), 
                           frente_errado = unique(frente_errado), 
                           errado_frente = unique(errado_frente), 
                           olhou_pro_errado = max(olhou_pro_errado), #max porque quero saber se teve ao menos um
                           acerto_frente = max(acerto_frente), #max porque quero saber se teve ao menos um
                           indice_alternancia_rja = unique(indice_alternancia_rja), 
                           indice_alternancia_ija = unique(indice_alternancia_ija), 
                           indice_alternancia_objeto = unique(indice_alternancia_objeto), 
                           total_acerto_frente = unique(total_acerto_frente), 
                           total_olhou_errado = unique(total_olhou_errado), 
                           fixation_acerto_frente = unique(fixation_acerto_frente), 
                           fixation_olhou_pro_errado = unique(fixation_olhou_pro_errado), 
                           indice_acerto = unique((acerto_frente - olhou_pro_errado)/ (acerto_frente + olhou_pro_errado))
)
alternancia[is.na(alternancia)] <- 0

#####################
## score transicao ##
#####################
condicoes =
  data.frame(condicao = c('ija', 'ija', 'ija', 'ija', 'ija', 'ija', 'rja', 'rja', 'rja', 'rja', 'rja', 'rja'),
             video = c('B2', 'B5', 'D2', 'D5', 'A2', 'A6', 'B6', 'B7', 'D1', 'D4', 'A1', 'A4')
  )

data = 
  merge(alternancia, condicoes, 
        by.x =c("video"), by.y = c("video"), 
        all = T)

prepos = fread("data_inputs/alternancia_alterado.csv")

prepos = 
  data.table(
    crianca = prepos$crianca,
    video = prepos$video,
    pre_pos = prepos$`pre/pos`
    )

data = 
  merge(data, prepos, 
        by.x =c("crianca", "video"), by.y = c("crianca", "video"), 
        all = T)


write.csv(data, 'dp3.csv')


