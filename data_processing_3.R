library(ggplot2)
library(dplyr)
library(data.table)

#sets the directory to where your file is
setwd('C:/Users/Lenovo/Documents/GitHub/eye_tracking_ufabc/data')

#######################################
## RETRIEVING INFO FROM RAW DATA SET ##
#######################################
alternancia =
  fread("alternancia.csv")[, 2: ncol(fread("alternancia.csv"))]

#####################
## score transicao ##
#####################
alternancia =
  alternancia[, alternancia_rja := 
                  (fundo_frente - fundo_errado) / (fundo_frente + fundo_errado),
       by = c('crianca', 'video')
       ]

alternancia =
  alternancia[, alternancia_ija := 
                  (frente_fundo - errado_fundo) / (frente_fundo + errado_fundo),
       by = c('crianca', 'video')
       ]

data =
  fread('alternancia_alterado.csv')[, 2:ncol(fread('alternancia_alterado.csv'))]

library(stringr)
data$`pre/pos` <- str_replace_all(data$`pre/pos`, "-", '0')

condicoes =
  data.frame(condicao = c('ija', 'ija', 'ija', 'ija', 'ija', 'ija', 'rja', 'rja', 'rja', 'rja', 'rja', 'rja'),
             video = c('B2', 'B5', 'D2', 'D5', 'A2', 'A6', 'B6', 'B7', 'D1', 'D4', 'A1', 'A4')
  )

data = 
  merge(data, condicoes, 
        by.x =c("video"), by.y = c("video"), 
        all = T)

write.csv(data, 'alternancia.csv')


