require(ggplot2)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(data.table)
library(stringr)

######################
### Data treatment ###
######################

setwd('C:/Users/Lenovo/Desktop/world/Ciência/Projetos de pesquisa/Luisiana')

data = 
  read_excel('dt.xlsx')

avaliadores = 
  fread('AV.csv', header = T)

avaliadores = 
  avaliadores[, 1:4]

teste =
  fread('TST.csv', header = T)

data =
  merge(x = data,
        y = teste,
        by.x = "subteste",
        by.y = "subteste")

data =
  merge(x = data, 
        y = avaliadores, 
        by.x = "crianca", 
        by.y = "crianca")

names(data)[names(data) == "nota Esc"] <- "nota_esc"

###################
### DESCRITIVAS ###
###################

#Tarefas
#add criterios de exclusao
#ver efeito avaliador
#diferenca pre-pos contra presenca
#descritivas overall por avaliador


plot<- plyr::ddply(data, c('condicao'), summarise,
                   N    = length(nota_esc),
                   mean = mean(nota_esc, na.rm = TRUE),
                   sd   = sd(nota_esc, na.rm = TRUE),
                   se = sd/sqrt(N)
)

data$filter = 0 #1 para quem está abaixo 2 para acima
ggplot(data = filter(data, as.numeric(data$`% presença`) < .6), aes(x= condicao, y=nota_esc))+
    facet_wrap(~dominio)+ 
#    geom_errorbar(size = 1.2, aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(0.5)) +
#    geom_point(shape = 21, size = 2.5, position=position_dodge(0.5), fill = 'white')+
    geom_boxplot()+
    ylab('Nota bruna')+
    xlab('')+
    scale_x_discrete(limits= c('pré', 'pós'), labels = c('Pré teste','Pós teste'))

#numero diferente te trials por condicao e dominio
plot<- plyr::ddply(data, c('dominio', 'condicao'), summarise,
                   N    = length(nota_esc),
                   mean = mean(nota_esc, na.rm = TRUE),
                   sd   = sd(nota_esc, na.rm = TRUE),
                   se = sd/sqrt(N)
)

ggplot(data = plot, aes(x= condicao, y=mean))+
    facet_wrap(~dominio, scales = "free")+
    geom_errorbar(size = 1.2, aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(0.5)) +
    geom_point(shape = 21, size = 2.5, position=position_dodge(0.5), fill = 'white')+
    ylab('Nota escalonada')+
    xlab('')+
    scale_x_discrete(limits= c('pré', 'pós'), labels = c('Pré teste', 'Pós teste'))


plot<- plyr::ddply(data, c('subteste', 'condicao'), summarise,
                   N    = length(nota_esc),
                   mean = mean(nota_esc, na.rm = TRUE),
                   sd   = sd(nota_esc, na.rm = TRUE),
                   se = sd/sqrt(N)
)


#Separar questionario de 
ggplot(data = plot, aes(x= condicao, y=mean))+
    facet_wrap(~subteste, scales = "free")+
    geom_errorbar(size = 1.2, aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(0.5)) +
    geom_point(shape = 21, size = 2.5, position=position_dodge(0.5), fill = 'white')+
    ylab('Nota escalonada')+
    xlab('')+
    scale_x_discrete(limits= c('pré', 'pós'), labels = c('Pré teste', 'Pós teste'))

