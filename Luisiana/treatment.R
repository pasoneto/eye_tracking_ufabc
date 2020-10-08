require("ggplot2")
library(dplyr)
library(ggpubr)
library(data.table)

setwd('/home/pasoneto/Documents/github/eye_tracking_ufabc/Luisiana')

data = 
  fread('raw.csv')

names(data)[names(data) == colnames(data)[13]] <- "presenca_pct"
names(data)[names(data) == colnames(data)[11]] <- "presenca"
names(data)[names(data) == colnames(data)[10]] <- "idade"
names(data)[names(data) == colnames(data)[8]] <- "data_teste"
names(data)[names(data) == "Nascimento"] <- "nascimento"

#Correcting names
data$condicao <- stringr::str_replace_all(data$condicao, data$condicao[1], "pre")
data$condicao <- stringr::str_replace_all(data$condicao, data$condicao[9], "pos")

#Editing and checking dates
data$data_teste <- as.character(data$data_teste) 
data$nascimento <- as.character(data$nascimento)
data$idade_ano <- (as.Date(data$data_teste) - as.Date(data$nascimento))/365

write.csv(data, "treated.csv")





# ###################
# ### DESCRITIVAS ###
# ###################

# ###################################################
# ## VERIFICANDO EFEITO DE AVALIADOR NO pré-TESTE  ##
# ###################################################
# data = filter(data, data$teste == 'bailey')

# plot<- plyr::ddply(filter(data, data$condicao == 'pré'), c('av1_pre', 'crianca'), summarise,
#                    N    = length(nota_esc),
#                    mean = mean(nota_esc, na.rm = TRUE),
#                    sd   = sd(nota_esc, na.rm = TRUE),
#                    se = sd/sqrt(N)
# )

# #Testing significance of avaliador as independent variable
# summary(aov(formula = plot$mean~plot$av1_pre)) #F(4, 17) = 1.077, p=0.39

# ###################################################
# ## VERIFICANDO EFEITO DE AVALIADOR NO pós-TESTE  ##
# ###################################################


# plot<- plyr::ddply(filter(data, data$condicao == 'pós'), c('av1_pos', 'crianca'), summarise,
#                    N    = length(nota_esc),
#                    mean = mean(nota_esc, na.rm = TRUE),
#                    sd   = sd(nota_esc, na.rm = TRUE),
#                    se = sd/sqrt(N)
# )

# #Testing significance of avaliador as independent variable
# summary(aov(formula = plot$mean~plot$av1_pos)) #F(3, 18) = 1.52, p = 0.24


# ###################################
# ## Visualizando efeito avaliador ##
# ###################################

# plot1<- plyr::ddply(filter(data, data$condicao == 'pré'), c('av1_pre', 'crianca'), summarise,
#                    N    = length(nota_esc),
#                    mean = mean(nota_esc, na.rm = TRUE),
#                    sd   = sd(nota_esc, na.rm = TRUE),
#                    se = sd/sqrt(N),
#                    cond = 'pré'
# )

# plot2<- plyr::ddply(filter(data, data$condicao == 'pós'), c('av1_pos', 'crianca'), summarise,
#                    N    = length(nota_esc),
#                    mean = mean(nota_esc, na.rm = TRUE),
#                    sd   = sd(nota_esc, na.rm = TRUE),
#                    se = sd/sqrt(N),
#                    cond = 'pós'
# )

# plot <- data.frame(cond = c(plot1$cond, plot2$cond),
#                    avaliador = c(plot1$av1_pre, plot2$av1_pos),
#                    score = c(plot1$mean, plot2$mean))


# plot <- plyr::ddply(plot, c('avaliador', 'cond'), summarise,
#                     N    = length(score),
#                     mean = mean(score, na.rm = TRUE),
#                     sd   = sd(score, na.rm = TRUE)
# )


# ggplot(data = plot, aes(x= avaliador, y=mean, color = avaliador))+
#     facet_wrap(~cond)+ 
#     geom_errorbar(size = 1.2, aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.5)) +
#     geom_point(shape = 21, size = 2.5, position=position_dodge(0.5), fill = 'white')+
#     ylab('Score')+
#     xlab('Avaliador')+
#     theme(axis.text.x=element_text(angle=60, hjust=1))


#######################################
## VERIFICANDO EFEITO DE TRATAMENTO  ##
#######################################

#################################
## Local agrupado por subteste ##
#################################

rm(list=ls()) #cleans directory





plot <- dplyr::filter(plot, plot$teste == 'bailey')
plot <- dplyr::filter(plot, plot$dominio == 'linguagem')
plot <- dplyr::filter(plot, plot$condicao == plot$condicao[1])

out_detector(plot$mean)




###########################
## Ispecionando presença ##
###########################

dt = 
  fread('data_treated.csv', header = T, encoding = 'UTF-8')

plot <- plyr::ddply(dt, c("% presen\xe7a", 'crianca', 'condicao', 'teste'), summarise,
                    mean = mean(nota_esc, na.rm = TRUE)
)

pre = filter(plot, plot$condicao == 'pré')
pos = filter(plot, plot$condicao == 'pós')

plot <-
  dplyr::bind_cols(pre, pos)

plot =
  data.frame(
    presenca = plot$`% presença`,
    crianca = plot$crianca,
    dif = plot$mean1 - plot$mean,
    teste = plot$teste
  )

#Visualizando efeito de presen�a na diferen�a entre tratamentos
ggplot(data = plot, aes(x = presenca, y = dif))+
  geom_jitter()+
  geom_smooth(method = 'lm', formula = y~x)

#Visualizando efeito de presença na diferença entre tratamentos
#por teste
ggplot(data = plot, aes(x = presenca, y = dif))+
  facet_wrap(~teste)+
  geom_jitter()+
  geom_smooth(method = 'lm', formula = y~x)
















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
    scale_x_discrete(limits= c('pré', 'pós'), labels = c('pré teste', 'pós teste'))


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
    scale_x_discrete(limits= c('pré', 'pós'), labels = c('pré teste', 'pós teste'))

