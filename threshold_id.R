library(dplyr)
library(data.table)

setwd('/home/pasoneto/Documents/github/eye_tracking_ufabc/data')
data = fread('dp1a.csv')

data %<>%
    mutate(roi_fundo_empirico = coalesce(roi_fundo_empirico, 0)) %>%
    group_by(crianca, video) %>%
    filter(parte == "pre_cruz") %>%
    #caso roi_frente_empirico == 7 porém roi_fundo != 3, agora ele é 3.
    mutate(roi_fundo_empirico = ifelse((roi_frente_empirico == 7 & video == "D1"), 3, roi_fundo_empirico)) %>%
    summarize(total = NROW(parte),
              pct_acerto = NROW(roi_fundo_empirico[roi_fundo_empirico == 3])/total) #%>%
#    write.csv("acertos_pre.csv")
write.csv(data, "acertos_pre.csv")

