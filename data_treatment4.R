library(dplyr)
library(data.table)

setwd('/home/pasoneto/Documents/github/eye_tracking_ufabc/data')
data = fread('dp1a.csv')

data %>%
    mutate(roi_fundo_empirico = coalesce(roi_fundo_empirico, 0)) %>%
    group_by(crianca, video) %>%
    filter(parte == "pre_cruz") %>%
    summarize(total = NROW(parte),
              pct_acerto = NROW(roi_fundo_empirico[roi_fundo_empirico == 3])/total) %>%
    write.csv("acertos_pre.csv")
