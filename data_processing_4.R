library(dplyr)
library(data.table)

setwd('/home/pasoneto/Documents/github/eye_tracking_ufabc/data')
out = fread('acertos_pre.csv') # pré
data = fread('dp3.csv') # pós
dt = merge(x = out, 
           y = data, by.x = c("crianca", "video"), by.y = c("crianca", "video") )

filtro = function(x){ if(x < 0.33){return(1)}else{return(0)} }
dt %<>% 
    group_by(crianca, video) %>%
    mutate(filtro = filtro(pct_acerto))

write.csv(dt, "../data/dp4.csv")



