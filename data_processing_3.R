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

info_raw =
  fread("babies_rawdata.csv")

final = 
  merge(info_raw, alternancia, 
        by.x =c("crianca", "video"), by.y = c("crianca", "video"), 
        all = T)




