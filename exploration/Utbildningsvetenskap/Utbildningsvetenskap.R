#Publikationer inom utbildningsvetenskap de senaste fem åren

#tidyverse behövs
library(plyr)
library(tidyverse)



#Utsökningsfil från DiVA
utbildningsvet <- read.csv(file="utbvet_5.csv", header = TRUE)
plyr::rename(utbildningsvet, c("FridaLevel" = "NorskaListan", "ISI" = "WoS"))


utbildningsvet %>% count(PublicationType)
table(utbildningsvet$PublicationType, utbildningsvet$Year)

#Publiceringskanaler i ISI
WoS <- subset(utbildningsvet, ISI !="")
WoS[,"Journal"]

#Publikationskanaler med norsk nivå
Norska <- subset(utbildningsvet, FridaLevel !="0")
Norska[,c("Journal","Publisher","FridaLevel", "ISI")]




