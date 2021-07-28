library(tidyverse); library(randomForest); library(pdp)

#Community 2: Zostera
ZoDensWQ_combined = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/ZoDensWQ_combined.csv")

Zo_RFvars = ZoDensWQ_combined %>% 
  select(dens.percomp.change, dens.weight.mean, 
         )

#Random Forests
max.compdenschangeRF <- randomForest(dens.percomp.change ~ ., RmZone_Env %>% drop_na() %>% select(-dens.change, -dens.weight.mean, -SAVArea.change, -SAVArea.prop.change, -SAVArea, -dens.prop.change, -SAVArea.percomp,  -SAVArea.percomp.change, -dens.percomp, -SAVArea.percomp.y1, -denscomp.max))  #41%
max.compareachangeRF <- randomForest(SAVArea.percomp.change ~ ., RmZone_Env %>% drop_na() %>% select(-dens.change, -dens.weight.mean, -SAVArea.change, -SAVArea.prop.change, -SAVArea, -dens.prop.change, -SAVArea.percomp, -dens.percomp.change, -dens.percomp, -dens.percomp.y1, -denscomp.max)) 
varImpPlot(max.compdenschangeRF, type = 2) #solid, Sal.y1Dpos and Sal.me, ChlA.me too
varImpPlot(max.compareachangeRF, type = 2)
