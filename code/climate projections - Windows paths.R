#cleanest climate projection code imaginable in order to remotely run this on a windows path: 
library(tidyverse); library(lubridate); library(lme4); library(vroom);

#Converting mac paths to windows paths: 
# r:/Current Projects needs to be r:/Current Projects 

SAVCommDensWQ_ForPred.with0s = vroom("r:/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_ForPredictions.with0s.csv")

twentyone = vroom("r:/Current Projects/Predicting-SAV/data/Water Quality/twentyone.csv")
future_yrs = seq(2020, 2060, by = 1) #should this be 2021
#Initial Community Dataframes, and ME Models here####
#check the ENV vars and the replace_na for each community here!

#Zostera Initial and Model####
Zostera_initialDPC = SAVCommDensWQ_ForPred.with0s %>% 
  ungroup() %>%
  filter(SpCluster == "Zostera") %>%
  # filter(!STATION %in% c("LE5.5-W")) %>% #seriously fuck this station
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, dens.weight.mean,  
         Chla.spme, TN.spme, Secc.summe, #customize these per community
         Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>%
  mutate(dens.weight.mean = case_when(dens.weight.mean == 0 ~ 0.0001, 
                                      TRUE ~ dens.weight.mean))
#vector of zos_stations for the loop
zos_station = Zostera_initialDPC$STATION

#DF and model for Zostera
ZoDensWQsem.No0_Predict = SAVCommDensWQ_ForPred.with0s %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "Zostera") %>%
  #  filter(!STATION %in% c("LE5.5-W")) %>% #seriously fuck this station
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1,
         denscomp.max, dens.percomp, 
         Chla.spme, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>%
  mutate(dens.weight.mean = case_when(dens.weight.mean == 0 ~ 0.0001, 
                                      TRUE ~ dens.weight.mean)) %>%
  mutate(dens.weight.mean.y1 = case_when(dens.weight.mean.y1 <= 0 ~ 0.0001, 
                                         TRUE ~ dens.weight.mean.y1)) %>%
  as.data.frame()

ZoInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1  + 
                     log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + 
                     (dens.percomp.y1:log10(Temp.spmed)) +  
                     (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)) + (1|STATION), data = ZoDensWQsem.No0_Predict)
model_performance(ZoInt.lmer)

#check_collinearity(ZoInt.lmer)

ZoDWM.lmer <- lmer(log10(dens.weight.mean) ~ log10(dens.weight.mean.y1)  + 
                     log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + 
                     (log10(dens.weight.mean.y1):log10(Temp.spmed)) +  
                     (log10(dens.weight.mean.y1):log10(Sal.summed)) + (log10(dens.weight.mean.y1):log10(Chla.spme))+ (log10(dens.weight.mean.y1):log10(Secc.summe)) + (1|STATION), data = ZoDensWQsem.No0_Predict)

model_performance(ZoDWM.lmer) #DINGDINGDING
check_model(ZoDWM.lmer)
AIC(ZoInt.lmer, ZoDWM.lmer)

#Ruppia Initial and Model####
Ruppia_initialDPC = SAVCommDensWQ_ForPred.with0s %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Ruppia") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, dens.weight.mean,
         Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% #we dont need these really... 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>%
  mutate(dens.weight.mean = case_when(dens.weight.mean == 0 ~ 0.0001, 
                                      TRUE ~ dens.weight.mean))

ru_station = Ruppia_initialDPC$STATION

#load in Ru model and dataa
RuDensWQsem.No0_Predict = SAVCommDensWQ_ForPred.with0s %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "Ruppia") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max, dens.percomp, 
         Temp.spme, Chla.spme, Sal.spme, TP.spme, TN.spme) %>%
  drop_na() %>% #1214 points
  mutate(dens.weight.mean = case_when(dens.weight.mean <= 0 ~ 0.0001, 
                                      TRUE ~ dens.weight.mean)) %>%
  mutate(dens.weight.mean.y1 = case_when(dens.weight.mean.y1 <= 0 ~ 0.0001, 
                                         TRUE ~ dens.weight.mean.y1)) %>%
  as.data.frame()

RuInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(Chla.spme) + 
                     log10(TP.spme) + log10(TN.spme) + log10(Temp.spme) +
                     (dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(Chla.spme)) + 
                     (log10(TP.spme):dens.percomp.y1) + (log10(TN.spme):dens.percomp.y1) + 
                     (log10(Temp.spme):dens.percomp.y1) + (1|STATION), 
                   data = RuDensWQsem.No0_Predict)
model_performance(RuInt.lmer)

RuDWM.lmer <- lmer(log10(dens.weight.mean) ~ log10(dens.weight.mean.y1) + log10(Chla.spme) + 
                     log10(TP.spme) + log10(TN.spme) + log10(Temp.spme) +
                     (log10(dens.weight.mean.y1):log10(Sal.spme)) + (log10(dens.weight.mean.y1):log10(Chla.spme)) + 
                     (log10(TP.spme):log10(dens.weight.mean.y1)) + (log10(TN.spme):log10(dens.weight.mean.y1)) + 
                     (log10(Temp.spme):log10(dens.weight.mean.y1)) + (1|STATION), 
                   data = RuDensWQsem.No0_Predict)
model_performance(RuDWM.lmer)

#MixedMeso Initial and Model####
MixMeso_initialDPC = SAVCommDensWQ_ForPred.with0s %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "MixedMeso") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) 

MM_station = MixMeso_initialDPC$STATION

MMDensWQsem.No0_Predict = SAVCommDensWQ_ForPred.with0s %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "MixedMeso") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max, dens.percomp, 
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  drop_na() %>% #161
  mutate(dens.weight.mean = case_when(dens.weight.mean <= 0 ~ 0.0001, 
                                      TRUE ~ dens.weight.mean)) %>%
  mutate(dens.weight.mean.y1 = case_when(dens.weight.mean.y1 <= 0 ~ 0.0001, 
                                         TRUE ~ dens.weight.mean.y1)) %>%
  as.data.frame()

MMInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(TN.summe) + log10(Chla.summe) + log10(TP.summe) +log10(Temp.summin) +log10(Sal.sumy1max):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1 +log10(TN.summe):dens.percomp.y1+ log10(Temp.summin):dens.percomp.y1 + (1|STATION), data = MMDensWQsem.No0_Predict)

model_performance(MMInt.lmer)

MMDWM.lmer <- lmer(log10(dens.weight.mean) ~ log10(dens.weight.mean.y1) + log10(TN.summe) + log10(Chla.summe) + log10(TP.summe) +log10(Temp.summin) +log10(Sal.sumy1max):log10(dens.weight.mean.y1) + log10(Chla.summe):log10(dens.weight.mean.y1) + log10(TP.summe):log10(dens.weight.mean.y1) +log10(TN.summe):log10(dens.weight.mean.y1)+ log10(Temp.summin):log10(dens.weight.mean.y1) + (1|STATION), data = MMDensWQsem.No0_Predict)

model_performance(MMDWM.lmer)

#Fresh Initial and Model####
Fresh_initialDPC = SAVCommDensWQ_ForPred.with0s %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Fresh") %>%
  filter(year == "2020") %>% 
  filter(!STATION == "PIS0033") %>% #apparently this station aint in the Projecteddata
  select(STATION, year, dens.percomp, denscomp.max, dens.weight.mean,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp))

F_station = Fresh_initialDPC$STATION


FreshDensWQsem.No0_Predict = SAVCommDensWQ_ForPred.with0s %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "Fresh") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max, dens.percomp, 
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>%
  drop_na() %>% #161
  mutate(Sal.summe = Sal.summe + .1) %>%
  mutate(dens.weight.mean = case_when(dens.weight.mean <= 0 ~ 0.0001, 
                                      TRUE ~ dens.weight.mean)) %>%
  mutate(dens.weight.mean.y1 = case_when(dens.weight.mean.y1 <= 0 ~ 0.0001, 
                                         TRUE ~ dens.weight.mean.y1)) %>%
  as.data.frame()

FInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + #CHECK FINT MODEL####
                  log10(Sal.summe) + log10(Chla.summe) + 
                    log10(TP.summe)  + 
                    log10(Temp.sumy1me) + log10(Temp.summe)  + 
                    log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + 
                    #log10(TP.summe):dens.percomp.y1  + #these ones make it singular so idk
                    log10(Temp.sumy1me):dens.percomp.y1 + 
                    (1|STATION), data = FreshDensWQsem.No0_Predict) 

model_performance(FInt.lmer)

FDWM.lmer <- lmer(log10(dens.weight.mean) ~ log10(dens.weight.mean.y1) + #CHECK FINT MODEL####
                  log10(Sal.summe) + log10(Chla.summe) + 
                    log10(TP.summe)  + 
                    log10(Temp.sumy1me) + log10(Temp.summe)  + 
                    log10(Sal.summe):log10(dens.weight.mean.y1) + log10(Chla.summe):log10(dens.weight.mean.y1) + 
                    log10(TP.summe):log10(dens.weight.mean.y1)  + #these ones make it singular so idk
                    log10(Temp.sumy1me):log10(dens.weight.mean.y1) + 
                    (1|STATION), data = FreshDensWQsem.No0_Predict) 

model_performance(FDWM.lmer)


OB_od90 = vroom("r:/Current Projects/Predicting-SAV/data/communityDFs/FutureMatrix1990-2020.csv") 

#OneTrueBay_CC Simulations#####
Zostera.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CC.csv")
Ruppia.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CC.csv")
MixMeso.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CC.csv")
Fresh.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CC.csv")

#Sticking with the DWM version, which both uses the DWM model and DPC model for projections
#Zostera CC####
bigZodatalist = list()
#DWM attempt (Works!)
for (t in 1:20){ 
  #t = 2
  Zos.OTB_CC = Zostera.OneBay_CC[Zostera.OneBay_CC$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_CC[Zos.OTB_CC$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.meanDPC = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      dens.weight.mean.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],13]
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.weight.mean = 10^(predict(ZoDWM.lmer, newdata = .))) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001,
                                            dens.weight.mean > denscomp.max ~ denscomp.max, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.weight.meanDPC = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.meanDPC = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                               TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] ##dens.weight.mean
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.percomp.change
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.percomp.changeDPC
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}

Zo_CC.PredOneTrueBae.DWM = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_CC.PredOneTrueBae.DWM,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBae.DWM.csv")

#biglist, not needed anymore.
gc(bigZodatalist)
#Zostera WIP
#DWM ZOstaer WIP#####
bigZodatalist = list()
for (t in 1:20){ 
  #t = 2
  Zos.OTB_WIP = Zostera.OneBay_WIP[Zostera.OneBay_WIP$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_WIP[Zos.OTB_WIP$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.meanDPC = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      dens.weight.mean.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],13]
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.weight.mean = 10^(predict(ZoDWM.lmer, newdata = .))) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001,
                                            dens.weight.mean > denscomp.max ~ denscomp.max, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.weight.meanDPC = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.meanDPC = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                               TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] ##dens.weight.mean
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.percomp.change
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.percomp.changeDPC
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}

Zo_WIP.PredOneTrueBae.DWM = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_WIP.PredOneTrueBae.DWM,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBae.DWM.csv")
gc(bigZodatalist)

#RuppiaDWM CC####
bigRudatalist = list()

for (t in 1:20){ 
  #t = 2
  Ru.OTB_CC = Ruppia.OneBay_CC[Ruppia.OneBay_CC$simnum_OB == t,] 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Ru.OTB_CC[Ru.OTB_CC$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.meanDPC = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      dens.weight.mean.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11]
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.weight.mean = 10^(predict(RuDWM.lmer, newdata = .))) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001,
                                            dens.weight.mean > denscomp.max ~ denscomp.max, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.weight.meanDPC = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.meanDPC = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                               TRUE ~ dens.weight.mean))
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.weight.mean
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.meanDPC
    }
    
    Rudatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Rudatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRudatalist[[t]] <- siteenvdataagg
  
} 

Ru_CC.PredOneTrueBae.DWM = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_CC.PredOneTrueBae.DWM,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBae.DWM.csv")
gc(bigRudatalist)

#DWM Ruppia Wip####
bigRudatalist = list()

for (t in 1:20){ 
  #t = 2
  Ru.OTB_WIP = Ruppia.OneBay_WIP[Ruppia.OneBay_WIP$simnum_OB == t,] 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Ru.OTB_WIP[Ru.OTB_WIP$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.meanDPC = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      dens.weight.mean.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11]
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.weight.mean = 10^(predict(RuDWM.lmer, newdata = .))) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001,
                                            dens.weight.mean > denscomp.max ~ denscomp.max, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.weight.meanDPC = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.meanDPC = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                               TRUE ~ dens.weight.mean))
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.weight.mean
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.meanDPC
    }
    
    Rudatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Rudatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRudatalist[[t]] <- siteenvdataagg
  
} 

#BigRuData#
Ru_WIP.PredOneTrueBae.DWM = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_WIP.PredOneTrueBae.DWM,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBae.DWM.csv")

gc(bigRudatalist)
#skipping MM for now sorry



#Fresh CC DWM####
bigFdatalist = list()

for (t in 1:20){ 
  #t = 2
  F.OTB_CC = Fresh.OneBay_CC[Fresh.OneBay_CC$simnum_OB == t,] 
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 1
    #subset data by site
    siteenvdata <- F.OTB_CC[F.OTB_CC$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.meanDPC = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      dens.weight.mean.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],13]
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.weight.mean = 10^(predict(FDWM.lmer, newdata = .))) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001,
                                            dens.weight.mean > denscomp.max ~ denscomp.max, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.weight.meanDPC = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.meanDPC = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                               TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.meanDPC
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.weight.mean
      
      
    }
    
    Fdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Fdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFdatalist[[t]] <- siteenvdataagg
  
}


#BigFData####
F_CC.PredOneTrueBae.DWM = bigFdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(F_CC.PredOneTrueBae.DWM,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBae.DWM.csv")

gc(bigFdatalist)
#Fresh WIP DWM####
bigFdatalist = list()

for (t in 1:20){ 
  #t = 2
  F.OTB_WIP = Fresh.OneBay_WIP[Fresh.OneBay_WIP$simnum_OB == t,] 
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 1
    #subset data by site
    siteenvdata <- F.OTB_WIP[F.OTB_WIP$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.meanDPC = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      dens.weight.mean.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],13]
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.weight.mean = 10^(predict(FDWM.lmer, newdata = .))) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001,
                                            dens.weight.mean > denscomp.max ~ denscomp.max, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.weight.meanDPC = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.meanDPC = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                               TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.meanDPC
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.weight.mean
      
      
    }
    
    Fdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Fdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFdatalist[[t]] <- siteenvdataagg
  
}


#BigFData####
F_WIP.PredOneTrueBae.DWM = bigFdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(F_WIP.PredOneTrueBae.DWM,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBae.DWM.csv")

gc(bigFdatalist)

