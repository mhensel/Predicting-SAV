#cleanest climate projection code imaginable in order to remotely run this on a windows path: 
library(tidyverse); library(lubridate); library(lme4); library(vroom);

#Converting mac paths to windows paths: 
# r:/Current Projects needs to be r:/Current Projects 

SAVCommDensWQ_69 = read.csv("r:/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69.csv")
SAVCommDensWQ_ForPred = vroom("r:/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_semForPredictions.csv")
SAVWQallClean = vroom("r:/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv") #200-2020
SAVCommunityDens_AllStations = vroom("r:/Current Projects/Predicting-SAV/data/communityDFs/SAVCommunityDens_AllStations.csv")

CBP.WQ_forPredictions = vroom("r:/Current Projects/Predicting-SAV/data/Water Quality/CBP.WQ_forPredictions.csv")

twentyone = vroom("r:/Current Projects/Predicting-SAV/data/Water Quality/twentyone.csv")
future_yrs = seq(2020, 2060, by = 1) #should this be 2021
#Initial Community Dataframes, and ME Models here####
#check the ENV vars and the replace_na for each community here!

#Zostera Initial and Model####
Zostera_initialDPC = SAVCommDensWQ_ForPred %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Zostera") %>%
  filter(!STATION %in% c("LE5.5-W")) %>% #seriously fuck this station
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.spme, TN.spme, Secc.summe, #customize these per community
         Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp))
#vector of zos_stations for the loop
zos_station = Zostera_initialDPC$STATION

#DF and model for Zostera
ZoDensWQsem.No0_Predict = SAVWQallClean %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "Zostera") %>%
  filter(!STATION %in% c("LE5.5-W")) %>% #seriously fuck this station
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.spme, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

ZoInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)) + (1|STATION), data = ZoDensWQsem.No0_Predict)

#Ruppia Initial and Model####
Ruppia_initialDPC = SAVCommDensWQ_ForPred %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Ruppia") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) 

ru_station = Ruppia_initialDPC$STATION

#load in Ru model and dataa
RuDensWQsem.No0_Predict = SAVWQallClean %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "Ruppia") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Temp.spme, Chla.spme, Sal.spme, TP.spme, TN.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

RuInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(Chla.spme) + 
                     log10(TP.spme) + log10(TN.spme) + log10(Temp.spme) +
                     (dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(Chla.spme)) + 
                     (log10(TP.spme):dens.percomp.y1) + (log10(TN.spme):dens.percomp.y1) + 
                     (log10(Temp.spme):dens.percomp.y1) + (1|STATION), 
                   data = RuDensWQsem.No0_Predict)

#MixedMeso Initial and Model####
MixMeso_initialDPC = SAVCommDensWQ_ForPred %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "MixedMeso") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) 

MM_station = MixMeso_initialDPC$STATION

MMDensWQsem.No0_Predict = SAVWQallClean %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "MixedMeso") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  drop_na() %>% #161
  as.data.frame()

MMInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(TN.summe) + log10(Chla.summe) + log10(TP.summe) +log10(Temp.summin) +log10(Sal.sumy1max):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1 +log10(TN.summe):dens.percomp.y1+ log10(Temp.summin):dens.percomp.y1 + (1|STATION), data = MMDensWQsem.No0_Predict)

#Fresh Initial and Model####
Fresh_initialDPC = SAVCommDensWQ_ForPred %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Fresh") %>%
  filter(year == "2020") %>% 
  filter(!STATION == "PIS0033") %>% #apparently this station aint in the Projecteddata
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp))

F_station = Fresh_initialDPC$STATION


FreshDensWQsem.No0_Predict = SAVWQallClean %>% #allclean is the 2000-2020 data! 
  filter(SpCluster == "Fresh") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>%
  drop_na() %>% #161
  mutate(Sal.summe = Sal.summe + .1) %>%
  as.data.frame()

FInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 +log10(Sal.summe) + log10(Chla.summe) + log10(TP.summe)  + log10(Temp.sumy1me) +log10(Temp.summe)  + log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1  +log10(Temp.sumy1me):dens.percomp.y1 + (1|STATION), data = FreshDensWQsem.No0_Predict)


#
##
###
####

OB_od = vroom("r:/Current Projects/Predicting-SAV/data/communityDFs/FutureMatrix.csv")

#OneTrueBay_CC Simulations#####
Zostera.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CC.csv")
Ruppia.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CC.csv")
MixMeso.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CC.csv")
Fresh.OneBay_CC = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CC.csv")
#IntoTheOneTrueMultiverse 1: Zostera OneTrueBay CC####
bigZodatalist = list()

for (t in 1:100){ 
  #t = 2
  Zos.OTB_CC = Zostera.OneBay_CC[Zostera.OneBay_CC$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_CC[Zos.OTB_CC$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],12] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.percomp.change
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.weight.mean
      
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


#BigZosData####
Zo_CC.PredOneTrueBae = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_CC.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigZodatalist)

#IntoTheOneTrueMultiverse 2: Ruppia OneTrueBay CC####
bigRudatalist = list()

for (t in 1:100){ 
  #t = 2
  Ru.OTB_CC = Ruppia.OneBay_CC[Ruppia.OneBay_CC$simnum_OB == t,] 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Ru.OTB_CC[Ru.OTB_CC$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
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


#BigRuData####
Ru_CC.PredOneTrueBae = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_CC.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigRudatalist)

#IntoTheOneTrueMultiverse 3: MixMeso OneTrueBay CC####
bigMMdatalist = list()

for (t in 1:100){ 
  #t = 2
  MM.OTB_CC = MixMeso.OneBay_CC[MixMeso.OneBay_CC$simnum_OB == t,] 
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- MM.OTB_CC[MM.OTB_CC$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}


#BigMMData####
MM_CC.PredOneTrueBae = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_CC.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_CC.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigMMdatalist)

#IntoTheOneTrueMultiverse 4: Fresg OneTrueBay CC####
bigFdatalist = list()

for (t in 1:100){ 
  #t = 2
  F.OTB_CC = Fresh.OneBay_CC[Fresh.OneBay_CC$simnum_OB == t,] 
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- F.OTB_CC[F.OTB_CC$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
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
F_CC.PredOneTrueBae = bigFdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(F_CC.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigFdatalist)








#OneTrueBay_WIP Simulations####
Zostera.OneBay_WIP = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_WIP.csv")
Ruppia.OneBay_WIP = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_WIP.csv")
MixMeso.OneBay_WIP = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_WIP.csv")
Fresh.OneBay_WIP = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_WIP.csv")
#IntoTheOneTrueMultiverse 1: Zostera OneTrueBay WIP####
bigZodatalist = list()

for (t in 1:100){ 
  #t = 2
  Zos.OTB_WIP = Zostera.OneBay_WIP[Zostera.OneBay_WIP$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_WIP[Zos.OTB_WIP$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],12] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.percomp.change
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.weight.mean
      
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


#BigZosData####
Zo_WIP.PredOneTrueBae = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_WIP.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigZodatalist)

#IntoTheOneTrueMultiverse 2: Ruppia OneTrueBay WIP####
bigRudatalist = list()

for (t in 1:100){ 
  #t = 2
  Ru.OTB_WIP = Ruppia.OneBay_WIP[Ruppia.OneBay_WIP$simnum_OB == t,] 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Ru.OTB_WIP[Ru.OTB_WIP$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
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


#BigRuData####
Ru_WIP.PredOneTrueBae = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_WIP.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigRudatalist)

#IntoTheOneTrueMultiverse 3: MixMeso OneTrueBay WIP####
bigMMdatalist = list()

for (t in 1:100){ 
  #t = 2
  MM.OTB_WIP = MixMeso.OneBay_WIP[MixMeso.OneBay_WIP$simnum_OB == t,] 
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- MM.OTB_WIP[MM.OTB_WIP$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}


#BigMMData####
MM_WIP.PredOneTrueBae = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_WIP.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigMMdatalist)

#IntoTheOneTrueMultiverse 4: Fresg OneTrueBay WIP####
bigFdatalist = list()

for (t in 1:100){ 
  #t = 2
  F.OTB_WIP = Fresh.OneBay_WIP[Fresh.OneBay_WIP$simnum_OB == t,] 
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- F.OTB_WIP[F.OTB_WIP$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
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
F_WIP.PredOneTrueBae = bigFdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(F_WIP.PredOneTrueBae,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigFdatalist)












#
##
###
####SEA LEVEL RISE####
##
#

#SLR dataset:####
SLRadj = vroom("r:/Current Projects/Predicting-SAV/data/SAV Habitat Change due to SLR/SLR Adjusted Denscompmax.csv")

#OneTrueBay_CC SLR Simulations DFs#####
Zostera.OneBay_CCSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CCSLR.csv")
Ruppia.OneBay_CCSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CCSLR.csv")
MixMeso.OneBay_CCSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CCSLR.csv")
Fresh.OneBay_CCSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CCSLR.csv")
#Run the MVerse here: OneTrueBay CC SLR####
#IntoTheOneTrueMultiverse 1: Zostera OneTrueBay CC SLR####
bigZodatalist = list()

for (t in 1:100){ 
  #t = 2
  Zos.OTB_CC = Zostera.OneBay_CCSLR[Zostera.OneBay_CCSLR$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_CC[Zos.OTB_CC$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],12] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.percomp.change
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.weight.mean
      
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


#BigZosData####
Zo_CC.PredOneTrueBaeSLR = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_CC.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigZodatalist)

#IntoTheOneTrueMultiverse 2: Ruppia OneTrueBay CC SLR####
bigRudatalist = list()

for (t in 1:100){ 
  #t = 2
  Ru.OTB_CC = Ruppia.OneBay_CCSLR[Ruppia.OneBay_CCSLR$simnum_OB == t,] 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Ru.OTB_CC[Ru.OTB_CC$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
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


#BigRuData####
Ru_CC.PredOneTrueBaeSLR = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_CC.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigRudatalist)

#IntoTheOneTrueMultiverse 3: MixMeso OneTrueBay CC SLR####
bigMMdatalist = list()

for (t in 1:100){ 
  #t = 2
  MM.OTB_CC = MixMeso.OneBay_CCSLR[MixMeso.OneBay_CCSLR$simnum_OB == t,] 
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- MM.OTB_CC[MM.OTB_CC$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}


#BigMMData####
MM_CC.PredOneTrueBaeSLR = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_CC.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_CC.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigMMdatalist)

#IntoTheOneTrueMultiverse 4: Fresg OneTrueBay CC####
bigFdatalist = list()

for (t in 1:100){ 
  #t = 2
  F.OTB_CC = Fresh.OneBay_CCSLR[Fresh.OneBay_CCSLR$simnum_OB == t,] 
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- F.OTB_CC[F.OTB_CC$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
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
F_CC.PredOneTrueBaeSLR = bigFdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(F_CC.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigFdatalist)









#Run the MVerse here: OneTrueBay WIP SLR####

Zostera.OneBay_WIPSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_WIPSLR.csv")
Ruppia.OneBay_WIPSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_WIPSLR.csv")
MixMeso.OneBay_WIPSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_WIPSLR.csv")
Fresh.OneBay_WIPSLR = vroom("r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_WIPSLR.csv")



#IntoTheOneTrueMultiverse 1: Zostera OneTrueBay WIP SLR####
bigZodatalist = list()

for (t in 1:100){ 
  #t = 2
  Zos.OTB_WIP = Zostera.OneBay_WIPSLR[Zostera.OneBay_WIPSLR$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_WIP[Zos.OTB_WIP$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],12] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.percomp.change
      siteenvdata[y,15] <- CurrentYrPredict[1,15] #dens.weight.mean
      
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


#BigZosData####
Zo_WIP.PredOneTrueBaeSLR = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_WIP.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigZodatalist)

#IntoTheOneTrueMultiverse 2: Ruppia OneTrueBay WIP SLR####
bigRudatalist = list()

for (t in 1:100){ 
  #t = 2
  Ru.OTB_WIP = Ruppia.OneBay_WIPSLR[Ruppia.OneBay_WIPSLR$simnum_OB == t,] 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Ru.OTB_WIP[Ru.OTB_WIP$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
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


#BigRuData####
Ru_WIP.PredOneTrueBaeSLR = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_WIP.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigRudatalist)

#IntoTheOneTrueMultiverse 3: MixMeso OneTrueBay WIP SLR####
bigMMdatalist = list()

for (t in 1:100){ 
  #t = 2
  MM.OTB_WIP = MixMeso.OneBay_WIPSLR[MixMeso.OneBay_WIPSLR$simnum_OB == t,] 
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- MM.OTB_WIP[MM.OTB_WIP$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}


#BigMMData####
MM_WIP.PredOneTrueBaeSLR = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_WIP.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_WIP.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigMMdatalist)

#IntoTheOneTrueMultiverse 4: Fresg OneTrueBay WIP####
bigFdatalist = list()

for (t in 1:100){ 
  #t = 2
  F.OTB_WIP = Fresh.OneBay_WIPSLR[Fresh.OneBay_WIPSLR$simnum_OB == t,] 
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- F.OTB_WIP[F.OTB_WIP$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
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
F_WIP.PredOneTrueBaeSLR = bigFdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(F_WIP.PredOneTrueBaeSLR,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_WIP.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigFdatalist)







