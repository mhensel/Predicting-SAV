#Applying SLR to the Projected data
library(tidyverse); library(vroom); library(lme4); 


#OldData
SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv") 

#SLR data (NO ACCRETION YET)

SLR_byStation = readxl::read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/SAV Habitat Change due to SLR/SAV possible habitat reduction due to SLR by station.xlsx", sheet = 3)

SLR_byStation_raw = readxl::read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/SAV Habitat Change due to SLR/SAV possible habitat reduction due to SLR by station.xlsx", sheet = 2)

FutureYears = as_tibble(seq(from = 2021, to = 2060, by = 1)) %>% rename("Year" = "value")
#Build changing denscomp dataset: 

SLRadj = SLR_byStation %>% 
  rename(year = ...1) %>%
  pivot_longer(cols = !year, names_to = "STATION", values_to = "per.HAloss") %>% 
  full_join(SAVWQallClean %>% filter(year == 2020))  %>%
  select(year, STATION, SpCluster, denscomp.max, per.HAloss) %>%
  group_by(STATION) %>% 
  fill(denscomp.max, SpCluster) %>%
  drop_na() %>% #drops the stations that dont have SAV
  group_by(STATION, year, SpCluster) %>%
  mutate(denscomp.SLRadj = denscomp.max + (denscomp.max*per.HAloss)) %>%  #note: its adding bc HA loss is -
  #filter(!year == 2020) %>%
  ungroup() 

vroom_write(SLRadj, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/SAV Habitat Change due to SLR/SLR Adjusted Denscompmax.csv")

SLR_byStation_raw = SLR_byStation_raw[-1,]
SLRadj.raw = SLR_byStation_raw %>% 
  pivot_longer(cols = !STATION, names_to = "STA", values_to = "HAloss") %>%
  rename(year = STATION, STATION = STA) %>% mutate(year = as.double(year)) #%>%
  #full_join(SAVWQallClean %>% filter(year == 2020)) %>%
 # select(year, STATION, SpCluster, HAloss) %>%

#Original Initial DFs: ####
SAVCommDensWQ_ForPred = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_semForPredictions.csv")
SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv") #200-2020
SAVCommunityDens_AllStations = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommunityDens_AllStations.csv")
CBP.WQ_forPredictions = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBP.WQ_forPredictions.csv")
twentyone = CBP.WQ_forPredictions %>%  #twentyone should work for all Communities
  filter(year %in% c("2020", "2019", "2018", "2017")) %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  summarize(across(Temp.summed:Sal.summax, ~mean(., na.rm = T))) %>%
  mutate(year = 2021) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION, Year = year) %>%
  select(Station, Year, everything()) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #LE5.5 doenst have temp, TFs and RET5.1A dont have salinities
  ungroup() 
#future years, used in loop
future_yrs = seq(2020, 2060, by = 1) #should this be 2021

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


#Should just be able to plop that in and replace denscomp.max with denscomp.SLRadj


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


#ONEBAY datasets####
OB_od = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/FutureMatrix.csv")

#SLR dataset:####
SLRadj = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/SAV Habitat Change due to SLR/SLR Adjusted Denscompmax.csv")
#Input datasets (are written!)
CC.wland_OneFuture = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.csv")
#Zostera.OneBay_CCSLR####
Zostera.OneBay_CCSLR = CC.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Zostera_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, 
         Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, Zostera_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>% 
  ungroup()  %>% 
  full_join(SLRadj %>% filter(SpCluster == "Zostera") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)#%>%
#nest_by(simnum_OB) %>%
#filter(simnum_OB %in% 1:5) 
#nest_by(simnum_OB) #i dont think this nesting is necessary but it loks nice
vroom_write(Zostera.OneBay_CCSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CCSLR.csv")

#Ruppia.OneBay_CCSLR####
Ruppia.OneBay_CCSLR = CC.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Ruppia_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>%
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, Ruppia_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>%
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "Ruppia") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)

vroom_write(Ruppia.OneBay_CCSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CCSLR.csv")

#MixMeso.OneBay_CCSLR####
MixMeso.OneBay_CCSLR = CC.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% MixMeso_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, MixMeso_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>%
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "MixMeso") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)

vroom_write(MixMeso.OneBay_CCSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CCSLR.csv")

#Fresh.OneBay_CCSLR####
Fresh.OneBay_CCSLR = CC.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Fresh_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>%
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, Fresh_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  mutate(Sal.summe = Sal.summe + .001) %>%
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>%
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "Fresh") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)

vroom_write(Fresh.OneBay_CCSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CCSLR.csv")

#OneTrueBay_CC SLR Simulations DFs#####
Zostera.OneBay_CCSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CCSLR.csv")
Ruppia.OneBay_CCSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CCSLR.csv")
MixMeso.OneBay_CCSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CCSLR.csv")
Fresh.OneBay_CCSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CCSLR.csv")
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

vroom_write(Zo_CC.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBaeSLR.csv")

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

vroom_write(Ru_CC.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBaeSLR.csv")

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

vroom_write(MM_CC.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_CC.PredOneTrueBaeSLR.csv")

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

vroom_write(F_CC.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigFdatalist)






#OneTrueBay_WIP SLR Simulations#####
WIP.wland_OneFuture = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/WIP.wland_OneFuture.csv")
#Zostera.OneBay_WIPSLR####
Zostera.OneBay_WIPSLR = WIP.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Zostera_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, 
         Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, Zostera_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>% 
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "Zostera") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)
#nest_by(simnum_OB) %>%
#filter(simnum_OB %in% 1:5) 
#nest_by(simnum_OB) #i dont think this nesting is necessary but it loks nice
vroom_write(Zostera.OneBay_WIPSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_WIPSLR.csv")

#Ruppia.OneBay_WIPSLR####
Ruppia.OneBay_WIPSLR = WIP.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Ruppia_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>%
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, Ruppia_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>%
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "Ruppia") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)

vroom_write(Ruppia.OneBay_WIPSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_WIPSLR.csv")

#MixMeso.OneBay_WIPSLR####
MixMeso.OneBay_WIPSLR = WIP.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% MixMeso_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, MixMeso_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>%
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "MixMeso") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year) %>%
  drop_na(simnum_OB)

vroom_write(MixMeso.OneBay_WIPSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_WIPSLR.csv")

#Fresh.OneBay_WIPSLR####
Fresh.OneBay_WIPSLR = WIP.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Fresh_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>%
  nest_by(year, Year.ref) %>% 
  drop_na() %>% #one set of sims uses 2020 as a reference for 2021 and it comes up as NA
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
  select(simnum_OB, year, everything())  %>% 
  ungroup() %>%
  select(-Year.ref) %>% 
  full_join(bind_rows(replicate(100, Fresh_initialDPC, simplify = F)) %>%  #new for ONEBAY
              group_by(STATION) %>% 
              mutate(simnum_OB = seq(1, 100))) %>% 
  mutate(Sal.summe = Sal.summe + .001) %>%
  group_by(STATION, year) %>%
  arrange(STATION, year) %>% 
  group_by(STATION) %>%
  fill(denscomp.max) %>%
  ungroup() %>%
  full_join(SLRadj %>% filter(SpCluster == "Fresh") %>% select(STATION, denscomp.SLRadj, year), 
            by = c("STATION", "year")) %>%
  select(-denscomp.max) %>% 
  rename(denscomp.max = denscomp.SLRadj) %>%
  arrange(simnum_OB, STATION, year)%>%
  drop_na(simnum_OB)

vroom_write(Fresh.OneBay_WIPSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_WIPSLR.csv")


#Run the MVerse here: OneTrueBay WIP SLR####

Zostera.OneBay_WIPSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_WIPSLR.csv", col_types = c(TP.spmed = "d"))
Ruppia.OneBay_WIPSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_WIPSLR.csv")
MixMeso.OneBay_WIPSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_WIPSLR.csv")
Fresh.OneBay_WIPSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_WIPSLR.csv")



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

vroom_write(Zo_WIP.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBaeSLR.csv")

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

vroom_write(Ru_WIP.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBaeSLR.csv")

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

vroom_write(MM_WIP.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_WIP.PredOneTrueBaeSLR.csv")

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

vroom_write(F_WIP.PredOneTrueBaeSLR,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_WIP.PredOneTrueBaeSLR.csv")

#biglist, not needed anymore.
gc(bigFdatalist)











######
######
#SLR Figures####
#Create ZCC, RCC, MCC, FCC: Summarize CC.wland_Predicts Area####
ZCC_SLR = Zo_CC.PredOneTrueBaeSLR %>% 
  add_column(., SpCluster = "Zostera") %>%
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
RCC_SLR = Ru_CC.PredOneTrueBae %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
MCC_SLR = MM_CC.PredOneTrueBae %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
FCC_SLR = F_CC.PredOneTrueBae %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

#allcomm_CC.df: All CC.wland Communities bound####
allcomm_CC.dfSLR = ZCC_SLR %>% bind_rows(RCC_SLR) %>% bind_rows(MCC_SLR) %>% 
  bind_rows(FCC_SLR) %>% 
  filter(!year %in% c("2020")) %>% ungroup()

#ZWIP, RWIP,: Summarize WIP.wland_Predicts Area####
ZWIP_SLR = Zo_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "Zostera") %>%
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

RWIP_SLR = Ru_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

MWIP_SLR = MM_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

FWIP_SLR = F_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))



allcomm_WIP.wl.dfSLR = ZWIP_SLR %>% full_join(RWIP_SLR) %>% full_join(MWIP_SLR) %>% 
  full_join(FWIP_SLR) %>% filter(!year %in% c("2020")) %>% ungroup()

#Allcomm_past df ####
allcomm_past.df = SAVWQallClean %>% #rename("year" = "Year") %>% 
  group_by(SpCluster, year) %>%
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(SAVArea, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  filter(!SpCluster == "RuZo") %>% filter(!year == 1984) %>% ungroup()

allcomm_pastWQ.df = SAVWQallClean %>% #rename("year" = "Year") %>% 
  group_by(SpCluster, year) %>%
  summarize(across(Temp.sumy1med:TN.summe, ~mean(.x, na.rm = T))) %>%
  filter(!SpCluster == "RuZo") %>% filter(!year == 1984) %>% ungroup()

allcomm_past85.df = SAVWQallClean_85 %>% #rename("year" = "Year") %>% 
  group_by(SpCluster, year) %>%
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(SAVArea, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  filter(!SpCluster == "RuZo") %>% filter(!year == 1984) %>% ungroup()

allcomm_pastWQ85.df = SAVWQallClean_85 %>% #rename("year" = "Year") %>% 
  group_by(SpCluster, year) %>%
  summarize(across(Chla.spme:Temp.sumy1me, ~mean(.x, na.rm = T))) %>%
  filter(!SpCluster == "RuZo") %>% filter(!year == 1984) %>% ungroup()

#pastSAV and WQ by SpCluster####
allcomm_pastSAVWQ.df = full_join(allcomm_past.df, allcomm_pastWQ.df)
allcomm_pastSAVWQ85.df = full_join(allcomm_past85.df, allcomm_pastWQ85.df)
#allcomm_past.df = SAVCommunityDens_AllStations %>% rename("year" = "Year") %>% 
#  group_by(SpCluster, year, Station) %>%
#  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
#            Area.total = sum(SAVArea, na.rm = T), 
#            DPC.mean = mean(dens.percomp, na.rm = T), 
#            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
#  filter(!SpCluster == "RuZo") %>% ungroup()






#Visualize SLR: 
ggplot(data = SLRadj.raw) + #%>% filter(STATION %in% c("CB1.1"))) + 
  stat_summary(aes(x = year, y = HAloss, group = STATION), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = .8, alpha = .5) +
  labs(y = expression(paste("Maximum Composite Potential Habitat (HA/Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_discrete(breaks=seq(2020, 2060, 10)) +
 # scale_y_continuous(limits = c(3500, 5000)) +
#  scale_color_manual(values = c("darkgoldenrod1", "darkorchid","brown2", "chartreuse2")) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")


