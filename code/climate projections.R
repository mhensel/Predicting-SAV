#Code for Climate Projections of Chesapeake Bay SAV communities#
library(tidyverse); library(janitor); library(lme4); library(car); library(vroom); library(performance)

#NOTES: ####
#As of 5/12/22: Start loading data with line 20 to build all the "past" dataframes and models we need for projections. Actual projecting is done with those DFs and the .OneBays 
#List of DFs needed to run projections: 
#Past:
#SAVCommDensWQ_ForPred = past (1984-2020) water quality and SAV data (actually only use 2020 from this DF and might adjust the need for this DF later, but whatever)
#SAVWQallClean = "modern" past (2000-2020) water quality and SAV data. Use this for the initial LMER model to begin predictions

#Future: 
#CommunityName.OneBay_CC or CommunityName.OneBay_WIP  (i.e., "Zostera.OneBay_WIP)
#Future (2020-2060) Zostera, Ruppia, MixMeso, Fresh environmental variable DF for each of the 2 scenarios, by simulation number Station and Year. this DF should have just the environmental variables needed for the LMER of each community, a col for dens.percomp (2020 density, % of composite max area), and denscomp.max (maximum density per composite area)

#Other Notes: ####
#most of the dataframes here were built in /code/assemble climate data.R
#this code was originally copy-pasted from the successful code/CC_wl projections clean.R, but the workflow changed a bit to accomodate the ONEBAY data.
#Models used originally come from the SEMs in code/community change model building.R Will need to make sure the past data from those models are available too in the code below.

####DFs needed for ALL loops but not in loop####
##SAV Density and WQ per Community over past time#
#SAVCommDensWQ_69sem.No0 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69sem.No0.csv")

SAVCommDensWQ_ForPred = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_ForPredictions.csv") #past water quality and sav data 
SAVCommDensWQ_ForPred = vroom("data/SAVCommDensWQ_forPredictions.csv") #github paths


SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv") #2000-2020 water quality and sav data, medians filling in NAs in order to feed properly into the models
SAVWQallClean = vroom("data/SAVWQallClean.csv") #GitHub path...

#twentyone: we need to generate y1 Temp and Sal for some models but the projected data doesnt have that. So we use 2020 real data
#twentyone = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/twentyone.csv") I dont think we need this anymore bc its baked into the .OneBay
#vector of all the future years, need this in the projection loop####
future_yrs = seq(2020, 2060, by = 1) #should this be 2021

#Initial Community Dataframes, and ME Models here, Run all this code!!####
#Build two of the key past DFs, and some important vectors for each communnity

#Zostera Initial and Model####
Zostera_initialDPC = SAVCommDensWQ_ForPred %>% #note to MH: investigate why this diff than allClean
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
ZoDensWQsem.No0_Predict = SAVCommDensWQ_69sem.No0 %>% #allclean is the 2000-2020 data! 
  filter(between(year, 2000, 2020)) %>%
  filter(SpCluster == "Zostera") %>%
  filter(!STATION %in% c("LE5.5-W")) %>% #seriously fuck this station
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max, dens.percomp, 
         Chla.spme, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

ZoInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1  + 
                     log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + 
                     (dens.percomp.y1:log10(Temp.spmed)) +  
                     (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)) + (1|STATION), data = ZoDensWQsem.No0_Predict)
r2(ZoInt.lmer)

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
r2(RuInt.lmer)
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

r2(MMInt.lmer)
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

FInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + 
                    log10(Sal.summe) + log10(Chla.summe) + log10(TP.summe)  + 
                    log10(Temp.sumy1me) +log10(Temp.summe)  + 
                    log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + 
                    log10(TP.summe):dens.percomp.y1  +log10(Temp.sumy1me):dens.percomp.y1 + 
                    (1|STATION), data = FreshDensWQsem.No0_Predict)
r2(FInt.lmer)
check_singularity(FInt.lmer)

#
##
###
####

OB_od = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/FutureMatrix.csv")
######SCENARIO 1 Climate Change, No Nutrient Reduction Scenario: CC_wland, Skip to line ~302 to load data####
####
###
##
#

#Scenario Global DF, can skip to .OneBay_CC####
#This is same for all CC.wlands. Replace for each scenario
CC.wlAllFut_ONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wlAllFutsmoodetre_ONEBAY.csv")

CC.wland_OneFuture.no2021 = CC.wlAllFut_ONEBAY %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#CC.wland_OneFuture, actually all futures ;)####
CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(CC.wland_One2021) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

#vroom_write(CC.wland_OneFuture, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.csv")

#CC.wland_OneFuture = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.csv")

#Zostera.OneBay_CC####
Zostera.OneBay_CC = CC.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Zostera_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, #TP.spmed, 
         TN.spme, Secc.summe, 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>%
#nest_by(simnum_OB) %>%
#filter(simnum_OB %in% 1:5) 
#nest_by(simnum_OB) #i dont think this nesting is necessary but it loks nice
vroom_write(Zostera.OneBay_CC,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CC.csv")

#Ruppia.OneBay_CC####
Ruppia.OneBay_CC = CC.wland_OneFuture %>% 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>% filter(simnum_OB %in% 1:5) 

vroom_write(Ruppia.OneBay_CC,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CC.csv")

#MixMeso.OneBay_CC####
MixMeso.OneBay_CC = CC.wland_OneFuture %>% 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>% filter(simnum_OB %in% 1:5) 

vroom_write(MixMeso.OneBay_CC,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CC.csv")

#Fresh.OneBay_CC####
Fresh.OneBay_CC = CC.wland_OneFuture %>% 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>% filter(simnum_OB %in% 1:5)

vroom_write(Fresh.OneBay_CC,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CC.csv")

#OneTrueBay_CC Simulations, LOAD ME!#####
Zostera.OneBay_CC = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_CC.csv")
Ruppia.OneBay_CC = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_CC.csv")
MixMeso.OneBay_CC = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_CC.csv")
Fresh.OneBay_CC = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_CC.csv")

#github path = data/Zostera.OneBay_CC.csv
#IntoTheOneTrueMultiverse 1: Zostera OneTrueBay CC####
#ADAPTIVE MODELLING APPLIED TO THIS LOOP!#####
bigZodatalist = list() #we will populate this list

#Outermost loop, run every simulation (simnum_OB) one at a time
for (t in 1:5){ #this will be 1:100 or 1:1000 but 5 is enough for now
 # t = 2
  Zos.OTB_CC = Zostera.OneBay_CC[Zostera.OneBay_CC$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #middle loop, run individual stations one at a time, builds "siteenvdata" which is a stations environmental data, into the future, for each simulation
   #  s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_CC[Zos.OTB_CC$STATION == zos_station[s],] 
    siteenvdata$dens.percomp.change = NA  #build a dens.percomp.change column, this is what is predicted by our LMER
    siteenvdata$dens.weight.mean = NA #dens.weright.mean gets back calculated
    siteenvdata <- as.data.frame(siteenvdata)
    datapred.y = ZoDensWQsem.No0_Predict  #here is the dataframe that will be updated as part of the adaptive modelling
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2. If I could get accumulate2() to start on the second row, wouldnt need this bullshit
   #     y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr

      #predict the year (y) based on thisyearinfo!
      CurrentYrPredict <- thisyrinfo %>% 
      #  select(-simnum_OB) %>%
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>% #predict change based on lmer
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>% #calculate density percomposite area from change
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, #No 0s allowed
                                        dens.percomp > 1 ~ 1.0001, #No densities allowed over 100%, i.e., bigger than we've ever seen before
                                        TRUE ~ dens.percomp)) %>% #no 
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>% #calculate dens.weight.mean
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean)) %>% #no 0s allowed for dens.weight.mean, although i dont think this matters
        mutate(dens.percomp.y1 = dens.percomp.y1) #put last year's density into this DF bc we need it for adaptive modelling adjustment
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean

      #ok now also bind that newest year to the datapred.y, and slice off the 20 most recent years!      
  datapred.y = datapred.y %>% #Trying adaptive modelling here####
   bind_rows(CurrentYrPredict) %>% 
    group_by(STATION) %>%
    arrange(desc(year)) %>%
    slice_head(n = 20) %>% #select the 20 most recent years
    ungroup()
    
  #ADAPTIVE MODEL! update the ZoInt.lmer model to have been fed the newest years
      ZoInt.lmer <- lm(dens.percomp.change ~ dens.percomp.y1  +  log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) +  (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ 
      (dens.percomp.y1:log10(Secc.summe)), data = datapred.y)
      #i switched over to the lm because the random effect is STATION
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1])) #will We  need to bring this back?
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}



#BigZosData####
Zo_CC.PredOneTrueBae = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_CC.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBae.csv")

#test figs
ggplot(data = Zo_CC.PredOneTrueBae)+ #%>% filter(between(simnum_OB, 1, 3)) ) +
  #geom_point(aes(x = year, y = dens.percomp.change, group = STATION, color = simnum_OB), position = position_jitter()) +
  stat_summary(data = ZCC.otbNonAdapt %>% filter(between(simnum_OB, 1, 2)),
               aes(x = year, y = dens.percomp, group = STATION), geom = "line", size = .9, color = "darkgreen") +
 # stat_smooth(data = ZCC.otbNonAdapt %>% filter(between(simnum_OB, 1, 2)),
 #             aes(x = year, y = dens.percomp.change, group = simnum_OB), method = "lm", color = "blue") +
  stat_summary(aes(x = year, y = dens.percomp, group = STATION), geom = "line", size = .9, color = "brown2") +
  # stat_smooth(aes(x = year, y = dens.percomp.change, group = simnum_OB), method = "lm", color = "black") +
  stat_summary(data = ZoDensWQsem.No0_Predict, aes(x = year, y = dens.percomp, group = STATION), geom = "line", size = .9) #+
  #stat_smooth(data = ZoDensWQsem.No0_Predict, aes(x = year, y = dens.percomp.change, group = STATION), method = "lm") 

ZCC.otbNonAdapt = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBae.csv")

#SO: the adaptive modelling works best with using the original _69 WQ data. the problem might be the spring but its hard to say
#I'm going to try growing season now with the allclean

#biglist, not needed anymore.
gc(bigZodatalist)

#No more Adaptive modelling after this...yet (5/24/22)####
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

vroom_write(Ru_CC.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBae.csv")

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

vroom_write(MM_CC.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_CC.PredOneTrueBae.csv")

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

vroom_write(F_CC.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigFdatalist)








######SCENARIO 2 WIP_wland, Skip to line 743####
####
###
##
#

#Do OneBay_WIP next####
WIP.wland_OneFuture = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/WIP.wland_OneFuture.csv")
#Zostera.OneBay_WIP####
Zostera.OneBay_WIP = WIP.wland_OneFuture %>% 
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Zostera_initialDPC$STATION) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, #TP.spmed, 
         TN.spme, Secc.summe, 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>%
#nest_by(simnum_OB) %>%
#filter(simnum_OB %in% 1:5) 
#nest_by(simnum_OB) #i dont think this nesting is necessary but it loks nice
vroom_write(Zostera.OneBay_WIP,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_WIP.csv")

#Ruppia.OneBay_WIP####
Ruppia.OneBay_WIP = WIP.wland_OneFuture %>% 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>% filter(simnum_OB %in% 1:5) 

vroom_write(Ruppia.OneBay_WIP,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_WIP.csv")

#MixMeso.OneBay_WIP####
MixMeso.OneBay_WIP = WIP.wland_OneFuture %>% 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>% filter(simnum_OB %in% 1:5) 

vroom_write(MixMeso.OneBay_WIP,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_WIP.csv")

#Fresh.OneBay_WIP####
Fresh.OneBay_WIP = WIP.wland_OneFuture %>% 
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) #%>% filter(simnum_OB %in% 1:5)

vroom_write(Fresh.OneBay_WIP,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_WIP.csv")

#OneTrueBay_WIP Simulations#####
Zostera.OneBay_WIP = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zostera.OneBay_WIP.csv")
Ruppia.OneBay_WIP = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ruppia.OneBay_WIP.csv")
MixMeso.OneBay_WIP = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MixMeso.OneBay_WIP.csv")
Fresh.OneBay_WIP = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Fresh.OneBay_WIP.csv")

#IntoTheOneTrueMultiverse 1: Zostera OneTrueBay WIP####
bigZodatalist = list()

bigZodatalist = list()

#adaptive test. Trying adaptive modelling on the Zos WIP but havent finished yet 5/24/22####
for (t in 1:5){ 
  # t = 2
  Zos.OTB_WIP = Zostera.OneBay_WIP[Zostera.OneBay_WIP$simnum_OB == t,] 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #  s = 3
    #subset data by site
    siteenvdata <- Zos.OTB_WIP[Zos.OTB_WIP$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    datapred.y = ZoDensWQsem.No0_Predict #%>%  #here
    # select(year, STATION, Chla.spme, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme, denscomp.max, dens.percomp.change, dens.weight.mean) 
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #     y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        #  select(-simnum_OB) %>%
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean)) %>%
        mutate(dens.percomp.y1 = dens.percomp.y1)
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
      datapred.y = datapred.y %>% #Trying adaptive modelling here####
      bind_rows(CurrentYrPredict) %>% 
        group_by(STATION) %>%
        arrange(desc(year)) %>%
        slice_head(n = 20) %>% #select the 20 most recent years
        ungroup()
      
      
      #Update thisyearinfo here, AND update the input into the mixed model
      #update dens.percomp.y1
      #drop the oldest year from the datapred.y
      
      ZoInt.lmer <- lm(dens.percomp.change ~ dens.percomp.y1  +  log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) +  (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ 
                         (dens.percomp.y1:log10(Secc.summe)), data = datapred.y)
      #i switched over to the lm because the random effect is STATION
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  #siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1])) #We will need to bring this back
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}

#original
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

qplot(data = Zo_WIP.PredOneTrueBae, x = year, y = dens.percomp.change, group = STATION, geom = "smooth")

vroom_write(Zo_WIP.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBae.csv")

ggplot(data = Zo_WIP.PredOneTrueBae)+ #%>% filter(between(simnum_OB, 1, 3)) ) +
  #geom_point(aes(x = year, y = dens.percomp.change, group = STATION, color = simnum_OB), position = position_jitter()) +
  stat_summary(data = ZWIP.otbNonAdapt %>% filter(between(simnum_OB, 1, 2)),
               aes(x = year, y = dens.percomp, group = STATION), geom = "line", size = .9, color = "darkgreen") +
  # stat_smooth(data = ZWIP.otbNonAdapt %>% filter(between(simnum_OB, 1, 2)),
  #             aes(x = year, y = dens.percomp.change, group = simnum_OB), method = "lm", color = "blue") +
  stat_summary(aes(x = year, y = dens.percomp, group = STATION), geom = "line", size = .9, color = "brown2") +
  # stat_smooth(aes(x = year, y = dens.percomp.change, group = simnum_OB), method = "lm", color = "black") +
  stat_summary(data = ZoDensWQsem.No0_Predict, aes(x = year, y = dens.percomp, group = STATION), geom = "line", size = .9) #+
#stat_smooth(data = ZoDensWQsem.No0_Predict, aes(x = year, y = dens.percomp.change, group = STATION), method = "lm") 

ZWIP.otbNonAdapt = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBae.csv")

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

vroom_write(Ru_WIP.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigRudatalist)
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

vroom_write(MM_WIP.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_WIP.PredOneTrueBae.csv")

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

vroom_write(F_WIP.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigFdatalist)



#END OF CURRENT WORKFLOW, although we need to get the next two simulations####


#FutureIsPast####
SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv")

SAVWQallClean_init = SAVWQallClean %>% filter(year %in% c(2018, 2019, 2020)) %>%
  group_by(STATION, SpCluster) %>%
  summarize(across(everything(), ~mean(.x))) %>% 
  mutate(year = 2020)

ZosFutureIsPast.df = SAVWQallClean %>% filter(SpCluster == "Zostera") %>%
  group_by(STATION, year) %>%
  slice_sample(., n = 40, replace = T, weight_by = year) %>%
  rename(Year.ref = year) %>%
  mutate(year = seq(2021, 2060, by = 1)) %>% 
  select(STATION, year, Year.ref,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, 
         Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
  ungroup() %>%
  nest_by(year, Year.ref) %>% 
  full_join(OB_od) %>% #new for ONEBAY
  arrange(simnum_OB) %>% #new for ONEBAY
  unnest(cols = c(data, year, Year.ref, simnum_OB)) %>%  
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
  drop_na(simnum_OB) %>% 
  arrange(simnum_OB, STATION, year) %>%
  select(simnum_OB, year, STATION, everything())


#Future is past 1: Zostera OneTrueBay WIP####
bigZodatalist = list()

for (t in 1:2){ 
  #t = 2
  Zos.OTB_WIP = ZosFutureIsPast.df[ZosFutureIsPast.df$simnum_OB == t,] 
  
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

qplot(data = Zo_WIP.PredOneTrueBae, x = year, y = dens.percomp.change, group = STATION, geom = "smooth")

vroom_write(Zo_WIP.PredOneTrueBae,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBae.csv")

#biglist, not needed anymore.
gc(bigZodatalist)







#
#
#
#

#
#
#
#
###
#### 
####
#OneBay style code that works but doesnt do the all common simulations thing:
#ALERT: This code corrects dens.comp over 1, and dwm over 1, too late, so fix that before useage####

######SCENARIO 1 CC_wland####
####
###
##
#

#Scenario Global DF####
#This is same for all CC.wlands. Replace for each scenario
CC.wlAllFut_ONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wlAllFutsmoodetre_ONEBAY.csv")

CC.wland_OneFuture.no2021 = CC.wlAllFut_ONEBAY %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#CC.wland_OneFuture, actually all futures ;)####
CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(CC.wland_One2021) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

#vroom_write(CC.wland_OneFuture, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.csv")

#MVERSE C1: Zostera CCwland Individual Community Loop####
bigZodatalist = list()

for (t in 1:100){ 
  
  Zos_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Zostera_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.spme, TP.spmed, TN.spme, Secc.summe, 
           Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
    nest_by(year, Year.ref) %>% 
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(Zostera_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup()   
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos_CC.wland_OneFuture[Zos_CC.wland_OneFuture$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}

#BigZosData####
Zo_CC.wland_PredONEBAY = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_CC.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Zo_CC.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigZodatalist)

##MVERSE C2: Ruppia CCwland Individual Community Loop####

bigRudatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  #everythnig above this is the same for all communities
  Ru_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Ruppia_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% #select the Ruppia WQ Variables
    nest_by(year, Year.ref) %>% #ok this might be it. how do i do math inzide of these fuckin nests
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(Ruppia_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- Ru_CC.wland_OneFuture[Ru_CC.wland_OneFuture$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],8] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 8 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 8 in CYP = dens.percomp | col 8 in siteenvdata
      #col 10 in CYP = dens.percomp.change | col 10 in siteenvdata 
      #col 11 in CYP = dens.weight.mean | col 11 in siteenvdata 
      
      siteenvdata[y,8] <- CurrentYrPredict[1,8] #dens.percomp
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp.change
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.weight.mean
      
      #add a case_when so no 0s?
      
    }
    
    Rudatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Rudatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRudatalist[[t]] <- siteenvdataagg
  
}

#BigRuData####

Ru_CC.wland_PredONEBAY = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_CC.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Ru_CC.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigRudatalist)

##MVERSE C3: MM CCwland Individual Community Loop####

bigMMdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  MM_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% MixMeso_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% #select the MixMeso WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    nest_by(year, Year.ref) %>% #ok this might be it. how do i do math inzide of these fuckin nests
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(MixMeso_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup() 
  
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- MM_CC.wland_OneFuture[MM_CC.wland_OneFuture$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 11 in CYP = dens.percomp.change | col 11 in siteenvdata 
      #col 12 in CYP = dens.weight.mean | col 12 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp 
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}

#Big MM data alert#####
MM_CC.wland_PredONEBAY = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_CC.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/MM_CC.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigMMdatalist)

##MVERSE C4: F CCwland Individual Community Loop####

bigFdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  F_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Fresh_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% #select the Fresh WQ Variables
    nest_by(year, Year.ref) %>% #ok this might be it. how do i do math inzide of these fuckin nests
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(Fresh_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    mutate(Sal.summe = Sal.summe + .001) %>%
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup() 
  
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    
    #subset data by site
    siteenvdata <- F_CC.wland_OneFuture[F_CC.wland_OneFuture$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 10 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      #col 10 in CYP = dens.percomp | col 10 in siteenvdata
      #col 12 in CYP = dens.percomp.change | col 12 in siteenvdata 
      #col 13 in CYP = dens.weight.mean | col 13 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp 
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    Fdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Fdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFdatalist[[t]] <- siteenvdataagg
  
}

#Big F_CC.wland_Predict alert#####
F_CC.wland_PredONEBAY = bigFdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

vroom_write(F_CC.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/F_CC.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigFdatalist)


######SCENARIO 2 WIP_wland####
####
###
##
#

#Scenario Global DF WIP.wland_OneFuture####
#This is same for all WIP.wlands. Replace for each scenario
WIP.wlAllFut_ONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wl.AllFutsmoodetre_ONEBAY.csv")

WIP.wland_OneFuture.no2021 = WIP.wlAllFut_ONEBAY %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#WIP.wland_OneFuture, actually all futures ;)####
WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(WIP.wland_One2021) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

vroom_write(WIP.wland_OneFuture,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/WIP.wland_OneFuture.csv")


#MVERSE W1: Zostera WIPwland Individual Community Loop####
bigZodatalist = list()

for (t in 1:100){ 
  
  Zos_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Zostera_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.spme, TP.spmed, TN.spme, Secc.summe, 
           Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
    nest_by(year, Year.ref) %>% 
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(Zostera_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup()   
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    # s = 3
    #subset data by site
    siteenvdata <- Zos_WIP.wland_OneFuture[Zos_WIP.wland_OneFuture$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}

#BigZosData####
Zo_WIP.wland_PredONEBAY = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_WIP.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Zo_WIP.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigZodatalist)

##MVERSE W2: Ruppia WIPwland Individual Community Loop####

bigRudatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  #everythnig above this is the same for all communities
  Ru_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Ruppia_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% #select the Ruppia WQ Variables
    nest_by(year, Year.ref) %>% #ok this might be it. how do i do math inzide of these fuckin nests
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(Ruppia_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- Ru_WIP.wland_OneFuture[Ru_WIP.wland_OneFuture$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],8] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 8 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 8 in CYP = dens.percomp | col 8 in siteenvdata
      #col 10 in CYP = dens.percomp.change | col 10 in siteenvdata 
      #col 11 in CYP = dens.weight.mean | col 11 in siteenvdata 
      
      siteenvdata[y,8] <- CurrentYrPredict[1,8] #dens.percomp
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp.change
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.weight.mean
      
      #add a case_when so no 0s?
      
    }
    
    Rudatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Rudatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRudatalist[[t]] <- siteenvdataagg
  
}

#BigRuData####

Ru_WIP.wland_PredONEBAY = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_WIP.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Ru_WIP.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigRudatalist)

##MVERSE W3: MM WIPwland Individual Community Loop####

bigMMdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  MM_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% MixMeso_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% #select the MixMeso WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    nest_by(year, Year.ref) %>% #ok this might be it. how do i do math inzide of these fuckin nests
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(MixMeso_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup() 
  
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- MM_WIP.wland_OneFuture[MM_WIP.wland_OneFuture$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 11 in CYP = dens.percomp.change | col 11 in siteenvdata 
      #col 12 in CYP = dens.weight.mean | col 12 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp 
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}

#Big MM data alert#####
MM_WIP.wland_PredONEBAY = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_WIP.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/MM_WIP.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigMMdatalist)

##MVERSE W4: F WIPwland Individual Community Loop####

bigFdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  F_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Fresh_initialDPC$STATION) %>% 
    select(STATION, year, Year.ref,
           Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% #select the Fresh WQ Variables
    nest_by(year, Year.ref) %>% #ok this might be it. how do i do math inzide of these fuckin nests
    group_by(year) %>%
    slice_sample(., n = 1, weight_by = Year.ref, replace = T) %>% #goodgod. it worked. is it only selecting complete cases? also, lets weight by more recent years
    #Yes its only selecting complete years, so if we have year gaps we are fucked
    #i fixed that problem^ i am god
    unnest(cols = c(data, year, Year.ref)) %>% 
    select(-c(Year.ref)) %>%
    full_join(Fresh_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    mutate(Sal.summe = Sal.summe + .001) %>%
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup() 
  
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    
    #subset data by site
    siteenvdata <- F_WIP.wland_OneFuture[F_WIP.wland_OneFuture$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 10 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        dens.percomp > 1 ~ 1.0001,
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      #col 10 in CYP = dens.percomp | col 10 in siteenvdata
      #col 12 in CYP = dens.percomp.change | col 12 in siteenvdata 
      #col 13 in CYP = dens.weight.mean | col 13 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp 
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    Fdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Fdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFdatalist[[t]] <- siteenvdataagg
  
}

#Big F_WIP.wland_Predict alert#####
F_WIP.wland_PredONEBAY = bigFdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

vroom_write(F_WIP.wland_PredONEBAY,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/F_WIP.wland_PredictONEBAY.csv")

#biglist, not needed anymore.
gc(bigFdatalist)



#
#
#
#
##
#
#













##EVEN OLDER, pre may 2022 code OLD!!!!!!!!!#####
#outdated as of May 4 2022


#Need this bigdatalist to be an empty list
bigZodatalist = list()

##MULTIVERSAL LOOP 1: Zos_CC.wland_Predict####

for (t in 1:2){ #eventually this is 1:1000
  
  ####Create CC.wland_OneFuture####
  #
  CC.wland_OneFuture.no2021 = CC.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
           Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
           Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  #Filter out 
  CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(CC.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  Zos_CC.wland_OneFuture = CC.wland_OneFuture %>% #This selects the one set of future data, so this is simnum1
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Zostera_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.spme, TP.spmed, TN.spme, Secc.summe, 
           Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% #select the Zostera WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Zostera_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    ungroup() 
  
  Zodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 3
    #subset data by site
    siteenvdata <- Zos_CC.wland_OneFuture[Zos_CC.wland_OneFuture$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 11 in CYP = dens.percomp | col 11 in siteenvdata
      #col 13 in CYP = dens.percomp.change | col 13 in siteenvdata 
      #col 14 in CYP = dens.weight.mean | col 14 in siteenvdata 
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
      #add a case_when so no 0s?
      
    }
    
    Zodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Zodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZodatalist[[t]] <- siteenvdataagg
  
}

#HUGE Zo_CC.wland_Predict ALERT####

Zo_CC.wland_Predict = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_CC.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Zo_CC.wland_Predict.csv")

#biglist, not needed anymore.
gc(bigZodatalist)


##MULTIVERSAL LOOP 2: Ru_CC.wland_Predict####


bigRudatalist = list()

for (t in 1:2){ #eventually this is 1:1000
  
  ####Create CC.wland_OneFuture####
  
  #Iteration should start here! 
  #Ruppia doesnt have any y1 so dont need to do these muttates
  CC.wland_OneFuture.no2021 = CC.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) #%>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  #group_by(Station) %>%
  #mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
  #       Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
  #       Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    #select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(CC.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  #everythnig above this is the same for all communities
  Ru_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Ruppia_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% #select the Ruppia WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Ruppia_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  Rudatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- Ru_CC.wland_OneFuture[Ru_CC.wland_OneFuture$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
     # y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],8] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 8 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(RuInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      
      
      #col 8 in CYP = dens.percomp | col 8 in siteenvdata
      #col 10 in CYP = dens.percomp.change | col 10 in siteenvdata 
      #col 11 in CYP = dens.weight.mean | col 11 in siteenvdata 
      
      siteenvdata[y,8] <- CurrentYrPredict[1,8] #dens.percomp
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp.change
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.weight.mean
      
      #add a case_when so no 0s?
      
    }
    
    Rudatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Rudatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRudatalist[[t]] <- siteenvdataagg
  
}

#lol ok now what
#HUGE Ru_CC.wland_Predict alert#####

Ru_CC.wland_Predict = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Ru_CC.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Ru_CC.wland_Predict.csv")

#biglist, not needed anymore.
gc(bigRudatalist)


##MULTIVERSAL LOOP 3: MM_CC.wland_Predict####
####DFs needed for loop but not in loop#




df_total = data.frame()

#####
bigMMdatalist = list()

for (t in 1:10){ #eventually this is 1:1000
  
  #Create CC.wland_OneFuture_MIXMESO
  #Iteration should start here! 
  #MixMeso has sumy1max y1 so dont need to do these muttates
  CC.wland_OneFuture.no2021 = CC.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year)) #, 
  #       Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
  #       Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Sal.sumy1max) %>% #, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(CC.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  MM_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% MixMeso_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% #select the MixMeso WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(MixMeso_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  MMdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- MM_CC.wland_OneFuture[MM_CC.wland_OneFuture$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(MMInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      #col 9 in CYP = dens.percomp | col 9 in siteenvdata
      #col 11 in CYP = dens.percomp.change | col 11 in siteenvdata 
      #col 12 in CYP = dens.weight.mean | col 12 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp 
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
    }
    
    MMdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMdatalist[[t]] <- siteenvdataagg
  
}

#Big MM_CC.wland_Predict alert#####
MM_CC.wland_Predict = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(MM_CC.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/MM_CC.wland_Predict.csv")

##MULTIVERSAL LOOP 4: F_CC.wland_Predict####




#####

bigFdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  #Create CC.wland_OneFuture_Fresh####
  #Iteration should start here! 
  #Fresh has sumy1max y1 
  CC.wland_OneFuture.no2021 = CC.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year))
  #     Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year)),  
  #     Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Temp.sumy1me) %>% #, -Temp.sumy1med, -Sal.sumy1max) %>% ) %>%
    full_join(twentyone) 
  
  CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(CC.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  F_CC.wland_OneFuture = CC.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Fresh_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% #select the Fresh WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Fresh_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    mutate(Sal.summe = Sal.summe + .001) %>%
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
 
    #subset data by site
    siteenvdata <- F_CC.wland_OneFuture[F_CC.wland_OneFuture$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
       #y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 10 is dens.percomp
      thisyrinfo <- siteenvdata[siteenvdata$year == future_yrs[y],] #y-1 for dpcy1 above, y for this yr
      
      CurrentYrPredict <- thisyrinfo %>% #kept my tidy code but needed? idk
        mutate(dens.percomp.change = predict(FInt.lmer, newdata = .)) %>%
        mutate(dens.percomp = dens.percomp.change + dens.percomp.y1) %>%
        mutate(dens.weight.mean = dens.percomp*denscomp.max) %>%
        mutate(dens.percomp = case_when(dens.percomp < 0 ~ 0.0001, 
                                        TRUE ~ dens.percomp)) %>%
        mutate(dens.weight.mean = case_when(dens.weight.mean < 0 ~ 0.0001, 
                                            TRUE ~ dens.weight.mean))
      #col 10 in CYP = dens.percomp | col 10 in siteenvdata
      #col 12 in CYP = dens.percomp.change | col 12 in siteenvdata 
      #col 13 in CYP = dens.weight.mean | col 13 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp 
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    Fdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Fdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFdatalist[[t]] <- siteenvdataagg
  
}

#lol ok now wha
#Big F_CC.wland_Predict alert#####
F_CC.wland_Predict = bigFdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

vroom_write(F_CC.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/F_CC.wland_Predict.csv")

gc(bigFdatalist)




###
####
#####
######SCENARIO 2 WIP_wland####
#####
####
###


#Scenario Global DF####
#This is same for all WIP.wlands. Replace for each scenario

#WIP.wland_AllFutures = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wland_AllFutures.csv")

WIP.wlAllFut

#all_equal(WIP.wland_AllFutures, WIP.wlAllFut)

#Community Initial Density DF####
#This is all going to be the same as other Scenarios

#Need this bigdatalist to be an empty list
#I renamed this Rudatalist on the Ruppia sim but was too scared to touch this one
bigZoWIPdatalist = list()

##MULTIVERSAL LOOP 1: Zos_WIP.wland_Predict####

for (t in 1:100){ #eventually this is 1:1000
  
  ####Create WIP.wland_OneFuture####
  #
  WIP.wland_OneFuture.no2021 = WIP.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
           Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
           Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  #Filter out 
  WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  #everythnig above this is the same for all communities
  Zos_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Zostera_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.spme, TP.spmed, TN.spme, Secc.summe, 
           Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% #select the Zostera WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Zostera_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  #i name this Rudatalist in the other loops but dont wanna mess
  ZoWIPdatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now

    #subset data by site
    siteenvdata <- Zos_WIP.wland_OneFuture[Zos_WIP.wland_OneFuture$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:

      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],11] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 11 is dens.percomp
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
      
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.percomp.change
      siteenvdata[y,14] <- CurrentYrPredict[1,14] #dens.weight.mean
      
      #add a case_when so no 0s?
      
    }
    
    ZoWIPdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(ZoWIPdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZoWIPdatalist[[t]] <- siteenvdataagg
  
}

#lol ok now what

Zo_WIP.wland_Predict = bigZoWIPdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")

vroom_write(Zo_WIP.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Zo_WIP.wland_Predict.csv")

##MULTIVERSAL LOOP 2: Ru_WIP.wland_Predict####


bigRuWIPdatalist = list()

for (t in 1:100){ #eventually this is 1:1000

  #Ruppia doesnt have any y1 so dont need to do these muttates
  WIP.wland_OneFuture.no2021 = WIP.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) #%>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  #group_by(Station) %>%
  #mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
  #       Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
  #       Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    #select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  #everythnig above this is the same for all communities
  Ru_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Ruppia_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% #select the Ruppia WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Ruppia_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  RuWIPdatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- Ru_WIP.wland_OneFuture[Ru_WIP.wland_OneFuture$STATION == ru_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #y = 3
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],8] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 8 is dens.percomp
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
      
      
      #col 8 in CYP = dens.percomp | col 8 in siteenvdata
      #col 10 in CYP = dens.percomp.change | col 10 in siteenvdata 
      #col 11 in CYP = dens.weight.mean | col 11 in siteenvdata 
      
      siteenvdata[y,8] <- CurrentYrPredict[1,8] #dens.percomp
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp.change
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.weight.mean
      
      #add a case_when so no 0s?
      
    }
    
    RuWIPdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(RuWIPdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRuWIPdatalist[[t]] <- siteenvdataagg
  
}


Ru_WIP.wland_Predict = bigRuWIPdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

vroom_write(Ru_WIP.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Ru_WIP.wland_Predict.csv")

##MULTIVERSAL LOOP 3: MM_WIP.wland_Predict####
####DFs needed for loop but not in loop#


#works for all
bigMMWIPdatalist = list()

for (t in 1:10){ #eventually this is 1:1000
  
  #Create WIP.wland_OneFuture_MIXMESO####
  #Iteration should start here! 
  #MixMeso has sumy1max y1 so dont need to do these muttates
  WIP.wland_OneFuture.no2021 = WIP.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year)) #, 
  #       Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
  #       Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Sal.sumy1max) %>% #, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  MM_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% MixMeso_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% #select the MixMeso WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(MixMeso_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  MMWIPdatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- MM_WIP.wland_OneFuture[MM_WIP.wland_OneFuture$STATION == MM_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],9] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
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
      #col 11 in CYP = dens.percomp.change | col 11 in siteenvdata 
      #col 12 in CYP = dens.weight.mean | col 12 in siteenvdata 
      
      siteenvdata[y,9] <- CurrentYrPredict[1,9] #dens.percomp 
      siteenvdata[y,11] <- CurrentYrPredict[1,11] #dens.percomp.change
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.weight.mean
      
    }
    
    MMWIPdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMWIPdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMWIPdatalist[[t]] <- siteenvdataagg
  
}

###BIG DATA ALERT####

MM_WIP.wland_Predict = bigMMWIPdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

vroom_write(MM_WIP.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/MM_WIP.wland_Predict.csv")


##MULTIVERSAL LOOP 4: F_WIP.wland_Predict####

df_total = data.frame()


bigFdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  #Create WIP.wland_OneFuture_Fresh####
  #Iteration should start here! 
  #Fresh has sumy1max y1 so dont need to do these muttates
  WIP.wland_OneFuture.no2021 = WIP.wlAllFut %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year))
  #     Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year)),  
  #     Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Temp.sumy1me) %>% #, -Temp.sumy1med, -Sal.sumy1max) %>% ) %>%
    full_join(twentyone) 
  
  WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.wland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  F_WIP.wland_OneFuture = WIP.wland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Fresh_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% #select the Fresh WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Fresh_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    mutate(Sal.summe = Sal.summe + .001) %>%
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  Fdatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- F_WIP.wland_OneFuture[F_WIP.wland_OneFuture$STATION == F_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #  y = 2
      dens.percomp.y1 <- siteenvdata[siteenvdata$year == future_yrs[y-1],10] #this var needed for predict(), should be dens.percomp (i.e. y1) #col 9 is dens.percomp
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
      #col 11 in CYP = dens.percomp.change | col 11 in siteenvdata 
      #col 12 in CYP = dens.weight.mean | col 12 in siteenvdata 
      
      siteenvdata[y,10] <- CurrentYrPredict[1,10] #dens.percomp 
      siteenvdata[y,12] <- CurrentYrPredict[1,12] #dens.percomp.change
      siteenvdata[y,13] <- CurrentYrPredict[1,13] #dens.weight.mean
      
    }
    
    Fdatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(Fdatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFdatalist[[t]] <- siteenvdataagg
  
}

#lol ok now what


F_WIP.wland_Predict = bigFdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

#select a random 420 samples 
vroom_write(F_WIP.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/F_WIP.wland_Predict.csv")

gc(bigFdatalist)



###
####



