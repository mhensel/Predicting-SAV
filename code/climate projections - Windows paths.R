#Code for Climate Projections of Chesapeake Bay SAV communities#
library(tidyverse); library(janitor); library(lme4); library(car)


#most of the dataframes here were built in /code/assemble climate data.R
#this code is copy-pasted from the successful code/CC_wl projections clean.R, so shouldnt be different from that.
#Models used originally come from the SEMs in code/community change model building.R Will need to make sure the past data from those models are available too in the code below.


#############DING DING DING, THE MULTIVERSE IS REAL 2/5 #### 
#1/26 Lets try all of this again
#2/6 update: This works, as far as I can tell!! Might be able to speed up the 1000x simulations with map(), by doing bind_rows() instread of joins. I'll go thru and quad hashtag the places to check/change when switching Communities. I'll try not to move things, because moving things for for() loops makes me nervous
#Go to "START HERE" on line 119
#2/7 update: trimming this down so we can just run this whole code file and itll do its thang

##START HERE#####
####DFs needed for ALL loops but not in loop####
#Load in the SAV change per year data, merged with CBP WQ data, with the 69 variables of interest selected:

#SAVCommDensWQ_69 = read.csv("~/Documents/R projects/Predicting-SAV/data/SAVCommDensWQ_69.csv")
SAVCommDensWQ_69 = read.csv("r:/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69.csv")

#We deal with 0s by eliminating any years where the 3 previous years were 0s, with this antijoin code
SAVCommZeros = SAVCommDensWQ_69 %>% mutate(dens.weight.mean.y2 = lag(dens.weight.mean.y1))  %>%
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0 & dens.weight.mean.y2 == 0) 
#anti join the 0s to get a No0 df

SAVCommDensWQ_69sem.No0 = anti_join(SAVCommDensWQ_69, SAVCommZeros) %>% #anti the 0s
  select(STATION, year, SpCluster, dens.weight.mean, dens.weight.mean.y1, dens.percomp.y1, dens.percomp, dens.percomp.change, denscomp.max, Temp.sumy1med, Temp.sumy1me, Sal.summax, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summed, Temp.summax, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Sal.summed, Secc.summe, Secc.spme, TP.spmed, TP.spme, TSS.summe, TP.summe, TP.summax, TN.spme, TN.spmed, TN.summe) #you'll want to select down more and then drop NA at each community, but this is technically all we need for now. IDK whats best but nice to have this here. 

#write.csv(SAVCommDensWQ_69sem.No0, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69sem.No0.csv")

#can probably use the SAVCommDensWQ above.... 
#CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
CBP.WQ_69vars = read.csv("r:/Current Projects/Predicting-SAV/data/CBP.WQ_69vars.csv")

##Universal DF: twentyone####
#twentyone##
twentyone = CBP.WQ_69vars %>%  #twentyone should work for all Communities
  filter(year == "2020") %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  mutate(Year = case_when(year == 2020 ~ 2021)) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION) %>%
  select(Station, Year, everything()) %>% select(-year) %>% 
  replace_na(list(Temp.sumy1me = 31.2550, Temp.sumy1med = 31.2550, Sal.sumy1max = 0.001)) %>% 
  ungroup() 

#future years, used in loop
future_yrs = seq(2020, 2060, by = 1) #should this be 2021

#
##
###
####
######SCENARIO 1 CC_wland####
####
###
##
#

#Scenario Global DF####
#This is same for all CC.wlands. Replace for each scenario
CC.wland_AllFutures = read.csv("r:/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wland_AllFutures.csv")


#Community Initial Density DF####
#check the ENV vars and the replace_na for each community here!
Zostera_initialDPC = SAVCommDensWQ_69sem.No0 %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Zostera") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.spme, TP.spmed, TN.spme, Secc.summe, #customize these per community
         Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>% #no 0s please
  replace_na(list(TP.spmed = 0.03, TN.spme = 0.4))#not ideal but need this to not be NA. It was Station LE4.3
#vector of zos_stations for the loop
zos_station = Zostera_initialDPC$STATION

#DF and model for Zostera
ZoDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Zostera") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

ZoInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)) + (1|STATION), data = ZoDensWQsem.No0_NEW)


df_total = data.frame() #idk if we need this actually
#Need this bigdatalist to be an empty list


bigZodatalist = list()

##MULTIVERSAL LOOP 1: Zos_CC.wland_Predict####

for (t in 1:100){ #eventually this is 1:1000
  
  ####Create CC.wland_OneFuture####
  #
  CC.wland_OneFuture.no2021 = CC.wland_AllFutures %>% 
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
  
  #everythnig above this is the same for all communities
  Zos_CC.wland_OneFuture = CC.wland_OneFuture %>% 
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

#lol ok now what

Zo_CC.wland_Predict = bigZodatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")
#HUGE DATA ALERT####
#biglist, not needed anymore.

write.csv(Zo_CC.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/Zo_CC.wland_Predict.csv")

gc(bigZodatalist)

##MULTIVERSAL LOOP 2: Ru_CC.wland_Predict####
####DFs needed for loop but not in loop#

Ruppia_initialDPC = SAVCommDensWQ_69sem.No0 %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Ruppia") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.spme, TP.spme, TN.spme, Sal.spme, Temp.spme) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>% #no 0s please
  replace_na(list(TP.spmed = 0.03, TN.spme = 0.4))#not ideal but need this to not be NA. It was Station LE4.3

ru_station = Ruppia_initialDPC$STATION

#load in Ru model and dataa
RuDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Ruppia") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Temp.spme, Chla.spme, Sal.spme, Secc.spme, TP.spme, TN.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

RuInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(Chla.spme) + 
                     log10(TP.spme) + log10(TN.spme) + log10(Temp.spme) +
                     (dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(Chla.spme)) + 
                     (log10(TP.spme):dens.percomp.y1) + (log10(TN.spme):dens.percomp.y1) + 
                     (log10(Temp.spme):dens.percomp.y1) + (1|STATION), 
                   data = RuDensWQsem.No0_NEW)

df_total = data.frame()

#####

bigRudatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  ####Create CC.wland_OneFuture####
  
  #Iteration should start here! 
  #Ruppia doesnt have any y1 so dont need to do these muttates
  CC.wland_OneFuture.no2021 = CC.wland_AllFutures %>% 
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

Ru_CC.wland_Predict = bigRudatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")
#HUGE DATA ALERT####
#biglist, not needed anymore.

write.csv(Ru_CC.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/Zo_CC.wland_Predict.csv")

gc(bigRudatalist)


#59393 / 1804000 #97% of the ruppia change is between



##MULTIVERSAL LOOP 3: MM_CC.wland_Predict####
####DFs needed for loop but not in loop#

MixMeso_initialDPC = SAVCommDensWQ_69sem.No0 %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "MixedMeso") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>% #no 0s please
  replace_na(list(Chla.summe = 51.085, Temp.summe = 26.675, Temp.summin = 19.9, TP.summe = 0.1477, TN.summe = 1.375, Sal.sumy1max = 8.095))#not ideal but need this to not be NA. It was Station XHH4742

#View(MixMeso_initialDPC %>% filter(STATION == "XHH4742") %>% 
#  summarize(across(everything(), ~median(., na.rm = T))))

MM_station = MixMeso_initialDPC$STATION

MMDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "MixedMeso") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  drop_na() %>% #161
  as.data.frame()

MMInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(TN.summe) + log10(Chla.summe) + log10(TP.summe) +log10(Temp.summin) +log10(Sal.sumy1max):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1 +log10(TN.summe):dens.percomp.y1+ log10(Temp.summin):dens.percomp.y1 + (1|STATION), data = MMDensWQsem.No0_NEW)


df_total = data.frame()


bigMMdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  #Create CC.wland_OneFuture_MIXMESO####
  #Iteration should start here! 
  #MixMeso has sumy1max y1 so dont need to do these muttates
  CC.wland_OneFuture.no2021 = CC.wland_AllFutures %>% 
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

#lol ok now what
library(wesanderson)


MM_CC.wland_Predict = bigMMdatalist %>% 
  map_dfr(as_tibble, .name_repair = "universal")
#HUGE DATA ALERT####
#biglist, not needed anymore.

write.csv(MM_CC.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/MM_CC.wland_Predict.csv")

gc(bigMMdatalist)
#Big Data alert#####
#list not needed

##MULTIVERSAL LOOP 4: F_CC.wland_Predict####

Fresh_initialDPC = SAVCommDensWQ_69sem.No0 %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Fresh") %>%
  filter(year == "2020") %>% 
  filter(!STATION == "PIS0033") %>% #apparently this station aint in the Projecteddata
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>% #no 0s please
  replace_na(list(Temp.sumy1me = 25.8, TP.summe = 0.058, TN.summe = 0.86, Sal.summe = 0.001))#not ideal but need this to not be NA. It was Station TFs. replaced w medians

#View(Fresh_initialDPC %>% filter(str_detect(STATION, "TF")) %>% 
#  summarize(across(everything(), ~median(., na.rm = T))))

F_station = Fresh_initialDPC$STATION

FreshDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Fresh") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TP.summax, TN.summe, Sal.summe, TSS.summe) %>%
  drop_na() %>% #161
  mutate(Sal.summe = Sal.summe + .1) %>%
  as.data.frame()

FInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 +log10(Sal.summe) + log10(Chla.summe) + log10(TP.summe)  + log10(Temp.sumy1me) +log10(Temp.summe)  + log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1  +log10(Temp.sumy1me):dens.percomp.y1 + (1|STATION), data = FreshDensWQsem.No0_NEW)


bigFdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
  #Create CC.wland_OneFuture_Fresh####
  #Iteration should start here! 
  #Fresh has sumy1max y1 
  CC.wland_OneFuture.no2021 = CC.wland_AllFutures %>% 
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

F_CC.wland_Predict = bigFdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")
#Big Data alert#####
write.csv(F_CC.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/F_CC.wland_Predict.csv")

gc(F_CC.wland_Predict)

#select a random 420 samples 

#list not needed



###
####
#####
######SCENARIO 2 WIP_wland####
#####
####
###


#Scenario Global DF####
#This is same for all WIP.wlands. Replace for each scenario

WIP.wland_AllFutures = read.csv("r:/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wland_AllFutures.csv")

#Community Initial Density DF####
#This is all going to be the same as other Scenarios

#Need this bigdatalist to be an empty list
#I renamed this Rudatalist on the Ruppia sim but was too scared to touch this one
bigZoWIPdatalist = list()

##MULTIVERSAL LOOP 1: Zos_WIP.wland_Predict####

for (t in 1:1000){ #eventually this is 1:1000
  
  ####Create WIP.wland_OneFuture####
  #
  WIP.wland_OneFuture.no2021 = WIP.wland_AllFutures %>% 
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
      # y = 2
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

#huge data alert####
Zo_WIP.wland_Predict = bigZoWIPdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

write.csv(Zo_WIP.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/Zo_WIP.wland_Predict.csv")

gc(Zo_WIP.wland_Predict)


##MULTIVERSAL LOOP 2: Ru_WIP.wland_Predict####

bigRuWIPdatalist = list()

for (t in 1:1000){ #eventually this is 1:1000
  
  ####Create WIP.wland_OneFuture####
  #Iteration should start here! 
  #Ruppia doesnt have any y1 so dont need to do these muttates
  WIP.wland_OneFuture.no2021 = WIP.wland_AllFutures %>% 
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

#huge data alert####
Ru_WIP.wland_Predict = bigRuWIPdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

write.csv(Ru_WIP.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/Ru_WIP.wland_Predict.csv")

gc(Ru_WIP.wland_Predict)

##MULTIVERSAL LOOP 3: MM_WIP.wland_Predict####
####DFs needed for loop but not in loop#

bigMMWIPdatalist = list()

for (t in 1:1000){ #eventually this is 1:1000
  
  #Create WIP.wland_OneFuture_MIXMESO####
  #Iteration should start here! 
  #MixMeso has sumy1max y1 so dont need to do these muttates
  WIP.wland_OneFuture.no2021 = WIP.wland_AllFutures %>% 
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

#lol ok now what
MM_WIP.wland_Predict = bigMMWIPdatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

#HUGE DATA ALERT####
#biglist, not needed anymore.

write.csv(MM_WIP.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/MM_WIP.wland_Predict.csv")


##MULTIVERSAL LOOP 4: F_WIP.wland_Predict####

Fresh_initialDPC = SAVCommDensWQ_69sem.No0 %>% #Do this separate for each community
  ungroup() %>%
  filter(SpCluster == "Fresh") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max, 
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% 
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) #%>% #no 0s please
#replace_na(list(Chla.summe = 51.085, Temp.summe = 26.675, Temp.summin = 19.9, TP.summe = 0.1477, TN.summe = 1.375, Sal.sumy1max = 8.095))#not ideal but need this to not be NA. It was Station XHH4742

F_station = Fresh_initialDPC$STATION

FInt.lmer <- lmer(dens.percomp.change ~dens.percomp.y1 +log10(Sal.summe) + log10(Chla.summe) + log10(TP.summe)  + log10(Temp.sumy1me) +log10(Temp.summe)  +log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1  +log10(Temp.sumy1me):dens.percomp.y1 + 1|STATION, data = FreshDensWQsem.No0_NEW)


bigFdatalist = list()

for (t in 1:1000){ #eventually this is 1:1000
  
  #Create WIP.wland_OneFuture_Fresh####
  #Iteration should start here! 
  #Fresh has sumy1max y1 so dont need to do these muttates
  WIP.wland_OneFuture.no2021 = WIP.wland_AllFutures %>% 
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

#HUGE DATA ALERT####
#biglist, not needed anymore.
gc(bigFdatalist)

write.csv(F_WIP.wland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/F_WIP.wland_Predict.csv")


###
####

###
####
#####
######SCENARIO 3 WIP_woland####
#####
####
###

######this is the one scenario that does need a little tweaking
####DFs needed for loop but not in loop####
##Universal DF: twentyone####

#Scenario Global DF####
#Add 2021-2030 wland to woland to complete this dataset
WIP.woland_AllFutures = read.csv("r:/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.woland_AllFutures.csv")

WIP.wland_AllFutures = read.csv("r:/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wland_AllFutures.csv")

WIP.woland_AllFutures2020start = WIP.wland_AllFutures %>% filter(between(Year, 2021, 2030)) %>%
  bind_rows(WIP.woland_AllFutures) %>%
  arrange(Station, Year)

#Community Initial Density DF####
#check the ENV vars and the replace_na for each community here!

#Need this bigdatalist to be an empty list
#I renamed this Rudatalist on the Ruppia sim but was too scared to touch this one
bigZoWIPwodatalist = list()

##MULTIVERSAL LOOP 1: Zos_WIP.woland_Predict####

for (t in 1:1000){ #eventually this is 1:1000
  
  ####Create WIP.woland_OneFuture####
  #
  
  WIP.woland_OneFuture.no2021 = WIP.woland_AllFutures2020start %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
           Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
           Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  #Filter out 
  WIP.woland_One2021 = WIP.woland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  WIP.woland_OneFuture = WIP.woland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.woland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  #everythnig above this is the same for all communities
  Zos_WIP.woland_OneFuture = WIP.woland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Zostera_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.spme, TP.spmed, TN.spme, Secc.summe, 
           Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% #select the Zostera WQ Variables
    #drop_na() %>% #DUDE fuck these NAs
    full_join(Zostera_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    # mutate(year = case_when(year == 2020 ~ 2030, 
    #                         TRUE ~ year)) %>%
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  #i name this Rudatalist in the other loops but dont wanna mess
  ZoWIPwodatalist = list()
  
  for(s in 1:length(zos_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 3
    #subset data by site
    siteenvdata <- Zos_WIP.woland_OneFuture[Zos_WIP.woland_OneFuture$STATION == zos_station[s],] 
    # intdendata <- ZDT2019[ZDT2019$STATION == zos_station[s],]
    siteenvdata$dens.percomp.change = NA  
    siteenvdata$dens.weight.mean = NA
    siteenvdata <- as.data.frame(siteenvdata)
    
    
    for(y in 2:length(future_yrs)) { #HERE is basically why i needed the for loop, to start on 2:
      #y = 2
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
    
    ZoWIPwodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(ZoWIPwodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigZoWIPwodatalist[[t]] <- siteenvdataagg
  
}

#lol ok now what

Zo_WIP.woland_Predict = bigZoWIPwodatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")


write.csv(Zo_WIP.woland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/Zo_WIP.woland_Predict.csv")

gc(bigZoWIPwodatalist)

##MULTIVERSAL LOOP 2: Ru_WIP.woland_Predict####
####DFs needed for loop but not in loop#


bigRuWIPwodatalist = list()

for (t in 1:1000){ #eventually this is 1:1000
  
  ####Create WIP.woland_OneFuture####
  
  #Iteration should start here! 
  #Ruppia doesnt have any y1 so dont need to do these muttates
  WIP.woland_OneFuture.no2021 = WIP.woland_AllFutures2020start %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) #%>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  #group_by(Station) %>%
  #mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
  #       Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
  #       Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  WIP.woland_One2021 = WIP.woland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    #select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
    full_join(twentyone) 
  
  WIP.woland_OneFuture = WIP.woland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.woland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  #everythnig above this is the same for all communities
  Ru_WIP.woland_OneFuture = WIP.woland_OneFuture %>% 
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
  
  RuWIPwodatalist = list()
  
  for(s in 1:length(ru_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- Ru_WIP.woland_OneFuture[Ru_WIP.woland_OneFuture$STATION == ru_station[s],] 
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
    
    RuWIPwodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(RuWIPwodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigRuWIPwodatalist[[t]] <- siteenvdataagg
  
}

#bigDATA ####
Ru_WIP.woland_Predict = bigRuWIPwodatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

write.csv(Ru_WIP.woland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/Ru_WIP.woland_Predict.csv")

gc(bigRuWIPwodatalist)
##MULTIVERSAL LOOP 3: MM_WIP.woland_Predict####
####DFs needed for loop but not in loop#


bigMMWIPwodatalist = list()

for (t in 1:1000){ #eventually this is 1:1000
  
  #Create WIP.woland_OneFuture_MIXMESO####
  #Iteration should start here! 
  #MixMeso has sumy1max y1 so dont need to do these muttates
  WIP.woland_OneFuture.no2021 = WIP.woland_AllFutures2020start %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year)) #, 
  #       Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
  #       Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  #Filter out 
  WIP.woland_One2021 = WIP.woland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Sal.sumy1max) %>%
    full_join(twentyone) 
  
  WIP.woland_OneFuture = WIP.woland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.woland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  MM_WIP.woland_OneFuture = WIP.woland_OneFuture %>% 
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
  
  MMWIPwodatalist = list()
  
  for(s in 1:length(MM_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- MM_WIP.woland_OneFuture[MM_WIP.woland_OneFuture$STATION == MM_station[s],] 
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
    
    MMWIPwodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(MMWIPwodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigMMWIPwodatalist[[t]] <- siteenvdataagg
  
}

#lol ok now what

#bid DATA ALERT####
MM_WIP.woland_Predict = bigMMWIPwodatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

write.csv(MM_WIP.woland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/MM_WIP.woland_Predict.csv")


##MULTIVERSAL LOOP 4: F_WIP.woland_Predict####
####DFs needed for loop but not in loop#

bigFWIPwodatalist = list()

for (t in 1:1000){ #eventually this is 1:1000
  
  #Create WIP.woland_OneFuture_Fresh####
  #Iteration should start here! 
  #Fresh has sumy1max y1 so dont need to do these muttates
  WIP.woland_OneFuture.no2021 = WIP.woland_AllFutures2020start %>% 
    group_by(Station, Year) %>% 
    slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
    group_by(Station) %>%
    mutate(Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year))
  #     Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year)),  
  #     Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
  
  WIP.woland_One2021 = WIP.woland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
    select(-Temp.sumy1me) %>% #, -Temp.sumy1med, -Sal.sumy1max) %>% ) %>%
    full_join(twentyone) 
  
  WIP.woland_OneFuture = WIP.woland_OneFuture.no2021 %>% 
    filter(!Year == "2021") %>%
    bind_rows(WIP.woland_One2021) %>% 
    ungroup() %>%
    arrange(Station, Year)
  
  F_WIP.woland_OneFuture = WIP.woland_OneFuture %>% 
    rename("STATION" = "Station", "year" = "Year") %>%
    filter(STATION %in% Fresh_initialDPC$STATION) %>% 
    select(STATION, year, 
           Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TN.summe, Sal.summe) %>% #select the Fresh WQ Variables
    full_join(Fresh_initialDPC) %>% #join in the initial dens. bind rows was what i had before so check the loop columns bc for loops are stupid
    mutate(Sal.summe = Sal.summe + .001) %>%
    group_by(STATION, year) %>%
    arrange(STATION, year) %>% 
    group_by(STATION) %>%
    fill(denscomp.max) %>%
    #mutate(dens.percomp.y1 = lag(dens.percomp, n = 1)) %>% #took this out bc we make our
    ungroup() 
  
  # FIX NA's IN THE PREDICTED CLIMATE MATRIX BEFORE GOING TO NEXT LOOP
  
  FWIPwodatalist = list()
  
  for(s in 1:length(F_station)) { #length(zos_station)  , but some stations have NA problems so just 11 for now
    #s = 4
    #subset data by site
    siteenvdata <- F_WIP.woland_OneFuture[F_WIP.woland_OneFuture$STATION == F_station[s],] 
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
    
    FWIPwodatalist[[s]] <- siteenvdata
    
  }
  
  # rbind together all objects in datalist
  siteenvdataagg <- bind_rows(FWIPwodatalist)
  # add column for simnum and populate with t (outer loop iteration)
  siteenvdataagg$simnum <- rep(t, length(siteenvdataagg[,1]))
  # write big dataframe to big data list 
  bigFWIPwodatalist[[t]] <- siteenvdataagg
  
}

#lol ok now what


F_WIP.woland_Predict = bigFWIPwodatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")


write.csv(F_WIP.woland_Predict,"r:/Current Projects/Predicting-SAV/data/Multiversal Futures/F_WIP.woland_Predict.csv")

gc(bigFWIPwodatalist)



