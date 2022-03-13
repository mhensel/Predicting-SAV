#Code for Climate Projections of Chesapeake Bay SAV communities#
library(tidyverse); library(janitor); library(lme4); library(car)

#Notes: 
#most of the dataframes here were built in /code/assemble climate data.R
#this code is copy-pasted from the successful code/CC_wl projections clean.R, so shouldnt be different from that.
#Models used originally come from the SEMs in code/community change model building.R Will need to make sure the past data from those models are available too in the code below.


#############DING DING DING, THE MULTIVERSE IS REAL 2/5 #### 
#1/26 Lets try all of this again
#2/6 update: This works, as far as I can tell!! Might be able to speed up the 1000x simulations with map(), by doing bind_rows() instread of joins. I'll go thru and quad hashtag the places to check/change when switching Communities. I'll try not to move things, because moving things for for() loops makes me nervous
#Go to "START HERE" on line 119


#Test code, can ignore these next few####
###run these next 4 DFs to get the CC.wland_OneFuture
##twentyone code: Really only need this once, can collapse
####2021 y1 data#
twentyone = CBP.WQ_69vars %>% filter(year == "2020") %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  mutate(Year = case_when(year == 2020 ~ 2021)) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>% #make 2020 vars into 2021 y1 vars
  rename(Station = STATION) %>% #this gets renamed again but just go w it
  select(Station, Year, everything()) %>% select(-year) %>% 
  replace_na(list(Temp.sumy1me = 31.2550, Temp.sumy1med = 31.2550, Sal.sumy1max = 0.001)) %>% #there are a couple NAs for unknown reasons, replaced w median values from that station
  ungroup() 

####Create CC.wland_OneFuture####
#Re-do this for each scenario, only need once as well and could probably tidy up these joins and pipes... 
CC.wland_AllFutures = left_join(CC.wland_summerPP, CC.wland_springPP) %>% 
  select(-year.y) %>% 
  rename("Year" = "year.x") 

#Iteration begins here####
CC.wland_OneFuture.no2021 = CC.wland_AllFutures %>% 
  group_by(Station, Year) %>% 
  slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))

CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% 
  filter(!Year == "2021") %>%
  bind_rows(CC.wland_One2021) %>% 
  ungroup() %>%
  arrange(Station, Year)

#SAVCommunityDens_AllStations = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommunityDens_AllStations.csv")
#SAVCommunityDens_AllStations read.csv("~/Documents/R projects/Predicting-SAV/data/SAVCommunityDens_AllStations.csv")

#Do we want No0 DF?
#SAVAllCommZeros = SAVCommunityDens_AllStations %>% mutate(dens.weight.mean.y2 = lag(dens.weight.mean.y1))  %>%
#  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0 & dens.weight.mean.y2 == 0) 
#anti join the 0s to get a No0 df
#SAVAllCommDens.No0 = anti_join(SAVCommunityDens_AllStations, SAVAllCommZeros)

#Do i want the WQs actually 
#SAVCommDensWQ_69sem.No0

#Community LMERs####
#Note that I switched back to lmer instead of lme because i was getting errors that aseemed related to the corARMA but idk.
#ZoInt.lmer <- lme(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)), random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = ZoDensWQsem.No0_NEW)
#UMMMMMM so..... doing the corr and control cause errors below

ZoInt.lmer <- lmer(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)) + (1|STATION), data = ZoDensWQsem.No0_NEW)

ZoInt.lm <- lm(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)), data = ZoDensWQsem.No0_NEW)

#####Initial 2020 Density per community#### 
#Also can select ENV of interest for na control
Zostera_initialDPC = SAVCommDensWQ_69sem.No0 %>%
  ungroup() %>%
  filter(SpCluster == "Zostera") %>%
  filter(year == "2020") %>% 
  select(STATION, year, dens.percomp, denscomp.max,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% #!!!! SELECT WHAT YOU NEED FOR LMER
  mutate(dens.percomp = case_when(dens.percomp <= 0 ~ 0.001, 
                                  dens.percomp > 0 ~ dens.percomp)) %>% #no 0s please
  replace_na(list(TP.spmed = 0.03, TN.spme = 0.4))#not ideal but need this to not be NA. It was Station LE4.3. hopefully not a big problem in the other zones. Check this each time.

#Use this in the for loop?
Zo_2020 = Zostera_initialDPC %>% select(STATION, year, dens.percomp)

###Predict one future here####
Zos_CC.wland_OneFuture = CC.wland_OneFuture %>% drop_na() %>%
  rename("STATION" = "Station", "year" = "Year") %>%
  filter(STATION %in% Zostera_initialDPC$STATION) %>% 
  select(year, STATION, 
         Chla.spme, TP.spmed, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>% #select the Zostera WQ Variables again
  bind_rows(Zostera_initialDPC) %>% select(-denscomp.max) %>% 
  # filter(STATION %in% c("EE3.5", "WE4.1", "LE5.4", "WE4.2")) %>% #check a couple stations
  arrange(STATION, year) %>% 
  ungroup() %>%
  group_by(STATION) %>% 
  mutate(dens.percomp.change = predict(ZoInt.lmer, newdata = data.frame(
    dens.percomp.y1 = lag(dens.percomp, n = 1, order_by = year), 
    Temp.sumy1med = Temp.sumy1med, Sal.summed = Sal.summed, Chla.spme = Chla.spme, Secc.summe = Secc.summe, Temp.spmed = Temp.spmed, STATION = STATION))) %>% #why cant i have it just start on the second row and overwrite dpcc
  mutate(dens.percomp = dens.percomp.change + lag(dens.percomp, n = 1))

##This just does it once, and I failed to get the accumulate2() functioning to work. Go over to code/projection tests.R to see a reprex that gets close. will revisit soon but need to move on for now on 2/6 before this "deadline"




###
####





##START HERE#####
####DFs needed for ALL loops but not in loop####
#Load in the SAV change per year data, merged with CBP WQ data, with the 69 variables of interest selected:

#SAVCommDensWQ_69 = read.csv("~/Documents/R projects/Predicting-SAV/data/SAVCommDensWQ_69.csv")
SAVCommDensWQ_69 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69.csv")

file <- "https://raw.githubusercontent.com/r-lib/vroom/main/inst/extdata/mtcars.csv"
vroom(file)

#We deal with 0s by eliminating any years where the 3 previous years were 0s, with this antijoin code
SAVCommZeros = SAVCommDensWQ_69 %>% mutate(dens.weight.mean.y2 = lag(dens.weight.mean.y1))  %>%
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0 & dens.weight.mean.y2 == 0) 
#anti join the 0s to get a No0 df

SAVCommDensWQ_69sem.No0 = anti_join(SAVCommDensWQ_69, SAVCommZeros) %>% #anti the 0s
  select(STATION, year, SpCluster, dens.weight.mean, dens.weight.mean.y1, dens.percomp.y1, dens.percomp, dens.percomp.change, denscomp.max, Temp.sumy1med, Temp.sumy1me, Sal.summax, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summed, Temp.summax, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Sal.summed, Secc.summe, Secc.spme, TP.spmed, TP.spme, TSS.summe, TP.summe, TP.summax, TN.spme, TN.spmed, TN.summe) #you'll want to select down more and then drop NA at each community, but this is technically all we need for now. IDK whats best but nice to have this here. 

#write.csv(SAVCommDensWQ_69sem.No0, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69sem.No0.csv")

#can probably use the SAVCommDensWQ above.... 
CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
CBP.WQ_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_69vars.csv")

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
#CC.wland_AllFutures = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wland_AllFutures.csv")
CC.wlAllFut

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



bigRudatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
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
  map_dfr(as.tibble, .name_repair = "universal")

vroom_write(Ru_CC.wland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/RuCC.wland_Predict.csv")

#biglist, not needed anymore.
gc(bigRudatalist)


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

#####
bigMMdatalist = list()

for (t in 1:100){ #eventually this is 1:1000
  
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

for (t in 1:100){ #eventually this is 1:1000
  
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



#####CAN STOP HERE. DELETING WIP_woland bc land use change does nothing!####
######SCENARIO 3 WIP_woland####
#####
####
###

######this is the one scenario that does need a little tweaking
####DFs needed for loop but not in loop####
##Universal DF: twentyone####

#Scenario Global DF####
#Add 2021-2030 wland to woland to complete this dataset
WIP.woland_AllFutures = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.woland_AllFutures.csv")

WIP.wland_AllFutures = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wland_AllFutures.csv")

WIP.woland_AllFutures2020start = WIP.wland_AllFutures %>% filter(between(Year, 2021, 2030)) %>%
  bind_rows(WIP.woland_AllFutures) %>%
  arrange(Station, Year)

#Community Initial Density DF####
#check the ENV vars and the replace_na for each community here!

#Need this bigdatalist to be an empty list
#I renamed this Rudatalist on the Ruppia sim but was too scared to touch this one
bigZoWIPwodatalist = list()

##MULTIVERSAL LOOP 1: Zos_WIP.woland_Predict####

for (t in 1:2){ #eventually this is 1:1000
  
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


##MULTIVERSAL LOOP 2: Ru_WIP.woland_Predict####
####DFs needed for loop but not in loop#

#####

bigRuWIPwodatalist = list()

for (t in 1:2){ #eventually this is 1:1000
  
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


Ru_WIP.woland_Predict = bigRuWIPwodatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")

#59393 / 1804000 #97% of the ruppia change is between


##MULTIVERSAL LOOP 3: MM_WIP.woland_Predict####
####DFs needed for loop but not in loop#


bigMMWIPwodatalist = list()

for (t in 1:2){ #eventually this is 1:1000
  
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


MM_WIP.woland_Predict = bigMMWIPwodatalist %>% 
  map_dfr(as.tibble, .name_repair = "universal")


##MULTIVERSAL LOOP 4: F_WIP.woland_Predict####
####DFs needed for loop but not in loop#

bigFWIPwodatalist = list()

for (t in 1:2){ #eventually this is 1:1000
  
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

write.csv(F_WIP.woland_Predict,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/F_WIP.woland_Predict.csv")

gc(bigFWIPwodatalist)

















#
#
#
#
##OLD CODE, before date match reconfiguration####

##2/6 quick look: this whole file might be trash now. go to CC_wl projections clean to see if you can just start there. 

####Build Past Data here
#Start with our real life data: 
#then load in the CBP data to 2020
CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")

#also, baseline
baseline_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/baseline_69vars.csv") #baseline is 1991-2000
#NOTE: baseline has these "depth" columns. 12 of them i guess? 1 for each season

#First, fix the repeating NA problem by substituting the real life 1990 data (ie 1991 y1 data) with this complicated ass bind select filter replace bullshit:
#here is the 1990 data, technically as the 1991 y1 data. bind ninetyone_Base to baseline in a bit, so we have real life 1991 y1 data. get it?
ninetyone = CBP.WQ_69vars %>% filter(year == "1991") %>% 
  select(STATION, year, contains("y1")) %>% #its 1991 y1 data
  select(-contains("TSS")) %>% 
  rename(Station = STATION) %>% rename(Year = year)

ninetyone_Base = baseline_69vars %>% 
  filter(Year == "1991") %>%
  select(Station, Year, everything()) %>%
  select(-contains("y1")) %>% #technically this will replace all the y1 variables but who cares
  full_join(ninetyone, by = c("Station", "Year")) 

#There is also another problem with NAs in certain stations in baseline that need to be replaced
#select random complete years to fill the incomplete stations: LE5.5-W	, TF3.1E, TF3.2A, WXT0001, XHH4742

#ninetyonebadstations = CBP.WQ_69vars %>% 
  filter(STATION %in% c("LE5.5-W","TF3.1E","TF3.2A","WXT0001","XHH4742")) %>% #XHH4742 dont have data till later
  select(STATION, year, contains("y1")) %>% 
  select(-contains("TSS")) %>% 
  group_by(STATION) %>% 
  slice_sample(n = 10 , replace = T) %>% #pick a random 10 years
  rename(Station = STATION) %>% rename(Year = year) %>%
  mutate(Year = seq(1991, 2000, by = 1)) #just a white lie about what year it is

#make sure you sliced and didnt get any NAs. If you see NAs, redo ninetyonebadstations:
ninetyonebadstations %>% group_by(Year) %>% 
       select(Year, Station, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max) %>%
       summarise(across(everything(), ~ sum(is.na(.))))

#can just run this code to fill in the NAs
#ninetyonebadstations[] <- ninetyonebadstations %>% group_by(Station) %>%
 # lapply(.,function(x) 
 #   replace(x,is.na(x), sample(x[!is.na(x)],sum(is.na(x)))))

ninetyonebadstations_Base = baseline_69vars %>% 
  filter(Station %in% c("LE5.5-W","TF3.1E","TF3.2A","WXT0001","XHH4742")) %>%
  select(Station, Year, everything()) %>%
  select(-contains("y1")) %>% #technically this will replace all the y1 variables but who cares bc we only use 3
  full_join(ninetyonebadstations, by = c("Station", "Year")) 

FutureYears = as_tibble(seq(from = 2021, to = 2060, by = 1)) %>% rename("Year" = "value")

####Baseline 1991-2000 data: Here is a spot where we select(only vars we need)####
base_df =  baseline_69vars %>% 
  filter(!Year == "1991") %>% #take out the 1991 data real quick bc we gonna bind it back
  bind_rows(ninetyone_Base) %>% 
  filter(!Station %in% c("LE5.5-W","TF3.1E","TF3.2A","WXT0001","XHH4742")) %>% #take out the problem stations too
  bind_rows(ninetyonebadstations_Base) %>% 
  filter(!Station == "LE5.5") %>%
  #mutate(Station = replace(Station, Station == "LE5.5", "LE5.5-W")) %>%
  bind_rows(replicate(3, ., simplify = FALSE)) %>% #then add 3 more onto the bottom of it
  select(Station, Year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, #selecting only what we use
         Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summax,
         Chla.spme, Chla.summe, Chla.summax, Sal.summed, Sal.spme, Sal.summe, 
         Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe) %>% 
  arrange(Station, Year) %>% 
  select(-Year) %>%
  group_by(Station) %>%
  mutate(slice_sample(FutureYears, n = 40, replace = F)) %>% #HERE, create the years that these baseline years will match up with
  mutate(base = rep("base", n = 40)) %>%
  select(Station, Year, everything()) %>%
  arrange(Station, Year) %>% 
  ungroup()

#na check:
#most of the NAs are from 1991 y1s, lots in the depth column too
View(base_df %>% group_by(Station)%>%
       summarise(across(everything(), ~ sum(is.na(.))))) %>% 
#       select(Station, Year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe))

#base_df shouldnt need changing now.

##Three Future datasets here####
#To then build the dataframe of all future values, first create the "scalar" dataset, the change values for the future: 
#load in projections for each scenario
CC.wland_2021_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CC.wland_2021_2060_69vars.csv") 
WIP.wland_2021_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.wland_2021_2060_69vars.csv")
WIP.woland_2031_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.woland_2031_2060_69vars.csv")

#Tidying up the Projection data####
#Starting with CC.wland: Here is a spot where we select(only vars we need)####
# proactively fix the y1 problem here by grabbing 2020 data, which will become the 2021 y1 data after some magic
twentyone = CBP.WQ_69vars %>% filter(year == "2020") %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  #slice_sample(n = 1 , replace = T) %>% #pick a random year
  mutate(Year = case_when(year == 2020 ~ 2021)) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION) %>%
  select(Station, Year, everything()) %>% select(-year) %>% 
  replace_na(list(Temp.sumy1me = 31.2550, Temp.sumy1med = 31.2550, Sal.sumy1max = 0.001)) %>% 
  ungroup() 
#TF5.2 is missing Temps so I used the temp from TF5.2A med = 31.2550 mean = 31.25500
# and all of the TF stations and RET5.1A have -Inf salinities that need to be 0

##Do this for each scenario
twentyone_CC.wl = CC.wland_2021_2060_69vars %>% filter(Year == "2021") %>%
  #select(Station, Year, everything()) %>%
  select(-Temp.sumy1med, -Temp.sumy1me, -Sal.sumy1max) %>% 
  full_join(twentyone, by = c("Station", "Year")) %>%
  select(Station, Year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, #selecting only what we use
         Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summax,
         Chla.spme, Chla.summe, Chla.summax, Sal.summed, Sal.spme, Sal.summe, 
         Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe)

####Here is a spot where we select(only vars we need)####
CC.wl_2021_2060 = CC.wland_2021_2060_69vars %>% filter(!Year == "2021") %>%
  bind_rows(twentyone_CC.wl) %>%
  filter(!Station == "LE5.5") %>% #take this out bc we have 5.5-W instead. alsp TF5.5 is trash and we have 5.5A
  #mutate(Station = replace(Station, Station == "LE5.5", "LE5.5-W")) %>%
 # replace_na(list(Temp.sumy1me = 27.489, Temp.sumy1med = 26.74)) %>% #its just two stations, LE5.5-W and XHH4742 that have 2021 NAs. replaced w a median point but still a salinity problem so use the lapply code below
  select(Station, Year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, #selecting only what we use
         Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summax,
         Chla.spme, Chla.summe, Chla.summax, Sal.summed, Sal.spme, Sal.summe, 
         Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe) %>% 
  arrange(Station, Year) %>%
  ungroup()
#na check
#nas are because there is no 2021 y1 data, which is why we do the twentyone_CC fix above.
View(CC.wl_2021_2060 %>% group_by(Station, Year) %>% 
       summarise(across(everything(), ~ sum(is.na(.))))) #%>%
     #  select(Station, Year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe))
#if still one NA, you can do this: 
#this code is supposed to replace NAs in the df with a random selection from the column
#CC.wl_2021_2060[] <- CC.wl_2021_2060 %>% group_by(Station) %>%
#  lapply(.,function(x) 
#    replace(x,is.na(x), sample(x[!is.na(x)],sum(is.na(x)))))


#Chris' Notes:: ####
#The forecast is based on a baseline set from 1991- 2000
#So I think we create a vector of years from 1990 to 2020 than randomly select, with replacement, 30 years. Ie. 2002, 2017, 2004, 2011, etc, etc,
#Then we stack those years up to create a randomly generated 30 year record from 2021 to 2050. 
#Then we add on the delta values for each year to create that vector of change. 
#Then we run the predictions for SAV stepwise and save those outputs.
#Then we do it again and again 1000 times in a row.  

allyr = CC.wl_2021_2060 %>% select(Year) #select the uears and stations (5760) for pasting
allsta = CC.wl_2021_2060 %>% select(Station)


#HMM: shouldnt this be Future - Baseline = future delta change??? I had it the other way initially...
deltaCC.wl1 = (CC.wl_2021_2060 %>% select(-Year, -Station)) - ((base_df %>% select(-Year, -Station))) 

#Delta Change DF####
#add the year and station columns back in to create Delta change DF
deltaCC.wl_df = CC.wl_2021_2060 %>% 
  group_by(Station) %>% 
  bind_rows(base_df) %>% 
  arrange(Year, Station) %>%
  group_by(Station, Year) %>%
  select(-base) %>%
  summarise(across(everything(), ~.x - lead(.x))) %>%
  drop_na()
  
  
  
  
  deltaCC.wl1 %>% #should be 5760
  mutate(CC.wl_2021_2060 %>% select(Year)) %>% 
  mutate(CC.wl_2021_2060 %>% select(Station)) %>% 
  filter(!Station == "LE5.5") %>% #take this out bc we have 5.5-W instead. alsp TF5.5 is trash and we have 5.5A
  select(Station, Year, everything()) %>%
  rename(STATION = Station) %>%
  arrange(STATION, Year)  

#nacheck
#LE5.5 & XHH4742 have Sal.sumy1 NAs unless you do the lapply code or insert values
View(deltaCC.wl_df %>% summarise(across(everything(), ~ sum(is.na(.)))))

#select random years from the real life CBP WQ data to generate a DF that is the "NO Climate Change" prediction df. Basically meaning that, for the next 40 years, every year is just goign to be the same as one of the last set of years 
####No Climate Change scenario df: Here is a spot where we select(only vars we need)####
NoCC_df = CBP.WQ_69vars %>% 
  select(STATION, year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, #selecting only what we use
         Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summax,
         Chla.spme, Chla.summe, Chla.summax, Sal.summed, Sal.spme, Sal.summe, 
         Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe) %>%
  filter(!STATION == "LE5.5") %>% #take this out bc we have 5.5-W instead. alsp TF5.5 is trash and we have 5.5A
  filter(!year %in% c(1984, 1985, 1986, 2020)) %>% #filter out 84 for sure (no y1) but lots of NAs in these other first few years
  #select(-year) %>% #take out "past" years, might need this to explore where NAs are coming from though
 # drop_na() %>% #not so sure about this but it could work since we want complete years? hmm 80 points get dropped and the DFs need to match up
  group_by(STATION) %>%
  slice_sample(n = 40 , replace = T) %>% #select a random 40 years from each Station. insane that this doesnt have a "select no NAs"
#  mutate(Temp.summin = case_when(Temp.summin == "Inf" ~ 0.001, 
 #                          TRUE ~ Temp.summin)
  #replace_na(across(Temp.sumy1med:TN.summe, sample_n(., size = 1, replace = T)) %>%
  mutate(Year = seq(2021, 2060, by = 1)) %>% #Add in the years that will be matched up to future. Should this be randomized?
  ungroup() #need to keep it grouped for the lapply code to work

#this code is supposed to replace NAs in the df with a random selection. group_by aint workin tho... 
#im not so sure it does that though....group_by may need to be inside the function?
#NoCC_df[] <- NoCC_df %>% group_by(STATION) %>%
#lapply(.,function(x) 
#   replace(x, is.na(x), sample(x[!is.na(x)],sum(is.na(x)))))

 #Check nas
View(NoCC_df %>% group_by(STATION, year) %>%
       summarise(across(everything(), ~ sum(is.na(.)))))

compare_df_cols(NoCC_df, deltaCC.wl_df)

futureprobs = NoCC_df %>%
  bind_rows(deltaCC.wl_df)

####STOPPED HERE FOR DATA RECONFIG####
#eventually this will be the 1000x thing, to create 1000 different predicted value DFs, but for now, 1x: 
PredVals.CC.wl = NoCC_df %>%
  bind_rows(deltaCC.wl_df) %>% 
  group_by(STATION, Year) %>% select(-year) %>%
  summarise(across(everything(), ~sum(.))) %>% #this adds the delta to the No Climate Change (CBP.WQ original data).
 # mutate(Sal.sumy1max = case_when(Sal.sumy1max < 1 ~ 1, #change all the negative (impossible) and less than 1 (will get fucked from log10()) salinities to 1
  #                                Sal.sumy1max == "Inf" ~ 1,
   #                               TRUE ~ Sal.sumy1max)) %>%
  select(-year) %>%
  ungroup() 


is.na(PredVals.CC.wl) <- PredVals.CC.wl == "NaN"
is.na(PredVals.CC.wl) <- PredVals.CC.wl == "Inf"
is.na(PredVals.CC.wl) <- PredVals.CC.wl == "-Inf"

View(PredVals.CC.wl %>% summarise(across(everything(), ~ sum(is.na(.)))))

###quick graph check####
ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = Temp.summe),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = PredVals.CC.wl %>% filter(Station == "CB7.1"), aes(x = Year, y = Temp.summe),
               geom = "smooth", fun.data = mean_se, color = "green") +
  stat_summary(data = baseline_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = Temp.summe),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "CB7.1"), aes(x = year, y = Temp.summe),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  labs(x = "", y = expression("Temp, summer mean, station CB7.1")) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank())

#####Here is the "Prediction under Climate Change DF, done once, for the CC WLand scenario####

#Predict Zostera

#LMER from Zostera SEM (from community change model building.R)

#####

ZoInt.lmer <- lme(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)), random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = ZoDensWQ69sem.No0 %>% filter(!STATION %in% c("CB8.1", "CB8.1E", "LE4.2", "EE3.3", "LE5.3"))) #removing these now doesnt change the significance but still might be a good idea. 772 to 647 observations
summary(ZoInt.lmer)
car::Anova(ZoInt.lmer)

#need Zos only density data?
ZoDensTime = read.csv("~/Documents/R projects/Predicting-SAV/data/ZoDensTime.csv")  %>%
  filter(!STATION %in% c("CB8.1", "CB8.1E", "LE4.2", "EE3.3", "LE5.3")) #%>% rename(Station = STATION) #take out this station w no grass

#build the newdata

##########LETS JUST FUCKING DO IT ROW BY ROW########
str(PredVals.CC.wl)

#only need one row of ZoDensTime per station
ZDT2019 = ZoDensTime %>% filter(year == "2019") %>% 
  mutate(Year = case_when(year == 2019 ~ 2020)) %>% # we just gonna pretend this is 2020 data 
  select(STATION, Year, dens.percomp)

#Grab the 2021 data we are missing for Zostera. this is a Zostera only solution unfortunately
twentyone_Tempsummed = twentyone %>% select(STATION, Year, Temp.summed) %>%
  filter(STATION %in% ZoDensTime$STATION) %>%
  rename(Temp.sumy1med = Temp.summed) #bind this to the Zostera data 

#ZoPredVals_CC.wl = PredVals.CC.wl %>% #Projected Future Env Variables
#  select(STATION, Year, Temp.sumy1med, Sal.summed, Chla.spme, Secc.summe, Temp.spmed) %>% #select what we need only, based on ZoInt formula
#  filter(STATION %in% ZoDensTime$STATION) %>% #Zostera stations only
#  add_column(dens.percomp = rep(NA, 800)) %>% #add in a Density column
#  bind_rows(ZDT2019) %>%
#  select(-Temp.sumy1med) %>%
#  select(STATION, Year, dens.percomp, everything()) %>% 
#  arrange(STATION, Year) %>% #runa bove this
#  left_join(fullTemp)
  
#fullTemp = ZoPredVals_CC.wl %>% select(STATION, Year, Temp.sumy1med) %>% drop_na()

write.csv(ZoPredVals_CC.wl, "~/Documents/R projects/Predicting-SAV/data/ZoPredVals_CC.wl.csv")

coef(ZoInt.lmer)

PredictZo2021 = ZoPredVals_CC.wl %>% 
  group_by(STATION) %>%
  mutate(dens.percomp.y1 = lag(dens.percomp, order_by = Year)) %>% #recreate the dpcy1 column so its all on one row
  filter(Year == 2021) %>%#lest just try one year for now
  select(-dens.percomp, -Year) %>% #this builds the 2021 DF completed
  drop_na() %>% ungroup() %>%
  mutate(DPCC = predict(ZoInt.lmer, newdata = .)) %>%
  mutate(dens.percomp = DPCC + dens.percomp.y1)
 
  
  # summarise(pred.DC = ((-1.832017*dens.percomp.y1) +
  #                    (-1.61951*log10(Temp.sumy1med)) +
  #                    (-0.2834316*log10(Sal.summed)) +
 #                     (-0.1102446 *log10(Chla.spme)) +
 #                     (0.2961508*log10(Secc.summe)) +
 #                     (0.3499578*(dens.percomp.y1:log10(Temp.spmed)))+
 #                     (1.594469*(dens.percomp.y1:log10(Sal.summed)))+
 #                     (0.01992432*(dens.percomp.y1:log10(Chla.spme)))+ 
 #                     (-0.0672103*(dens.percomp.y1:log10(Secc.summe)))))


DPCC2021 = predict(ZoInt.lmer, newdata = PredictZo2021)

#can we make a fuunction
predict.ZoInt <- function(x) {
  fit <- ZoInt.lmer <- lme(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)), random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = ZoDensWQ69sem.No0) 
  pred <- predict(fit, newdata = PredVals.CC.wl[x+1, ], se.fit = TRUE)
  c(summary(fit)$adj.r.squared, pred$fit, pred$se.fit)
}

predict.ZoInt(PredVals.CC.wl)


rolling_Zo <- rollify(~median(.x, na.rm = T), window = 10)

####
ZoDensFuture_CC.wl <- PredVals.CC.wl %>%
  filter(STATION %in% ZoDensTime$STATION) %>% #grab just the stations in the ZoZone
###ACTUALLY: what we need to do here is to grab 2019 and 2020 rows from the real data to use here.  
  full_join(ZoDensTime %>% filter(year == "2019")) %>% #Grab 2019 grass only 
  select(-oYear, -year) %>% 
  select(STATION, Year, dens.percomp.change, dens.percomp.y1, Temp.sumy1med, Sal.summed, Chla.spme, Secc.summe, Temp.spmed, Sal.summed)  %>% #for drop_na, try to select only the things you need bc itll drop too many points
  drop_na() #NOTE: using y1 here drops 2021 data bc there isnt a sumy1 for 2021... 

#predict
ZoDPCFut_CC.wl  <- predict(ZoInt.lmer, newdata = ZoDensFuture_CC.wl, re.form = ~(1|STATION), interval = "confidence")
#do i need to do bootstrapped CI?

#put the predicted in the DF
ZoFuture_CC.wl = ZoDensFuture_CC.wl %>% add_column(DPC_predict = ZoDPCFut_CC.wl)



ZoDPC2022_CC.wl  <- predict(ZoInt.lmer, newdata = ZoDensFuture_CC.wl %>% filter(Year == 2022), re.form = ~(1|STATION), interval = "confidence")

ZoDPC2022_CC.wl + dens.percomp.y1 = dens.percomp.2022

#put the predicted in the DF
ZF2022 = ZoDensFuture_CC.wl %>% filter(Year == 2022) %>%
  add_column(DPCC_2022 = as.numeric(predict(
    ZoInt.lmer, newdata = ., re.form = ~(1|STATION)))) %>%   
  group_by(STATION) %>% 
  mutate(DPC_2022 = DPCC_2022 + dens.percomp.y1) %>%
  add_column(DPCC_2023 = predict(ZoInt.lmer, newdata = ZoDensFuture_CC.wl %>% filter(Year == 2023), re.form = ~(1|STATION), interval = "confidence"))





#############
##IF this is right, here would be the full workflow that needs to be repeated 1000x: 

predtest1 = CBP.WQ_69vars %>% #start w the NO CC data
  select(-contains("TSS")) %>% #no TSS in the climate data so remove this
  rename(oYear = year) %>% 
  group_by(STATION) %>%
  slice_sample(n = 40 , replace = T) %>% #select a random 40 years from each Station
  mutate(Year = seq(2021, 2060, by = 1)) %>% #Add in the years that will be matched up to future
  select(STATION, Year, oYear, everything()) %>% #%>% rownames_to_column()  
  ungroup() %>%
  bind_rows(deltaCC.wl_df) %>% #Year and Station will match up to bind
  group_by(STATION, Year) %>%
  summarise_all(sum) %>%  #this adds the delta to the CBP.WQ original data
  filter(STATION %in% ZoDensTime$STATION) %>% #grab just the stations in the ZoZone
  full_join(ZoDensTime %>% filter(year == "2019")) %>% #Grab 2019 grass only
  select(-oYear) %>% 
  select(STATION, Year, dens.percomp.change, dens.percomp.y1, Temp.sumy1med, Sal.summed, Chla.spme, Secc.summe, Temp.spmed, Sal.summed)  %>% #for drop_na, try to select only the things you need bc itll drop too many points
  drop_na() %>%
  add_column(pred.DPC = predict(ZoInt.lmer, newdata = ., re.form = ~(1|STATION), interval = "confidence")) %>% #add in the predicteds
  select(STATION, Year, pred.DPC, )





#use that formula to precict area (dens.weight.mean)
preZoFut_CC.wl <-predict(ZoFuture_CC.wl) #this lme is the dens.percomp.change
predZoFutdwm_CC.wl <- preZoFut_CC.wl * ZoDensFuture_CC.wl$denscomp.max

#prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + pred.dwm, data = Rm_SEM)
#summary(prdSEM.lm)




##First cut, wrong but dont delete yet####
#First, create a random vector of years (selected random w replacement) for each station, parameter, (and each decade????)


CC.w_Rand.2130 = CC.wland_2021_2060_69vars %>%
  group_by(Station) %>% 
  filter(between(Year, 2021, 2030)) %>% #if decade scalars are real, then need to do this i guess? 
  slice_sample(n = 10 , replace = T) %>% #n = 10, if doing by decade?
  
  #this gives me 10 rows per station. so then id need to do baseline - these for a decade scalar. 
 
#scalar calculation for 2021-2030. do this by decade bc the scalars should change w decade?
  scal_CC.w = (baseline_69vars  %>% select(-Year, -Station)) - (CC.wland_2021_2060_69vars %>% filter(between(Year, 2021, 2030)) %>% select(-Year, -Station))

baseYr = baseline_69vars$Year
baseSt = baseline_69vars$Station

#this DF is the difference (i.e., the scalar?) between baseline and 2021-2030 decade
delta_CC.w2130 = scal_CC.w %>% 
  add_column(baseline_69vars$Year) %>% add_column(baseline_69vars$Station) %>% 
  add_column(Decade = "2020s") %>%
  rename(Year = "baseline_69vars$Year", Station = "baseline_69vars$Station") %>%
  select(Year, Station, Decade, everything())



##TEST: Project Ruppia in to 2021-2030

delta_CC.w2130.spring = delta_CC.w2130 %>% 
  select(Station, Chla.spme, Secc.spme, Sal.spme, Temp.spme, TP.spme, TN.spme) %>%
  rename(STATION = Station)

Ru2130WQ = RuDensWQsem.No0 %>% filter(year == 2019) %>%
  inner_join(delta_CC.w2130.spring, by = "STATION") %>% 
  group_by(STATION) %>%
  summarise(Chla.spme = Chla.spme.x - Chla.spme.y)

RuDensWQsem.No0

#Ruppia
RuInt <- lme(dens.percomp.change ~ dens.percomp.y1 +
               # log10(Sal.spme) + log10(Chla.spme) + 
               log10(TP.spme) + log10(TN.spme) + log10(Secc.spme) + log10(Temp.spme) +
               (dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(Chla.spme)) + 
               (log10(TP.spme):dens.percomp.y1) + (log10(TN.spme):dens.percomp.y1) +
               (log10(Secc.spme):dens.percomp.y1) + (log10(Temp.spme):dens.percomp.y1),
             random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),
             control = lmeControl(opt = "optim"), data = RuDensWQsem.No0)

#use that formula to precict area (dens.weight.mean)
preRuInt <-predict(RuInt) #this lme is the dens.percomp.change
pred.Rudwm <- preRuInt * RuDensWQsem.No0$denscomp.max

#prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + pred.dwm, data = Rm_SEM)
#summary(prdSEM.lm)

RuPred.lmer <- lme(dens.weight.mean ~ dens.weight.mean.y1 + pred.Rudwm, 
                   random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"), data = RuDensWQsem.No0)

summary(RuPred.lmer)
r2(RuPred.lmer)
  
  