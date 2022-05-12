#Code to build the SAV change and water quality over time dataframe for each of the 4 communities
library(tidyverse); library(readxl)

#load in the Water Quality data. I think most use the 69vars df,
#CBP.WQ_69vars <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
#CBP.WQ_combined <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_combined.csv")
#CBP.WQ_combined = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_combined.csv")

#CBPsimp.WQ_combined <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBPsimp.WQ_combined.csv")
#CBPsimp.WQ_combined = read.csv("~/Documents/R projects/Predicting-SAV/data/CBPsimp.WQ_combined.csv")

#load in DF of cluster groups per station
#clusters = read.csv("~/Documents/R projects/TraitsSAV/spp.prop.cluster.csv")

#community.desig = clusters %>% select(STATION, LATITUDE, LONGITUDE, clust.group, clust.no) #now use these clust.group to filter data! 

#NEW!!! 1/20/22: We redid the Overlap w Stations Zones ####


AllCommunitiesDensityTime <- readxl::read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/SAV Area by Year by Station Zone and Species Cluster.xlsx")
Overlap_AllStations <- readxl::read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/SAV Composite with max density by Station, Species Cluster, and Density.xlsx")

#calculate mean area to density conversions: 
dwm.to.HA_Zo = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% filter(SpCluster == "Zostera"))
dwm.to.HA_Ru = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% filter(SpCluster == "Ruppia"))
dwm.to.HA_MM = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% filter(SpCluster == "MixedMeso"))
dwm.to.HA_F = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% filter(SpCluster == "Fresh"))

SAVCommunityDens_AllStations %>% group_by(Station, Year) %>% filter(SpCluster == "Zostera") %>%
  mutate(pred.Area = predict(dwm.to.HA_Zo, newdata = .))

AllStationsByCommunity <- Overlap_AllStations %>% 
  group_by(SpCluster) %>%
  pivot_wider(names_from = Density, values_from = SAV_HA, names_prefix = "SAVdens") %>%
  group_by(Station, SpCluster) %>%
 # dplyr::filter(RMZoneSAV_HA > 0) %>% #filter out non-Ruppia: 51 total stations
  mutate(denscomp.max = (SAVdens4*.85) + (SAVdens3*.55) + (SAVdens2*.25) + (SAVdens1*.05)) %>% #calculate max composite area
  mutate(SAV_HA.max = SAVdens4+SAVdens3+SAVdens2+SAVdens1) %>%
  select(Station, SpCluster, denscomp.max, SAV_HA.max)

SAVCommunityDens_AllStations <- AllCommunitiesDensityTime %>%
  rename(SpCluster = SPCluster) %>%
  #filter(between(Year, 1990, 2019)) %>% use this if you want to create a certain time dataset
  mutate(per.cov = case_when(Density == 1 ~ .05,
                             Density == 2 ~ .25, 
                             Density == 3 ~ .55, 
                             Density == 4 ~ .85)) %>% #convert to density weighted means 
  mutate(dens_cov = Area_HA * per.cov) %>%
  group_by(SpCluster, Station, Year) %>%
  summarize(dens.weight.mean = sum(dens_cov), 
            SAVArea = sum(Area_HA)) %>%
  mutate(dens.weight.mean.y1 = lag(dens.weight.mean, order_by = Year, k = 1), 
         SAVArea.y1 = lag(SAVArea, order_by = Year, k = 1)) %>%
  full_join(AllStationsByCommunity) %>% group_by(Station, SpCluster) %>% 
  mutate(dens.percomp = dens.weight.mean/denscomp.max, 
         dens.percomp.y1 = dens.weight.mean.y1/denscomp.max, 
         SAVArea.percomp = SAVArea/SAV_HA.max, 
         SAVArea.percomp.y1 = SAVArea.y1/SAV_HA.max) %>% 
  mutate(dens.percomp.change = (dens.percomp-dens.percomp.y1), 
         SAVArea.percomp.change = (SAVArea.percomp-SAVArea.percomp.y1)) %>% 
  select(SpCluster, Station, Year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, SAVArea.percomp.change, SAVArea, denscomp.max, SAV_HA.max) %>%
  filter(!denscomp.max < 1) #filter out teenie stations? if not this, == 0 so at least get the 0s out

#New MASTER Bay Community SAV Dataset!!!#####
write_csv(SAVCommunityDens_AllStations, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommunityDens_AllStations.csv")
write_csv(SAVCommunityDens_AllStations, "~/Documents/R projects/Predicting-SAV/data/SAVCommunityDens_AllStations.csv")

#CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
#CBP.WQ_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_69vars.csv")

#SAVCommDensWQ_69 <- SAVCommunityDens_AllStations %>%
#  rename(year = Year, STATION = Station) %>%
 # left_join(CBP.WQ_69vars)

CBP.WQ_forPredictions = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBP.WQ_forPredictions.csv")

SAVCommDensWQ_forPredictions <- SAVCommunityDens_AllStations %>%
  rename(year = Year, STATION = Station) %>%
 left_join(CBP.WQ_forPredictions)

vroom_write(SAVCommDensWQ_forPredictions, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_forPredictions.csv")
vroom_write(SAVCommDensWQ_forPredictions, "~/Documents/R projects/Predicting-SAV/data/SAVCommDensWQ_forPredictions.csv")




#nacheck
#View(SAVCommDensWQ_69 %>% group_by(year) %>% 
#       summarise(across(everything(), ~ sum(is.na(.)))) %>%
 # select(STATION, year, dens.percomp.change, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe))

#New MASTER Bay SAV Density and WQ Dataset!!!#####
#write_csv(SAVCommDensWQ_69, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69.csv")
#write_csv(SAVCommDensWQ_69, "~/Documents/R projects/Predicting-SAV/data/SAVCommDensWQ_69.csv")















#THIS IS NOT NEEDED ANYMORE#########
#
##
###
####Community 1: Ruppia maritima monoculture####
###
##
#
#load in data to make SAV density over time per zone DF
RuppiaOverlap_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Ruppia SAV Zones Overlap with Station Zones.xlsx")


####NOTE: NEED TO REDO THIS ABOVE FROM THE SAV AREA BY YEAR BY STATION ZONE FILE BECAUSE WE ARENT GOING TO BE UPDATING TO 2020, THIS ABOVE FILE####

RuppiaStations <- RuppiaOverlap_StationZone %>% 
  dplyr::filter(RMZoneSAV_HA > 0) %>% #filter out non-Ruppia: 51 total stations
  mutate(denscomp.max = (RMZoneD4_Ha*.85) + (RMZoneD3_Ha*.55) + (RMZoneD2_Ha*.25) + (RMZoneD1_Ha*.05)) %>% #calculate max composite area
  select(STATION, denscomp.max, RMZoneSAV_HA) #clean up this DF

#load in SAV area by year for Rupppia stations
RuppiaDensityTime <- readxl::read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Ruppia SAV Area by Year by Station Zone.xlsx")

#calculate Ruppia coverage in each station zone for each year and year - 1
#response variables created here: 
#!!dens.percomp.change and related dens.percomp are dens weighted means, scaled from the maxiumum composite extent in each Station zone. 
#Also: dens.weight.mean = total density-weighted-mean area in zone, SAVArea = total area of SAV in zone, dens.change = change in dens weight mean area from y1, SAVArea.change, dens.prop.change = scaled density weighted area change, SAVArea.prop.change = scaled area change

##Station Ruppia change over time#
RuDensTime <- RuppiaDensityTime %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  #filter(between(Year, 1990, 2019)) %>% use this if you want to create a certain time dataset
  mutate(per.cov = case_when(Density == 1 ~ .05,
                             Density == 2 ~ .25, 
                             Density == 3 ~ .55, 
                             Density == 4 ~ .85)) %>% #convert to density weighted means 
  mutate(dens_cov = SAVAreaHa * per.cov) %>%
  group_by(STATION, Year) %>%
  summarize(dens.weight.mean = sum(dens_cov), 
            SAVArea = sum(SAVAreaHa)) %>%
  mutate(dens.weight.mean.y1 = lag(dens.weight.mean, order_by = Year, k = 1), 
         SAVArea.y1 = lag(SAVArea, order_by = Year, k = 1)) %>%
  full_join(RuppiaStations) %>% group_by(STATION) %>% 
  mutate(dens.percomp = dens.weight.mean/denscomp.max, 
         dens.percomp.y1 = dens.weight.mean.y1/denscomp.max, 
         SAVArea.percomp = SAVArea/RMZoneSAV_HA, 
         SAVArea.percomp.y1 = SAVArea.y1/RMZoneSAV_HA) %>% 
  mutate(dens.percomp.change = (dens.percomp-dens.percomp.y1), 
         SAVArea.percomp.change = (SAVArea.percomp-SAVArea.percomp.y1)) %>% 
  rename("year" = "Year") %>% 
  select(STATION, year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, SAVArea.percomp.change, SAVArea, denscomp.max)



#Full DF all Stations. Use spring data instead, SKIP THIS####
#Here is I recommend not using this DF (even simp): when we drop_na() for the sem, there are too many chances for a dumb NA kick out (e.g., who cares if this one datapoint didnt have a TN.sumy1ran??)
RuDensWQ_combined.ALL <- CBPsimp.WQ_combined %>%
  filter(STATION %in% RuppiaStations$STATION) 

is.na(RuDensWQ_combined.ALL) <- RuDensWQ_combined.ALL == "Inf"
is.na(RuDensWQ_combined.ALL) <- RuDensWQ_combined.ALL == "-Inf"
RuDensWQ_combined.ALL <- as.data.frame(RuDensWQ_combined.ALL)

#Bad STATIONS####
#filter out some of the Stations that are mostly 0s
#RuDensTime_trim = RuDensTime %>% 
#  filter(!STATION %in% c("TF1.7", "WT7.1", "WT8.2", "RET1.1", 
#                         "LE3.1", "WT8.3", "CB3.3W"))


#merge it with the ENV data from tidyCBPWQ_2020
RuDensWQ_combined <- CBPsimp.WQ_combined %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  full_join(RuDensTime)

is.na(RuDensWQ_combined) <- RuDensWQ_combined == "Inf"
is.na(RuDensWQ_combined) <- RuDensWQ_combined == "-Inf"
RuDensWQ_combined <- as.data.frame(RuDensWQ_combined)

#Ruppia change and WQ in stations over time (2019)
#write_csv(RuDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/RuDensWQ_combined.csv")
#write_csv(RuDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/RuDensWQ_combined.csv")

#Spring Ru Data Merge USE THIS#####
#merge it with the Spring ENV data from tidyCBPWQ_2020
CBP.WQ_spme = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_spme.csv")
RuDensWQ_spme <- CBP.WQ_spme %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  full_join(RuDensTime)

RuDensWQ_spme[is.nan(RuDensWQ_spme)] <- 0
is.na(RuDensWQ_spme) <- RuDensWQ_spme == "Inf"
is.na(RuDensWQ_spme) <- RuDensWQ_spme == "-Inf"
RuDensWQ_spme <- as.data.frame(RuDensWQ_spme)

#Ruppia change and Spring WQ in stations over time (2019)
#write_csv(RuDensWQ_spme, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/RuDensWQ_spme.csv")
write_csv(RuDensWQ_spme, "~/Documents/R projects/Predicting-SAV/data/RuDensWQ_spme.csv")


#69 Vars Ru Data Merge#####
#merge it with the 69vars ENV data from tidyCBPWQ_2020
CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
RuDensWQ_69 <- CBP.WQ_69vars %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  full_join(RuDensTime)

RuDensWQ_69[is.nan(RuDensWQ_69)] <- 0
is.na(RuDensWQ_69) <- RuDensWQ_69 == "Inf"
is.na(RuDensWQ_69) <- RuDensWQ_69 == "-Inf"
RuDensWQ_69 <- as.data.frame(RuDensWQ_69)

#Ruppia change and Spring WQ in stations over time (2019)
#write_csv(RuDensWQ_69, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/RuDensWQ_69.csv")
write_csv(RuDensWQ_69, "~/Documents/R projects/Predicting-SAV/data/RuDensWQ_69.csv")

#
##
###
#Community 2: Zostera Monoculture####
###
##
#

ZosteraOverlap_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Ruppia/Ruppia areas in Chesapeake Bay/Zostera SAV Zones Overlap with Station Zones.xlsx")

ZosteraStations <- ZosteraOverlap_StationZone %>% 
  dplyr::filter(ZMZoneSAV_HA > 0) %>% #filter out non-Zostera: 25 total stations
  mutate(denscomp.max = (ZMZoneD4_Ha*.85) + (ZMZoneD3_Ha*.55) + (ZMZoneD2_Ha*.25) + (ZMZoneD1_Ha*.05)) %>% #calculate max composite area
  select(STATION, denscomp.max, ZMZoneSAV_HA) #clean up this DF

#load in SAV area by year for Rupppia stations
ZosteraDensityTime <- read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Zostera SAV Area by Year by Station Zone.xlsx")

#calculate Zostera coverage in each station zone for each year and year - 1

####Station Zostera change over time####
#denspercomp only... use this for master code file
ZoDensTime <- ZosteraDensityTime %>%
  filter(STATION %in% ZosteraStations$STATION) %>%
  #filter(between(Year, 1990, 2019)) %>% use this if you want to create a certain time dataset
  mutate(per.cov = case_when(Density == 1 ~ .05,
                             Density == 2 ~ .25, 
                             Density == 3 ~ .55, 
                             Density == 4 ~ .85)) %>% #convert to density weighted means 
  mutate(dens_cov = SAVAreaHa * per.cov) %>%
  group_by(STATION, Year) %>%
  summarize(dens.weight.mean = sum(dens_cov), #sum density weighted area
            SAVArea = sum(SAVAreaHa)) %>%
  mutate(dens.weight.mean.y1 = lag(dens.weight.mean, order_by = Year, k = 1), 
         SAVArea.y1 = lag(SAVArea, order_by = Year, k = 1)) %>%
  full_join(ZosteraStations) %>% group_by(STATION) %>% 
  mutate(dens.percomp = dens.weight.mean/denscomp.max, 
         dens.percomp.y1 = dens.weight.mean.y1/denscomp.max, 
         SAVArea.percomp = SAVArea/ZMZoneSAV_HA, 
         SAVArea.percomp.y1 = SAVArea.y1/ZMZoneSAV_HA) %>% 
  mutate(dens.percomp.change = (dens.percomp-dens.percomp.y1), 
         SAVArea.percomp.change = (SAVArea.percomp-SAVArea.percomp.y1)) %>% 
  rename("year" = "Year") %>% 
  select(STATION, year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, SAVArea.percomp.change, SAVArea, denscomp.max)

write_csv(ZoDensTime, "~/Documents/R projects/Predicting-SAV/data/ZoDensTime.csv")

hist(ZoDensTime %>% filter(dens.weight.mean > 0) %>% pull(dens.weight.mean))

#filter out some of the Stations that are mostly 0s
#ZoDensTime_trim = ZoDensTime %>% 
#  filter(!STATION %in% c("CB8.1", "CB8.1E", "LE4.2", "EE3.3", "LE5.3"))

#merge it with the ENV data from tidyCBPWQ_2020
ZoDensWQ_combined <- CBPsimp.WQ_combined %>%
  filter(STATION %in% ZosteraStations$STATION) %>%
  full_join(ZoDensTime)

qplot(x = Sal.med, y = dens.percomp.change, data = ZoDensWQ_combined)


is.na(ZoDensWQ_combined) <- ZoDensWQ_combined == "Inf"
is.na(ZoDensWQ_combined) <- ZoDensWQ_combined == "-Inf"
ZoDensWQ_combined <- as.data.frame(ZoDensWQ_combined)

#Zostera change and WQ in stations over time (2019)
#write_csv(ZoDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/ZoDensWQ_combined.csv")
#write_csv(ZoDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/ZoDensWQ_combined.csv")

qplot(x = dens.percomp.y1, y = dens.percomp, data = ZoDensWQ_combined)


#merge it with the Spring ENV data from tidyCBPWQ_2020
ZoDensWQ_spme <- CBP.WQ_spme %>%
  filter(STATION %in% ZosteraStations$STATION) %>%
  full_join(ZoDensTime)

ZoDensWQ_spme[is.nan(ZoDensWQ_spme)] <- 0
is.na(ZoDensWQ_spme) <- ZoDensWQ_spme == "Inf"
is.na(ZoDensWQ_spme) <- ZoDensWQ_spme == "-Inf"
ZoDensWQ_spme <- as.data.frame(ZoDensWQ_spme)

#Zostera change and Spring WQ in stations over time (2019)
#write_csv(ZoDensWQ_spme, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/ZoDensWQ_spme.csv")
#write_csv(ZoDensWQ_spme, "~/Documents/R projects/Predicting-SAV/data/ZoDensWQ_spme.csv")

#69 Vars Zostera Data Merge#####
#merge it with the 69vars ENV data from tidyCBPWQ_2020
CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
ZoDensWQ_69 <- CBP.WQ_69vars %>%
  filter(STATION %in% ZosteraStations$STATION) %>%
  full_join(ZoDensTime)

ZoDensWQ_69[is.nan(ZoDensWQ_69)] <- 0
is.na(ZoDensWQ_69) <- ZoDensWQ_69 == "Inf"
is.na(ZoDensWQ_69) <- ZoDensWQ_69 == "-Inf"
ZoDensWQ_69 <- as.data.frame(ZoDensWQ_69)

#Ruppia change and Spring WQ in stations over time (2019)
#write_csv(ZoDensWQ_69, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/ZoDensWQ_69.csv")
write_csv(ZoDensWQ_69, "~/Documents/R projects/Predicting-SAV/data/ZoDensWQ_69.csv")


#may want to filter
#RmZone_Env9010.tss <- RmZone_Env.tss %>%
#filter(!dens.percomp.y1 > 0.90) %>% filter(!dens.percomp.y1 < 0.10)

#
##
###
#Prep MixMeso and Fresh communities here####

#this is actually data prep for both of the next two community DFs
BaeOverlap_StationZone <- read_excel("~/Documents/R projects/Predicting-SAV/data/SAV Composite with max density and potential habitat.xlsx")

BaeStations <- BaeOverlap_StationZone %>% 
  #dplyr::filter(ZMZoneSAV_HA > 0) %>% #filter out non-Zostera: 25 total stations
  mutate(denscomp.max = (D4_Ha*.85) + (D3_Ha*.55) + (D2_Ha*.25) + (D1_Ha*.05)) %>% #calculate max composite area
  select(STATION, denscomp.max, SAV_HA) #clean up this DF

#load in SAV area by year for ALL stations
BaeDensityTime <- read_excel("~/Documents/R projects/Predicting-SAV/data/SAV Area by Year by Station Zone.xlsx")

#calculate SAV coverage in each station zone for each year and year - 1

####Station Baywide change over time####
#denspercomp only... use this for master code file
BaeDensTime <- BaeDensityTime %>%
 # filter(STATION %in% ZosteraStations$STATION) %>%
  #filter(between(Year, 1990, 2019)) %>% use this if you want to create a certain time dataset
  mutate(per.cov = case_when(Density == 1 ~ .05,
                             Density == 2 ~ .25, 
                             Density == 3 ~ .55, 
                             Density == 4 ~ .85)) %>% #convert to density weighted means 
  mutate(dens_cov = SAVAreaHa * per.cov) %>%
  group_by(STATION, Year) %>%
  summarize(dens.weight.mean = sum(dens_cov), #sum density weighted area
            SAVArea = sum(SAVAreaHa)) %>%
  mutate(dens.weight.mean.y1 = lag(dens.weight.mean, order_by = Year, k = 1), 
         SAVArea.y1 = lag(SAVArea, order_by = Year, k = 1)) %>%
  full_join(BaeStations) %>% group_by(STATION) %>% 
  mutate(dens.percomp = dens.weight.mean/denscomp.max, 
         dens.percomp.y1 = dens.weight.mean.y1/denscomp.max, 
         SAVArea.percomp = SAVArea/SAV_HA, 
         SAVArea.percomp.y1 = SAVArea.y1/SAV_HA) %>% 
  mutate(dens.percomp.change = (dens.percomp-dens.percomp.y1), 
         SAVArea.percomp.change = (SAVArea.percomp-SAVArea.percomp.y1)) %>% 
  rename("year" = "Year") %>% 
  select(STATION, year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, SAVArea.percomp.change, SAVArea, denscomp.max)

#2300 DWMs are 0
hist(BaeDensTime %>% filter(dens.weight.mean > 0) %>% pull(dens.weight.mean))

#filter out some of the Stations that are mostly 0s
#ZoDensTime_trim = ZoDensTime %>% 
#  filter(!STATION %in% c("CB8.1", "CB8.1E", "LE4.2", "EE3.3", "LE5.3"))

#merge it with the ENV data from tidyCBPWQ_2020
BaeDensWQ_combined <- CBPsimp.WQ_combined %>%
  filter(STATION %in% BaeStations$STATION) %>%
  full_join(BaeDensTime)

qplot(x = Sal.med, y = dens.percomp.change, data = BaeDensWQ_combined)


is.na(BaeDensWQ_combined) <- BaeDensWQ_combined == "Inf"
is.na(BaeDensWQ_combined) <- BaeDensWQ_combined == "-Inf"
BaeDensWQ_combined <- as.data.frame(BaeDensWQ_combined)

#Bae change and WQ in stations over time (2019)
#write_csv(BaeDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/BaeDensWQ_combined.csv")
#write_csv(BaeDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/BaeDensWQ_combined.csv")

qplot(x = dens.percomp.y1, y = dens.percomp, data = BaeDensWQ_combined)

#69 Vars Baewide Data Merge#####
#merge it with the 69vars ENV data from tidyCBPWQ_2020
CBP.WQ_69vars = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_69vars.csv")
BDWQ_69 <- CBP.WQ_69vars %>%
  filter(STATION %in% BaeStations$STATION) %>%
  full_join(BaeDensTime)

BDWQ_69[is.nan(BDWQ_69)] <- 0
is.na(BDWQ_69) <- BDWQ_69 == "Inf"
is.na(BDWQ_69) <- BDWQ_69 == "-Inf"
BDWQ_69 <- as.data.frame(BDWQ_69)

#Baywide change and Spring WQ in stations over time (2019)

#load in DF of cluster groups per station
#####NOTE: 12/20/2021 I noticed some of the MixedMeso are really ruppia and such (or partially?) Check this other file too that we made up for the sticky app ####
clusters_manual <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/Community by Station.csv")
clusters = read.csv("~/Documents/R projects/TraitsSAV/spp.prop.cluster.csv")

community.desig = clusters %>% select(STATION, LATITUDE, LONGITUDE, clust.group, clust.no) #now use these clust.group to filter data! 

#write_csv(community.desig, "~/Documents/R projects/Predicting-SAV/data/community.desig.csv")

BaeDensWQ_69 = full_join(community.desig, BDWQ_69)

#write_csv(BaeDensWQ_69, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/BaeDensWQ_69.csv")
write_csv(BaeDensWQ_69, "~/Documents/R projects/Predicting-SAV/data/BaeDensWQ_69.csv")


#
##
###
#####Community 3: Mixed mesohaline #####
###
##
#

#BaeDensWQ_combined = read.csv("~/Documents/R projects/Predicting-SAV/data/BaeDensWQ_combined.csv")
MixMesoDensWQ_combined = BaeDensWQ_combined %>% 
  full_join(community.desig) %>%
  filter(clust.group == "MixedMeso") %>% select(-clust.group)

length(unique(MixMesoDensWQ_combined$STATION)) #24 total stations in Mixed Meso

#Mixed Mesohaline change and ALL vars WQ in stations over time (2019)
#write_csv(MixMesoDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/MixMesoDensWQ_combined.csv")
#write_csv(MixMesoDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/MixMesoDensWQ_combined.csv")

#BaeDensWQ_69 = read.csv("~/Documents/R projects/Predicting-SAV/data/BaeDensWQ_69.csv")
MixMesoDensWQ_69 = BaeDensWQ_69 %>% 
  #full_join(community.desig) %>%
  filter(clust.group == "MixedMeso") %>% select(-clust.group)

length(unique(MixMesoDensWQ_69$STATION)) #24 total stations in Mixed Meso

#Mixed Mesohaline change and 69 WQ in stations over time (2019)
#write_csv(MixMesoDensWQ_69, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/MixMesoDensWQ_69.csv")
write_csv(MixMesoDensWQ_69, "~/Documents/R projects/Predicting-SAV/data/MixMesoDensWQ_69.csv")


#
##
###
#####Community 4: Oligohaline/Tidal Fresh #####
###
##
#
#BaeDensWQ_combined = read.csv("~/Documents/R projects/Predicting-SAV/data/BaeDensWQ_combined.csv")
FreshDensWQ_combined = BaeDensWQ_combined %>% 
  full_join(community.desig) %>%
  filter(clust.group == "Fresh") %>% select(-clust.group)

length(unique(FreshDensWQ_combined$STATION)) #45 total stations in Fresh

# Fresh change and 69 vars WQ in stations over time (2019)
#write_csv(FreshDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/FreshDensWQ_combined.csv")
#write_csv(FreshDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/FreshDensWQ_combined.csv")

FreshDensWQ_69 = BaeDensWQ_69 %>% 
 # full_join(community.desig) %>%
  filter(clust.group == "Fresh") %>% select(-clust.group)

length(unique(FreshDensWQ_69$STATION)) #45 total stations in Fresh

#Mixed Mesohaline change and 69 WQ in stations over time (2019)
#write_csv(FreshDensWQ_69, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/FreshDensWQ_69.csv")
write_csv(FreshDensWQ_69, "~/Documents/R projects/Predicting-SAV/data/FreshDensWQ_69.csv")

#BaeDensWQ_69 = read.csv("~/Documents/R projects/Predicting-SAV/data/BaeDensWQ_69.csv")

###Clusters per Station####
#Change RuZo to Ruppia or Zostera
RuSTA = unique(RuppiaStations$STATION) 
ZoSTA = unique(ZosteraStations$STATION)

Rsta = tibble(STATION = c(RuSTA), 
               clust.group = "Ruppia")
Zsta = tibble(STATION = c(ZoSTA), 
              clust.group = "Zostera")
RZsta = bind_rows(Rsta, Zsta)

mmfSTA = full_join(community.desig, RZsta, by = "STATION") %>%
  rename(RuZo_JJ = clust.group.y) %>% select(-clust.no) 

write_csv(mmfSTA, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/Community By Station.csv")


