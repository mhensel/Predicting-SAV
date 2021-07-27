#Code to build the SAV change and water quality over time dataframe for each of the 4 communities
library(tidyverse); library(readxl)

#load in the Water Quality data
CBP.WQ_combined <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_combined.csv")
CBP.WQ_combined = read.csv("~/Documents/R projects/Predicting-SAV/data/CBP.WQ_combined.csv")

#Community 1: Ruppia maritima monoculture####
#load in data to make SAV density over time per zone DF
RuppiaOverlap_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Ruppia SAV Zones Overlap with Station Zones.xlsx")

RuppiaStations <- RuppiaOverlap_StationZone %>% 
  dplyr::filter(RMZoneSAV_HA > 0) %>% #filter out non-Ruppia: 51 total stations
  mutate(denscomp.max = (RMZoneD4_Ha*.85) + (RMZoneD3_Ha*.55) + (RMZoneD2_Ha*.25) + (RMZoneD1_Ha*.05)) %>% #calculate max composite area
  select(STATION, denscomp.max, RMZoneSAV_HA) #clean up this DF

#load in SAV area by year for Rupppia stations
RuppiaDensityTime <- read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Ruppia SAV Area by Year by Station Zone.xlsx")

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
  select(STATION, year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, SAVArea.percomp.change)

#merge it with the ENV data from tidyCBPWQ_2020
RuDensWQ_combined <- CBP.WQ_combined %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  full_join(RuDensTime)

RuDensWQ_combined[is.nan(RuDensWQ_combined)] <- 0
is.na(RuDensWQ_combined) <- RuDensWQ_combined == "Inf"
is.na(RuDensWQ_combined) <- RuDensWQ_combined == "-Inf"
RuDensWQ_combined <- as.data.frame(RuDensWQ_combined)

#Ruppia change and WQ in stations over time (2019)
write_csv(RuDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/RuDensWQ_combined.csv")
write_csv(RuDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/RuDensWQ_combined.csv")

#merge it with the Spring ENV data from tidyCBPWQ_2020
RuDensWQ_spme <- CBP.WQ_spme %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  full_join(RuDensTime)

RuDensWQ_spme[is.nan(RuDensWQ_spme)] <- 0
is.na(RuDensWQ_spme) <- RuDensWQ_spme == "Inf"
is.na(RuDensWQ_spme) <- RuDensWQ_spme == "-Inf"
RuDensWQ_spme <- as.data.frame(RuDensWQ_spme)

#Ruppia change and Spring WQ in stations over time (2019)
write_csv(RuDensWQ_spme, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/RuDensWQ_spme.csv")
write_csv(RuDensWQ_spme, "~/Documents/R projects/Predicting-SAV/data/RuDensWQ_spme.csv")

#may want to filter
#RmZone_Env9010.tss <- RmZone_Env.tss %>%
  filter(!dens.percomp.y1 > 0.90) %>% filter(!dens.percomp.y1 < 0.10)

#Community 2: Zostera Monoculture####

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
  summarize(dens.weight.mean = sum(dens_cov), 
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
  select(STATION, year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, SAVArea.percomp.change)

#merge it with the ENV data from tidyCBPWQ_2020
ZoDensWQ_combined <- CBP.WQ_combined %>%
  filter(STATION %in% ZosteraStations$STATION) %>%
  full_join(ZoDensTime)

ZoDensWQ_combined[is.nan(ZoDensWQ_combined)] <- 0
is.na(ZoDensWQ_combined) <- ZoDensWQ_combined == "Inf"
is.na(ZoDensWQ_combined) <- ZoDensWQ_combined == "-Inf"
ZoDensWQ_combined <- as.data.frame(ZoDensWQ_combined)

#Ruppia change and WQ in stations over time (2019)
write_csv(ZoDensWQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/ZoDensWQ_combined.csv")
write_csv(ZoDensWQ_combined, "~/Documents/R projects/Predicting-SAV/data/ZoDensWQ_combined.csv")

#merge it with the Spring ENV data from tidyCBPWQ_2020
ZoDensWQ_spme <- CBP.WQ_spme %>%
  filter(STATION %in% ZosteraStations$STATION) %>%
  full_join(ZoDensTime)

ZoDensWQ_spme[is.nan(ZoDensWQ_spme)] <- 0
is.na(ZoDensWQ_spme) <- ZoDensWQ_spme == "Inf"
is.na(ZoDensWQ_spme) <- ZoDensWQ_spme == "-Inf"
ZoDensWQ_spme <- as.data.frame(ZoDensWQ_spme)

#Ruppia change and Spring WQ in stations over time (2019)
write_csv(ZoDensWQ_spme, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/ZoDensWQ_spme.csv")
write_csv(ZoDensWQ_spme, "~/Documents/R projects/Predicting-SAV/data/ZoDensWQ_spme.csv")

#may want to filter
#RmZone_Env9010.tss <- RmZone_Env.tss %>%
#filter(!dens.percomp.y1 > 0.90) %>% filter(!dens.percomp.y1 < 0.10)

#Community 3: Mixed mesohaline #####

#Community 4: Oligohaline/Tidal Fresh ####

