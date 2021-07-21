library(tidyverse)

#load in env data (can also load from mirrored R projects file on hard drive)
CBP_WQ = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBPWQall_2019.csv")

#load in data to make SAV density over time per zone DF






RuppiaOverlap_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Ruppia/Ruppia areas in Chesapeake Bay/Ruppia SAV Zones Overlap with Station Zones.xlsx")

#Use this to filter out the station zones that arent in the RM_Zone and calculate the maximum composite area in any given year (denscomp.max). This gives max area of each density class in each station
RuppiaStations <- RuppiaOverlap_StationZone %>% 
  dplyr::filter(RMZoneSAV_HA > 0) %>%
  mutate(denscomp.max = (RMZoneD4_Ha*.85) + (RMZoneD3_Ha*.55) + (RMZoneD2_Ha*.25) + (RMZoneD1_Ha*.05)) %>%
  select(STATION, denscomp.max, RMZoneSAV_HA)

#Ruppia coverage in each Station zone per year is "SAVAreaHa". with density class too
Ruppiayear_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Ruppia/Ruppia areas in Chesapeake Bay/Ruppia SAV Area by Year by Station Zone.xlsx")

#calculate Ruppia coverage in each station zone for each year and year - 1
#Ruppia response variables created here: 
#dens.weight.mean = total density-weighted-mean area in zone, SAVArea = total area of SAV in zone, dens.change = change in dens weight mean area from y1, SAVArea.change,  dens.prop.change = scaled density weighted area change, SAVArea.prop.change = scaled area change
#dens.permax.change density weighted means but scaled from the maximum extent of the zone. UPDATE 11/10: now dens.percomp.change and related dens.percomp are dens weighted means, scaled from the maxiumum composite extent in each Station zone. 

####Station Ruppia change over time####
#denspercomp only... use this for master code file
RmZoneStations_denspercomp <- SAVyear_StationZone %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  #filter(between(Year, 1990, 2019)) %>% use this if you want to create a certain time dataset
  mutate(per.cov = case_when(Density == 1 ~ .05,
                             Density == 2 ~ .25, 
                             Density == 3 ~ .55, 
                             Density == 4 ~ .85)) %>% #convert to density weighted means 
  mutate(dens_cov = SAVAreaHa * per.cov) %>%
  group_by(STATION, Year) %>%
  summarize(dens.weight.mean = sum(dens_cov), SAVArea = sum(SAVAreaHa)) %>%
  mutate(dens.weight.mean.y1 = lag(dens.weight.mean, order_by = Year, k = 1), SAVArea.y1 = lag(SAVArea, order_by = Year, k = 1)) %>%
  full_join(RuppiaStations) %>% group_by(STATION) %>% 
  mutate(dens.percomp = dens.weight.mean/denscomp.max, dens.percomp.y1 = dens.weight.mean.y1/denscomp.max, SAVArea.percomp = SAVArea/RMZoneSAV_HA, SAVArea.percomp.y1 = SAVArea.y1/RMZoneSAV_HA) %>% 
  mutate(dens.percomp.change = (dens.percomp-dens.percomp.y1), SAVArea.percomp.change = (SAVArea.percomp-SAVArea.percomp.y1)) %>% 
  rename("year" = "Year") %>% select(STATION, year, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, dens.percomp.change, SAVArea.percomp.change)

#if you want to rewrite, go for it.
write_csv(RmZoneStations_denspercomp, "/Volumes/savshare2/Current Projects/Ruppia/Data/RmSEM datasets/RmZoneStations_denspercomp.csv")



#repeat for Zostera

ZosteraOverlap_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Ruppia/Ruppia areas in Chesapeake Bay/Ruppia SAV Zones Overlap with Station Zones.xlsx")

ZosteraStations <- ZosteraOverlap_StationZone %>% 
  dplyr::filter(??????ZoneSAV_HA > 0) %>%
  mutate(denscomp.max = (RMZoneD4_Ha*.85) + (RMZoneD3_Ha*.55) + (RMZoneD2_Ha*.25) + (RMZoneD1_Ha*.05)) %>%
  select(STATION, denscomp.max, RMZoneSAV_HA)

Ruppiayear_StationZone <- read_excel("/Volumes/savshare2/Current Projects/Ruppia/Ruppia areas in Chesapeake Bay/Ruppia SAV Area by Year by Station Zone.xlsx")

####Station Ruppia change over time####
#denspercomp only... use this for master code file
RmZoneStations_denspercomp <- SAVyear_StationZone %>%
  filter(STATION %in% RuppiaStations$STATION) %>%
  #filter(between(Year, 1990, 2019)) %>% use this if you want to create a certain time dataset
  mutate(per.cov = case_when(Density == 1 ~ .05,
                             Density == 2 ~ .25, 
                             Density == 3 ~ .55, 
                             Density == 4 ~ .85)) %>% #convert to density weighted means 
  mutate(dens_cov = SAVAreaHa * per.cov) %>%
  group_by(STATION, Year) %>%
  summarize(dens.weight.mean = sum(dens_cov), SAVArea = sum(SAVAreaHa)) %>%
  mutate(dens.weight.mean.y1 = lag(dens.weight.mean, order_by = Year, k = 1), SAVArea.y1 = lag(SAVArea, order_by = Year, k = 1)) %>%
  full_join(RuppiaStations) %>% group_by(STATION) %>% 
  mutate(dens.percomp = dens.weight.mean/denscomp.max, dens.percomp.y1 = dens.weight.mean.y1/denscomp.max, SAVArea.percomp = SAVArea/RMZoneSAV_HA, SAVArea.percomp.y1 = SAVArea.y1/RMZoneSAV_HA) %>% 
  mutate(dens.percomp.change = (dens.percomp-dens.percomp.y1), SAVArea.percomp.change = (SAVArea.percomp-SAVArea.percomp.y1)) %>% 
  rename("year" = "Year") %>% select(STATION, year, dens.weight.mean, dens.weight.mean.y1, dens.percomp, dens.percomp.y1, dens.percomp.change, SAVArea.percomp.change)

#if you want to rewrite, go for it.
write_csv(RmZoneStations_denspercomp, "/Volumes/savshare2/Current Projects/Ruppia/Data/RmSEM datasets/RmZoneStations_denspercomp.csv")



