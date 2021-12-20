

clusters = read.csv("~/Documents/R projects/TraitsSAV/spp.prop.cluster.csv")


future = seq(from = 2020, to = 2060, by = 1)
fut.years = rep(future, length(unique(clusters$STATION)))


BaeOverlap_StationZone <- read_excel("~/Documents/R projects/Predicting-SAV/data/SAV Composite with max density and potential habitat.xlsx")

BaeStations <- BaeOverlap_StationZone %>% 
  #dplyr::filter(ZMZoneSAV_HA > 0) %>% #filter out non-Zostera: 25 total stations
  mutate(denscomp.max = (D4_Ha*.85) + (D3_Ha*.55) + (D2_Ha*.25) + (D1_Ha*.05)) %>% #calculate max composite area
  select(STATION, denscomp.max, SAV_HA)

