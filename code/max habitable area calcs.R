

clusters = read.csv("~/Documents/R projects/TraitsSAV/spp.prop.cluster.csv")
clusters_manual <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/Community by Station.csv")



BaeOverlap_StationZone <- read_excel("~/Documents/R projects/Predicting-SAV/data/SAV Composite with max density and potential habitat.xlsx")


station.info = clusters_manual %>% #select(STATION, LATITUDE, LONGITUDE, clust.group) %>%
  full_join(BaeOverlap_StationZone) %>%
  mutate(denscomp.max = (D4_Ha*.85) + (D3_Ha*.55) + (D2_Ha*.25) + (D1_Ha*.05)) %>%
  mutate(SLR.mmyr = case_when(clust.group == "Fresh" ~  3.78, #SLRs for each group
                              clust.group == "MixedMeso" ~ 3.44, 
                              clust.group == "Ruppia" ~ 4.78, 
                              clust.group == "Zostera" ~ 3.6)) %>%
  mutate(ACC.mmyr = case_when(clust.group == "Fresh" ~  5.5, #Accretion for each group
                              clust.group == "MixedMeso" ~ 9.2, 
                              clust.group == "Ruppia" ~ 5.2, 
                              clust.group == "Zostera" ~ 6)) %>%
  drop_na(clust.group)

write.csv(station.info, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Station Accretion and SLR with Max Composite Area.csv")


#add in the years and add up the SLR and ACC

#another thing i could do is get the SLR by latitude and have dave get the stations that are the closest? 



future = seq(from = 2019, to = 2060, by = 1)
fut.years = rep(future, length(unique(station.info$STATION)))

station.potarea = station.info[rep(seq_len(nrow(station.info)), each = length(future)), ]

future_maxdenscomp.df = station.info %>% 
  group_by(STATION) %>%
  mutate(Year = future)

station.potarea


