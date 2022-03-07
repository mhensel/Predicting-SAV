library(tidyverse); library(ggsci); library(ggthemes); library(patchwork); library(viridis)

#scale_color_aaas() from ggsci
zo.col = "darkolivegreen3"
ru.col = "chocolate4"
mm.col = "darkorchid4"
f.col = "aquamarine1"
#Figure building and non-SEM viz fpr predicting SAV
#MULTIVERSAL FUTURE DFs#####

Zo_CC.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/Zo_CC.wland_Predict.csv") 
Ru_CC.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/Ru_CC.wland_Predict.csv")
MM_CC.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/MM_CC.wland_Predict.csv")
F_CC.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/F_CC.wland_Predict.csv")

Zo_WIP.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/Zo_WIP.wland_Predict.csv")
Ru_WIP.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/Ru_WIP.wland_Predict.csv")
MM_WIP.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/MM_WIP.wland_Predict.csv")
F_WIP.wland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/F_WIP.wland_Predict.csv")

Zo_WIP.woland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/Zo_WIP.woland_Predict.csv")
Ru_WIP.woland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/Ru_WIP.woland_Predict.csv")
MM_WIP.woland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/MM_WIP.woland_Predict.csv")
F_WIP.woland_Predict = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/F_WIP.woland_Predict.csv")

##Past Data WQ and SAV####
SAVCommDensWQ_69 = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69.csv")

#We deal with 0s by eliminating any years where the 3 previous years were 0s, with this antijoin code
SAVCommZeros = SAVCommDensWQ_69 %>% mutate(dens.weight.mean.y2 = lag(dens.weight.mean.y1))  %>%
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0 & dens.weight.mean.y2 == 0) 
#anti join the 0s to get a No0 df

SAVCommDensWQ_69sem.No0 = anti_join(SAVCommDensWQ_69, SAVCommZeros) %>% #anti the 0s
  select(STATION, year, SpCluster, dens.weight.mean, dens.weight.mean.y1, dens.percomp.y1, dens.percomp, dens.percomp.change, denscomp.max, Temp.sumy1med, Temp.sumy1me, Sal.summax, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summed, Temp.summax, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Sal.summed, Secc.summe, Secc.spme, TP.spmed, TP.spme, TSS.summe, TP.summe, TP.summax, TN.spme, TN.spmed, TN.summe)

#Past data by Community####
ZoDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Zostera") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

RuDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Ruppia") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Temp.spme, Chla.spme, Sal.spme, Secc.spme, TP.spme, TN.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

MMDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "MixedMeso") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  drop_na() %>% #161
  as.data.frame()

FDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Fresh") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TP.summax, TN.summe, Sal.summe, TSS.summe) %>%
  drop_na() %>% #161
  mutate(Sal.summe = Sal.summe + .1) %>%
  as.data.frame()


SAVCommunityDens_AllStations =read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommunityDens_AllStations.csv")

#back-calc HA acreage: 
dwm.to.HA_Zo = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% 
                    filter(SpCluster == "Zostera"))
dwm.to.HA_Ru = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% 
                    filter(SpCluster == "Ruppia"))
dwm.to.HA_MM = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% 
                    filter(SpCluster == "MixedMeso"))
dwm.to.HA_F = lm(SAVArea ~ dens.weight.mean, data = SAVCommunityDens_AllStations %>% 
                   filter(SpCluster == "Fresh"))

zarea = Zo_CC.wland_Predict %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .))
###Compare scenarios for each Community####

#Zostera, Scenario by scenario####
Zo_CC.wland_Predict = Zo_CC.wland_Predict %>% filter(!year == 2020)
Zo_WIP.wland_Predict = Zo_WIP.wland_Predict %>% filter(!year == 2020)
Zo_WIP.woland_Predict = Zo_WIP.woland_Predict %>% filter(!year == 2020) 
ZoDensWQsem.No0_NEW

#how to calc total grass in a zone (across all stations)
View(Zo_WIP.woland_Predict %>% group_by(simnum, year) %>% 
  summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)))
#Zostera all scens####
Zo_allScenarios = 
ggplot(data = Zo_CC.wland_Predict) + 
  stat_smooth(data = Zo_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Zo), method = "gam", 
              size = 2, color = "coral", fill = "coral1") +
  geom_line(data = Zo_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Zo, group = simnum), method = "lm", 
            size = .1, alpha = .2, color = "coral1") +
  stat_smooth(data = Zo_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Zo), method = "gam", 
              size = 2, color = "cyan3", fill = "cyan1") +
  geom_line(data = Zo_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Zo, group = simnum), method = "lm", 
            size = .1, alpha = .2, color = "cyan3") +
stat_smooth(data = Zo_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Zo), method = "gam", 
            size = 2, color = "darkslateblue", fill = "darkslateblue") +
 geom_line(data = Zo_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Zo, group = simnum), method = "lm", 
              size = .1, alpha = .2, color = "darkslateblue") +
stat_smooth(data = ZoDensWQsem.No0_NEW %>% group_by(year) %>% 
              summarize(tot.Zo = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Zo), method = "gam", 
            size = 2, color = "chartreuse2", fill = "chartreuse1") +
  geom_text(aes(x = 2030, y = 8000, label = "Nutrient reduction + Land Use Management"), stat = "unique", size = 8, color = "cyan3") +
  geom_text(aes(x = 2030, y = 7420, label = "Nutrient Reduction"), stat = "unique", size = 8, color = "darkslateblue") +
  geom_text(aes(x = 2030, y = 6900, label = "No Action"), stat = "unique", size = 8, color = "coral") +
  theme_bw(base_size=26) + 
  labs(y = expression(paste("Baywide eelgrass area \n (density-weighted HA)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Zo_allScenarios

#ENV ZO Zone####
#TEMP ZO ZONE####
Temp.Zo_allScenarios = #2020 data (i.e. 2021 sumy1med data) is too high bc of sampling
  ggplot(data = Zo_CC.wland_Predict %>% filter(!year %in% c(2020, 2021))) + 
  geom_line(data = Zo_CC.wland_Predict %>% filter(!year %in% c(2020, 2021)) %>% group_by(simnum, year) %>% 
              summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
            aes(x = year, y = Temp, group = simnum), alpha = .3, color = "coral1") +
  geom_line(data = Zo_WIP.woland_Predict %>% filter(!year %in% c(2020, 2021)) %>% group_by(simnum, year) %>% 
              summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
            aes(x = year, y = Temp, group = simnum), alpha = .3, color = "darkslateblue") + 
  geom_line(data = Zo_WIP.wland_Predict %>% filter(!year %in% c(2020, 2021)) %>% group_by(simnum, year) %>% 
              summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
            aes(x = year, y = Temp, group = simnum), alpha = .3, color = "cyan") + 
  stat_smooth(data = Zo_WIP.wland_Predict %>% filter(!year %in% c(2020, 2021)) %>% group_by(simnum, year) %>% 
                summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
              aes(x = year, y = Temp), method = "gam", 
              size = 3, color = "black", fill = "cyan1") +
  
  stat_smooth(data = Zo_WIP.woland_Predict %>% filter(!year %in% c(2020, 2021)) %>% group_by(simnum, year) %>% 
                summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
              aes(x = year, y = Temp), method = "gam", 
              size = 3, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = Zo_CC.wland_Predict %>% filter(!year %in% c(2020, 2021)) %>% group_by(simnum, year) %>% 
                summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
              aes(x = year, y = Temp), method = "gam", 
              size = 3, color = "coral", fill = "coral1") +
  stat_smooth(data = ZoDensWQsem.No0_NEW %>% group_by(year) %>% 
                summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
              aes(x = year, y = Temp), method = "loess", 
              size = 3, color = "brown2", fill = "brown2") +
  geom_text(aes(x = 2030, y = 25, label = "Nutrient Reduction"), stat = "unique", size = 8, color = "cyan3") +
  geom_text(aes(x = 2030, y = 24.75, label = "Nutrient Reduction & Land Use Management"), stat = "unique", size = 8, color = "darkslateblue") +
  geom_text(aes(x = 2030, y = 24.5, label = "No Action"), stat = "unique", size = 8, color = "coral") +
  theme_bw(base_size=30) + 
  labs(y = expression(paste("mean summer Temperature (", degree ~ C, ")")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Temp.Zo_allScenarios

Secc.Zo_allScenarios = #2020 data (i.e. 2021 sumy1med data) is too high bc of sampling
  ggplot(data = Zo_CC.wland_Predict ) + 
  stat_smooth(data = Zo_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Secc = mean(Secc.summe, na.rm = T)), 
              aes(x = year, y = Secc), method = "loess", 
              size = 1.5, color = "coral", fill = "coral1") +
  stat_smooth(data = Zo_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Secc = mean(Secc.summe, na.rm = T) + .01), 
              aes(x = year, y = Secc), method = "loess", 
              size = 1.5, color = "cyan3", fill = "cyan1") +
  stat_smooth(data = Zo_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(Secc = mean(Secc.summe, na.rm = T)), 
              aes(x = year, y = Secc), method = "loess", 
              size = 1.5, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = ZoDensWQsem.No0_NEW %>% group_by(year) %>% 
                summarize(Secc = mean(Secc.summe, na.rm = T)), 
              aes(x = year, y = Secc), method = "loess", 
              size = 1.5, color = "darkslategrey", fill = "darkslategrey") +
  theme_bw(base_size=20) + 
  labs(y = "mean summer Secchi depth", x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Secc.Zo_allScenarios

Chla.Zo_allScenarios = #2020 data (i.e. 2021 sumy1med data) is too high bc of sampling
  ggplot(data = Zo_CC.wland_Predict ) + 
  stat_smooth(data = Zo_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Chla = mean(Chla.spme, na.rm = T)), 
              aes(x = year, y = Chla), method = "gam", 
              size = 1.5, color = "coral", fill = "coral1") +
  stat_smooth(data = Zo_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Chla = mean(Chla.spme, na.rm = T)), 
              aes(x = year, y = Chla), method = "gam", 
              size = 1.5, color = "cyan3", fill = "cyan1") +
  stat_smooth(data = Zo_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(Chla = mean(Chla.spme, na.rm = T) + 0.2), 
              aes(x = year, y = Chla), method = "gam", 
              size = 1.5, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = ZoDensWQsem.No0_NEW %>% group_by(year) %>% 
                summarize(Chla = mean(Chla.spme, na.rm = T)), 
              aes(x = year, y = Chla), method = "loess", 
              size = 1.5, color = "darkslategrey", fill = "darkslategrey") +
  theme_bw(base_size=20) + 
  labs(y = "mean spring Chla", x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Chla.Zo_allScenarios


#Ruppia all scenarios####
Ru_CC.wland_Predict = Ru_CC.wland_Predict %>% filter(!year == 2020)
Ru_WIP.wland_Predict = Ru_WIP.wland_Predict %>% filter(!year == 2020)
Ru_WIP.woland_Predict = Ru_WIP.woland_Predict %>% filter(!year == 2020) 
RuDensWQsem.No0_NEW


#how to calc total grass in a zone (across all stations)
View(Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
       summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)))

Ru_allScenarios = 
  ggplot(data = Ru_CC.wland_Predict) + 
  stat_smooth(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "coral", fill = "coral1") +
  geom_line(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "coral") +
  stat_smooth(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "cyan3", fill = "cyan2") +
  geom_line(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "cyan3") +
  stat_smooth(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "darkslateblue", fill = "darkslateblue") +
  geom_line(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "darkslateblue") +
  stat_smooth(data = RuDensWQsem.No0_NEW %>% group_by(year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "chocolate4", fill = "chocolate4") +
  geom_text(aes(x = 2030, y = 1400, label = "Nutrient reduction + Land Use Management"), stat = "unique", size = 8, color = "cyan3") +
  geom_text(aes(x = 2030, y = 720, label = "Nutrient Reduction"), stat = "unique", size = 8, color = "darkslateblue") +
  geom_text(aes(x = 2030, y = 220, label = "No Action"), stat = "unique", size = 8, color = "coral") +
  theme_bw(base_size=28) + 
  labs(y = expression(paste("Baywide Widgeongrass area \n (density-weighted HA)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 2, .25, 2), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Ru_allScenarios

#RuZone ENV Graphs####
Sal.Ru_allScenarios = 
  ggplot(data = Ru_CC.wland_Predict) + 
  geom_line(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(Sal = mean(Sal.spme)), 
            aes(x = year, y = Sal, group = simnum), alpha = .3, color = "coral1") +
  geom_line(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(Sal = mean(Sal.spme)), 
            aes(x = year, y = Sal, group = simnum), alpha = .3, color = "cyan") + 
  geom_line(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(Sal = mean(Sal.spme)), 
            aes(x = year, y = Sal, group = simnum), alpha = .3, color = "darkslateblue") + 
  stat_smooth(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Sal = mean(Sal.spme)), 
              aes(x = year, y = Sal), method = "gam", 
              size = 4, color = "coral", fill = "coral1") +
  stat_smooth(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Sal = mean(Sal.spme)), 
              aes(x = year, y = Sal), method = "gam", 
              size = 4, color = "cyan", fill = "cyan1") +
  stat_smooth(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(Sal = mean(Sal.spme)), 
              aes(x = year, y = Sal), method = "gam", 
              size = 4, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = RuDensWQsem.No0_NEW %>% filter(!year == 2020) %>% group_by(year) %>% 
                summarize(Sal = mean(Sal.spme)), 
              aes(x = year, y = Sal), method = "loess", 
              size = 4, color = "burlywood", fill = "burlywood1") +
  theme_bw(base_size=24) + 
  labs(y = "mean spring Sal", x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Sal.Ru_allScenarios


Chla.Ru_allScenarios = 
  ggplot(data = Ru_CC.wland_Predict) + 
  geom_line(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(Chla = mean(Chla.spme)), 
            aes(x = year, y = Chla, group = simnum), alpha = .3, color = "coral1") +
  geom_line(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(Chla = mean(Chla.spme)), 
            aes(x = year, y = Chla, group = simnum), alpha = .3, color = "cyan") + 
  geom_line(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(Chla = mean(Chla.spme)), 
            aes(x = year, y = Chla, group = simnum), alpha = .3, color = "darkslateblue") + 
  stat_smooth(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Chla = mean(Chla.spme)), 
              aes(x = year, y = Chla), method = "gam", 
              size = 4, color = "coral", fill = "coral1") +
  stat_smooth(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(Chla = mean(Chla.spme)), 
              aes(x = year, y = Chla), method = "gam", 
              size = 4, color = "cyan", fill = "cyan") +
  stat_smooth(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(Chla = mean(Chla.spme)), 
              aes(x = year, y = Chla), method = "gam", 
              size = 4, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = RuDensWQsem.No0_NEW %>% filter(!year == 2020) %>% group_by(year) %>% 
                summarize(Chla = mean(Chla.spme)), 
              aes(x = year, y = Chla), method = "loess", 
              size = 4, color = "burlywood", fill = "burlywood1") +
  theme_bw(base_size=24) + 
  labs(y = "mean spring Chla", x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Chla.Ru_allScenarios

TN.Ru_allScenarios = 
  ggplot(data = Ru_CC.wland_Predict) + 
  geom_line(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(TN = mean(TN.spme)), 
            aes(x = year, y = TN, group = simnum), alpha = .3, color = "coral1") +
  geom_line(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(TN = mean(TN.spme)), 
            aes(x = year, y = TN, group = simnum), alpha = .3, color = "cyan") + 
  geom_line(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(TN = mean(TN.spme)), 
            aes(x = year, y = TN, group = simnum), alpha = .3, color = "darkslateblue") + 
  stat_smooth(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TN.spme)), 
              aes(x = year, y = TN), method = "gam", 
              size = 4, color = "coral", fill = "coral1") +
  stat_smooth(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TN.spme)), 
              aes(x = year, y = TN), method = "gam", 
              size = 4, color = "cyan", fill = "cyan") +
  stat_smooth(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TN.spme)), 
              aes(x = year, y = TN), method = "gam", 
              size = 4, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = RuDensWQsem.No0_NEW %>% filter(!year == 2020) %>% group_by(year) %>% 
                summarize(TN = mean(TN.spme)), 
              aes(x = year, y = TN), method = "loess", 
              size = 4, color = "burlywood", fill = "burlywood1") +
  theme_bw(base_size=24) + 
  labs(y = "mean spring TN", x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
TN.Ru_allScenarios

TP.Ru_allScenarios = 
  ggplot(data = Ru_CC.wland_Predict) + 
  geom_line(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(TN = mean(TP.spme)), 
            aes(x = year, y = TN, group = simnum), alpha = .3, color = "coral1") +
  geom_line(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(TN = mean(TP.spme)), 
            aes(x = year, y = TN, group = simnum), alpha = .3, color = "cyan") + 
  geom_line(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(TN = mean(TP.spme)), 
            aes(x = year, y = TN, group = simnum), alpha = .3, color = "darkslateblue") + 
  stat_smooth(data = Ru_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TP.spme)), 
              aes(x = year, y = TN), method = "gam", 
              size = 4, color = "coral", fill = "coral1") +
  stat_smooth(data = Ru_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TP.spme)), 
              aes(x = year, y = TN), method = "gam", 
              size = 4, color = "cyan", fill = "cyan") +
  stat_smooth(data = Ru_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TP.spme)), 
              aes(x = year, y = TN), method = "gam", 
              size = 4, color = "darkslateblue", fill = "darkslateblue") +
  stat_smooth(data = RuDensWQsem.No0_NEW %>% filter(!year == 2020) %>% group_by(year) %>% 
                summarize(TN = mean(TP.spme)), 
              aes(x = year, y = TN), method = "loess", 
              size = 4, color = "burlywood", fill = "burlywood1") +
  theme_bw(base_size=24) + 
  labs(y = "mean spring TP", x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
TP.Ru_allScenarios


#Mixed Mesohaline All Scens####
MM_CC.wland_Predict = MM_CC.wland_Predict %>% filter(!year == 2020)
MM_WIP.wland_Predict = MM_WIP.wland_Predict %>% filter(!year == 2020)
MM_WIP.woland_Predict = MM_WIP.woland_Predict %>% filter(!year == 2020) 

#how to calc total grass in a zone (across all stations)
View(MM_CC.wland_Predict %>% group_by(simnum, year) %>% 
       summarize(tot.MM = sum(dens.weight.mean, na.rm = T)))

View(MMDensWQsem.No0_NEW %>% group_by(year) %>% 
  summarize(tot.MM = sum(dens.weight.mean, na.rm = T)))

MM_allScenarios = 
  ggplot(data = MM_CC.wland_Predict) + 
  stat_smooth(data = MM_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "coral", fill = "coral1") +
  geom_line(data = MM_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "coral") +
  stat_smooth(data = MM_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "cyan3", fill = "cyan2") +
  geom_line(data = MM_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "cyan3") +
  stat_smooth(data = MM_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "darkslateblue", fill = "darkslateblue") +
  geom_line(data = MM_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "darkslateblue") +
  stat_smooth(data = MMDensWQsem.No0_NEW %>% group_by(year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "loess", 
              size = 3, color = "darkorchid", fill = "darkorchid") +
  geom_text(aes(x = 2030, y = 800, label = "Nutrient reduction + Land Use Management"), stat = "unique", size = 8, color = "cyan3") +
  geom_text(aes(x = 2030, y = 750, label = "Nutrient Reduction"), stat = "unique", size = 8, color = "darkslateblue") +
  geom_text(aes(x = 2030, y = 700, label = "No Action"), stat = "unique", size = 8, color = "coral") +
  theme_bw(base_size=28) + 
  labs(y = expression(paste("Baywide mixed meso area \n (density-weighted HA)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 2, .25, 2), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
MM_allScenarios


#Fresh all scenarios ####
F_CC.wland_Predict = F_CC.wland_Predict %>% filter(!year %in% c(2020, 2021))
F_WIP.wland_Predict = F_WIP.wland_Predict %>% filter(!year %in% c(2020, 2021))
F_WIP.woland_Predict = F_WIP.woland_Predict %>% filter(!year %in% c(2020, 2021)) 
  FDensWQsem.No0_NEW
#%>% filter(!STATION %in% c("CB1.1", "TF3.1E"))
  
  #how to calc total grass in a zone (across all stations)
View(F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
         summarize(tot.F = sum(dens.weight.mean, na.rm = T)))
  
F_allScenarios = 
  ggplot(data = F_CC.wland_Predict) + 
  stat_smooth(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "coral", fill = "coral1") +
  geom_line(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "coral") +
  stat_smooth(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "cyan3", fill = "cyan2") +
  geom_line(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "cyan3") +
  stat_smooth(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "gam", 
              size = 3, color = "darkslateblue", fill = "darkslateblue") +
  geom_line(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
              summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
            aes(x = year, y = tot.Ru, group = simnum), 
            size = .1, alpha = .4, color = "darkslateblue") +
  stat_smooth(data = FDensWQsem.No0_NEW %>% group_by(year) %>% 
                summarize(tot.Ru = sum(dens.weight.mean, na.rm = T)), 
              aes(x = year, y = tot.Ru), method = "loess", 
              size = 3, color = "aquamarine1", fill = "aquamarine1") +
  geom_text(aes(x = 2030, y = 4200, label = "Nutrient reduction + Land Use Management"), stat = "unique", size = 8, color = "cyan3") +
  geom_text(aes(x = 2030, y = 3500, label = "Nutrient Reduction"), stat = "unique", size = 8, color = "darkslateblue") +
  geom_text(aes(x = 2030, y = 2800, label = "No Action"), stat = "unique", size = 8, color = "coral") +
  theme_bw(base_size=28) + 
  labs(y = expression(paste("Baywide Fresh area \n (density-weighted HA)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 2, .25, 2), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
F_allScenarios
  
  ggplot(data = F_CC.wland_Predict ) + 
    stat_smooth(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "coral", fill = "coral1") +
    stat_smooth(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "cyan", fill = "cyan1") +
    stat_smooth(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "darkslateblue", fill = "darkslateblue") +
    stat_smooth(data = FDensWQsem.No0_NEW  %>% group_by(year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "chartreuse2", fill = "chartreuse1") +
    geom_text(aes(x = 2030, y = 2000, label = "Climate change + Nutrient reduction + land use change"), stat = "unique", size = 5, color = "cyan") +
    geom_text(aes(x = 2030, y = 1220, label = "Climate change + Nutrient reduction, no land use change"), stat = "unique", size = 5, color = "darkslateblue") +
    geom_text(aes(x = 2030, y = 600, label = "Climate change + land use change"), stat = "unique", size = 5, color = "coral") +
    theme_bw(base_size=20) + 
    labs(y = expression(paste("Baywide Fstera area \n (density-weighted HA)")), x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  
  F_CC.wland_Predict %>% filter(STATION %in% c("CB1.1")) %>% 
    group_by(simnum) %>% filter(dens.percomp > .85) %>% summarize(length(unique(simnum)))


  F_twosta = 
    ggplot(data = F_CC.wland_Predict %>% filter(STATION %in% c("CB1.1"))) + 
    geom_line(data = F_CC.wland_Predict %>% filter(STATION %in% c("CB1.1")), 
                aes(x = year, y = dens.weight.mean, group = simnum), 
                size = .5, alpha = .4, color = "coral") +
    geom_line(data = F_WIP.wland_Predict %>% filter(STATION %in% c("CB1.1")), 
              aes(x = year, y = dens.weight.mean, group = simnum), 
              size = .5, alpha = .4, color = "cyan1") +
    geom_line(data = F_WIP.woland_Predict %>% filter(STATION %in% c("CB1.1")), 
              aes(x = year, y = dens.weight.mean, group = simnum), 
              size = .5, alpha = .4, color = "darkslateblue") +
    stat_smooth(data = F_CC.wland_Predict %>% filter(STATION %in% c("CB1.1")) %>% group_by(simnum, year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "coral", fill = "coral1") +
    stat_smooth(data = F_WIP.wland_Predict %>% filter(STATION %in% c("CB1.1")) %>% group_by(simnum, year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "cyan", fill = "cyan1") +
    stat_smooth(data = F_WIP.woland_Predict %>% filter(STATION %in% c("CB1.1")) %>% group_by(simnum, year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "darkslateblue", fill = "darkslateblue") +
    stat_smooth(data = FDensWQsem.No0_NEW %>% filter(STATION %in% c("CB1.1")) %>% group_by(year) %>% 
                  summarize(tot.F = sum(dens.weight.mean, na.rm = T)), 
                aes(x = year, y = tot.F), method = "gam", 
                size = 1.5, color = "chartreuse2", fill = "chartreuse1") +
    theme_bw(base_size=20) + 
    labs(y = expression(paste("Baywide Freshwater grasses area \n CB1.1 (density-weighted HA)")), x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  F_twosta

  F_zoneTemp.filt = #problem w 2021 data. its too hot in RET5.1A, TF3.1E TF 4.4
    ggplot(data = F_CC.wland_Predict %>% filter(!STATION %in% c("CB1.1", "TF3.1E", "RET5.1A", "TF3.1E", "TF 4.4"))) + 
    stat_smooth(data = F_CC.wland_Predict %>% filter(!STATION %in% c("CB1.1", "TF3.1E", "RET5.1A", "TF3.1E", "TF 4.4")) %>% group_by(simnum, year) %>% 
                  summarize(Temp.F = mean(Temp.sumy1me, na.rm = T)), 
                aes(x = year, y = Temp.F), method = "gam", 
                size = 1.5, color = "coral", fill = "coral1") +
    stat_smooth(data = F_WIP.wland_Predict %>% filter(!STATION %in% c("CB1.1", "TF3.1E", "RET5.1A", "TF3.1E", "TF 4.4")) %>% group_by(simnum, year) %>% 
                  summarize(Temp.F = mean(Temp.sumy1me, na.rm = T)), 
                aes(x = year, y = Temp.F), method = "gam", 
                size = 1.5, color = "cyan", fill = "cyan1") +
    stat_smooth(data = F_WIP.woland_Predict %>% filter(!STATION %in% c("CB1.1", "TF3.1E", "RET5.1A", "TF3.1E", "TF 4.4")) %>% group_by(simnum, year) %>% 
                  summarize(Temp.F = mean(Temp.sumy1me, na.rm = T)), 
                aes(x = year, y = Temp.F), method = "gam", 
                size = 1.5, color = "darkslateblue", fill = "darkslateblue") +
    stat_smooth(data = FDensWQsem.No0_NEW %>% filter(!STATION %in% c("CB1.1", "TF3.1E", "RET5.1A", "TF3.1E", "TF 4.4")) %>% group_by(year) %>% 
                  summarize(Temp.F = mean(Temp.sumy1me, na.rm = T)), 
                aes(x = year, y = Temp.F), method = "loess", 
                size = 1.5, color = "chartreuse2", fill = "chartreuse1") +
    theme_bw(base_size=20) + 
    labs(y = expression(paste("Fresh zone sum mean Temp")), x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  F_zoneTemp
  F_zoneTemp.filt
 
  
  TN.F_allScenarios = 
    ggplot(data = F_CC.wland_Predict) + 
    geom_line(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TN.summe)), 
              aes(x = year, y = TN, group = simnum), alpha = .3, color = "coral1") +
    geom_line(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TN.summe)), 
              aes(x = year, y = TN, group = simnum), alpha = .3, color = "cyan") + 
    geom_line(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TN.summe)), 
              aes(x = year, y = TN, group = simnum), alpha = .3, color = "darkslateblue") + 
    stat_smooth(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
                  summarize(TN = mean(TN.summe)), 
                aes(x = year, y = TN), method = "gam", 
                size = 4, color = "coral", fill = "coral1") +
    stat_smooth(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                  summarize(TN = mean(TN.summe)), 
                aes(x = year, y = TN), method = "gam", 
                size = 4, color = "cyan", fill = "cyan") +
    stat_smooth(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                  summarize(TN = mean(TN.summe)), 
                aes(x = year, y = TN), method = "gam", 
                size = 4, color = "darkslateblue", fill = "darkslateblue") +
    stat_smooth(data = FDensWQsem.No0_NEW %>% filter(!year == 2020) %>% group_by(year) %>% 
                  summarize(TN = mean(TN.summe)), 
                aes(x = year, y = TN), method = "loess", 
                size = 4, color = "burlywood", fill = "burlywood1") +
    theme_bw(base_size=24) + 
    labs(y = "mean summer TN", x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  TN.F_allScenarios
  
  TP.F_allScenarios = 
    ggplot(data = F_CC.wland_Predict) + 
    geom_line(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TP.summe)), 
              aes(x = year, y = TN, group = simnum), alpha = .3, color = "coral1") +
    geom_line(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TP.summe)), 
              aes(x = year, y = TN, group = simnum), alpha = .3, color = "cyan") + 
    geom_line(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                summarize(TN = mean(TP.summe)), 
              aes(x = year, y = TN, group = simnum), alpha = .3, color = "darkslateblue") + 
    stat_smooth(data = F_CC.wland_Predict %>% group_by(simnum, year) %>% 
                  summarize(TN = mean(TP.summe)), 
                aes(x = year, y = TN), method = "gam", 
                size = 4, color = "coral", fill = "coral1") +
    stat_smooth(data = F_WIP.wland_Predict %>% group_by(simnum, year) %>% 
                  summarize(TN = mean(TP.summe)), 
                aes(x = year, y = TN), method = "gam", 
                size = 4, color = "cyan", fill = "cyan") +
    stat_smooth(data = F_WIP.woland_Predict %>% group_by(simnum, year) %>% 
                  summarize(TN = mean(TP.summe)), 
                aes(x = year, y = TN), method = "gam", 
                size = 4, color = "darkslateblue", fill = "darkslateblue") +
    stat_smooth(data = FDensWQsem.No0_NEW %>% filter(!year == 2020) %>% group_by(year) %>% 
                  summarize(TN = mean(TP.summe)), 
                aes(x = year, y = TN), method = "loess", 
                size = 4, color = "burlywood", fill = "burlywood1") +
    theme_bw(base_size=24) + 
    labs(y = "mean summer TP", x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  TP.F_allScenarios 
  
#All communities by Scenario####

#CC.wland all communities####
#load in the segment info####
  
seg_info = read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Stations_byBasinGroup.xlsx", col_names = T, skip = 1)
  
Basins = seg_info %>% select(station, latitude, longitude, BasinSummaryGroup) %>%
  rename(STATION = station, BASIN = BasinSummaryGroup)

ZCCbasin = Zo_CC.wland_Predict %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  add_column(., SpCluster = "Zostera")

RCCbasin = Ru_CC.wland_Predict %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
MCCbasin = MM_CC.wland_Predict %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
FCCbasin = F_CC.wland_Predict %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
#ZCC####
ZCC = Zo_CC.wland_Predict %>% 
    add_column(., SpCluster = "Zostera") %>%
    mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
#RCC####
RCC = Ru_CC.wland_Predict %>% 
    add_column(., SpCluster = "Ruppia") %>%
    mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
MCC = MM_CC.wland_Predict %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
FCC = F_CC.wland_Predict %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))


#allcomm_CC.df#### 
allcomm_CC.df = ZCC %>% bind_rows(RCC) %>% bind_rows(MCC) %>% bind_rows(FCC) %>% filter(!year == "2020") %>% ungroup()
    #group_by(SpCluster, simnum) %>% slice_sample(., n = 42, replace = F) %>%
    #group_by(year, SpCluster) %>% summarize(DWM.total = mean(DWM.total, na.rm = T))
allcommbasin_CC.df = ZCCbasin %>% bind_rows(RCCbasin) %>% bind_rows(MCCbasin) %>% bind_rows(FCCbasin) %>% filter(!year == "2020") %>% ungroup()

qplot(x = year, y = Area.total, color = SpCluster, geom = "smooth", method = "gam", data = allcomm_byyear_CC)

#allcomm_past.df All Past data here######
allcomm_past.df = SAVCommunityDens_AllStations %>% rename("year" = "Year") %>% 
  group_by(SpCluster, year) %>%
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(SAVArea, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  filter(!SpCluster == "RuZo") %>% ungroup()

allcommbasin_past.df = SAVCommunityDens_AllStations %>% rename("year" = "Year", STATION = Station) %>% 
  left_join(Basins, by = c("STATION")) %>%
  group_by(SpCluster, BASIN, year) %>%
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(SAVArea, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  filter(!SpCluster == "RuZo") %>% ungroup()

  
colorme = c("darkgoldenrod1", "darkorchid", "brown2",  "chartreuse2") #alpha order: Fresh, MM, Ru, Zo
#Graph AllComms_CC.wland DPCC####
#mean DPCC is soooo small, bc of the small changes. Filter out maybe?
AllCommsDPCC_CC.wland =
ggplot(data = Zo_CC.wland_Predict) + 
  geom_smooth(data = Zo_CC.wland_Predict, 
              aes(x = year, y = dens.percomp.change), 
              method = "lm", size = 2, color = "chartreuse2") +
  geom_smooth(data = Ru_CC.wland_Predict, 
              aes(x = year, y = dens.percomp.change), 
              method = "lm", size = 2, color = "brown2") +
  geom_smooth(data = MM_CC.wland_Predict, 
              aes(x = year, y = dens.percomp.change), 
              method = "lm", size = 2, color = "darkorchid") +
  geom_smooth(data = F_CC.wland_Predict, 
              aes(x = year, y = dens.percomp.change), 
              method = "lm", size = 2, color = "darkgoldenrod1") +
  theme_bw(base_size=30) + 
  ylab("Predicted Grass Change HA") + xlab("") +
  labs(y = expression(paste("Baywide SAV area \n (density-weighted HA)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
AllCommsDPCC_CC.wland

#AllDWM CC####
  ggplot(data = allcomm_past.df) + 
  stat_summary(data = allcomm_past.df, 
               aes(x = year, y = DWM.total, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3) +
  stat_summary(data = allcomm_CC.df, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3) +
  geom_smooth(data = allcomm_CC.df, 
             aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster),
              size = .5, alpha = .2) +
  geom_labelsmooth(data = allcomm_CC.df, 
                   aes(x = year, y = DWM.total, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
  geom_text(aes(x = 2040, y = 10000, label = "Climate change + land use change"), 
            stat = "unique", size = 8, color = "coral") +
  labs(y = expression(paste("Baywide SAV area \n (density-weighted HA)")), x = "") +
  theme_bw(base_size=30) + 
  ylab("Total Ches Bay SAV Density (HA)") + xlab("") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
AllCommsDWM_CC.wland


#FIg 4: AllCommsDWM_cc####
AllCommsTotalDWM_CC.wland =
  ggplot(data = allcomm_past.df) + 
  stat_summary(data = allcomm_past.df, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  stat_summary(data = allcomm_CC.df, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  geom_line(data = allcomm_CC.df %>% filter(SpCluster == "Zostera"),
            aes(x = year, y = DWM.total, group = simnum), 
            size = .1, alpha = .4, color = "chartreuse2") +
  geom_line(data = allcomm_CC.df %>% filter(SpCluster == "Ruppia"),
            aes(x = year, y = DWM.total, group = simnum), 
            size = .1, alpha = .4, color = "brown2") +
  geom_line(data = allcomm_CC.df %>% filter(SpCluster == "MixedMeso"),
            aes(x = year, y = DWM.total, group = simnum),  
            size = .1, alpha = .4, color = "darkorchid") +
  geom_line(data = allcomm_CC.df %>% filter(SpCluster == "Fresh"),
            aes(x = year, y = DWM.total, group = simnum),
            size = .1, alpha = .4, color = "darkgoldenrod") +
  geom_labelsmooth(data = allcomm_CC.df, 
                   aes(x = year, y = DWM.total, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
  geom_text(aes(x = 2040, y = 10000, label = "No Action"), 
             stat = "unique", size = 8, color = "coral") +
  labs(y = expression(paste("SAV Community Area\n (density-weighted mean)")), x = "") +
  theme_bw(base_size=30) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
AllCommsTotalDWM_CC.wland
  
#Zwip Basins####
ZWIP.wlbasin = Zo_WIP.wland_Predict %>% 
  add_column(., SpCluster = "Zostera") %>%
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

RWIP.wlbasin = Ru_WIP.wland_Predict %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

MWIP.wlbasin = MM_WIP.wland_Predict %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

FWIP.wlbasin = F_WIP.wland_Predict %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))



  #ZWIP.wl WIP.wland####

ZWIP.wl = Zo_WIP.wland_Predict %>% 
    add_column(., SpCluster = "Zostera") %>%
    mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))

  #RwipWL####
RWIP.wl = Ru_WIP.wland_Predict %>% 
    add_column(., SpCluster = "Ruppia") %>%
    mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
MWIP.wl = MM_WIP.wland_Predict %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

FWIP.wl = F_WIP.wland_Predict %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

FWIP.wl_FLATS = F_WIP.wland_Predict %>% filter(STATION == "CB1.1") %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))


  #allcomm#### 
  allcomm_WIP.wl.df = ZWIP.wl %>% full_join(RWIP.wl) %>% full_join(MWIP.wl) %>% full_join(FWIP.wl) %>% filter(!year == "2020") %>% ungroup()
  #group_by(SpCluster, simnum) %>% slice_sample(., n = 42, replace = F) %>%
  #group_by(year, SpCluster) %>% summarize(DWM.total = mean(DWM.total, na.rm = T))
allcommbasin_WIP.wl.df = ZWIP.wlbasin %>% full_join(RWIP.wlbasin) %>% full_join(MWIP.wlbasin) %>% full_join(FWIP.wlbasin) %>% filter(!year == "2020") %>% ungroup()
  
  
  colorme = c("darkgoldenrod1", "darkorchid", "brown2",  "chartreuse2") #alpha order: Fresh, MM, Ru, Zo
#AllComms_WIP.wland####
  AllComms_WIP.wland =
    ggplot(data = Zo_WIP.wland_Predict) + 
    stat_summary(data = Zo_WIP.wland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = .1, alpha = .2, color = "chartreuse2") +
    stat_summary(data = Ru_WIP.wland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = .1, alpha = .2, color = "brown2") +
    stat_summary(data = MM_WIP.wland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1),
                 size = .1, alpha = .2, color = "darkorchid") +
    stat_summary(data = F_WIP.wland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = .1, alpha = .2, color = "darkgoldenrod1") +
    geom_smooth(data = Zo_WIP.wland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "chartreuse2") +
    geom_smooth(data = Ru_WIP.wland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "brown2") +
    geom_smooth(data = MM_WIP.wland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "darkorchid") +
    geom_smooth(data = F_WIP.wland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "darkgoldenrod1") +
    theme_bw(base_size=30) + 
    ylab("Predicted Grass Change HA") + xlab("") +
    # geom_text(aes(x = 2030, y = 6900, label = "Climate change + land use change"), stat = "unique", size = 8, color = "coral") +
    labs(y = expression(paste("Baywide SAV area \n (density-weighted HA)")), x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  AllComms_WIP.wland
  
#AllCommsDWM_cc####

AllCommsTotalDWM_WIP.wland =
    ggplot(data = allcomm_past.df) + 
    stat_summary(data = allcomm_past.df, 
                 aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 3, alpha = .8) +
    stat_summary(data = allcomm_WIP.wl.df, 
                 aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 3, alpha = .8) +
    geom_line(data = allcomm_WIP.wl.df %>% filter(SpCluster == "Zostera"),
              aes(x = year, y = DWM.total, group = simnum), 
              size = .1, alpha = .4, color = "chartreuse2") +
    geom_line(data = allcomm_WIP.wl.df %>% filter(SpCluster == "Ruppia"),
              aes(x = year, y = DWM.total, group = simnum), 
              size = .1, alpha = .4, color = "brown2") +
    geom_line(data = allcomm_WIP.wl.df %>% filter(SpCluster == "MixedMeso"),
              aes(x = year, y = DWM.total, group = simnum),  
              size = .1, alpha = .4, color = "darkorchid") +
    geom_line(data = allcomm_WIP.wl.df %>% filter(SpCluster == "Fresh"),
              aes(x = year, y = DWM.total, group = simnum),
              size = .1, alpha = .4, color = "darkgoldenrod") +
    geom_labelsmooth(data = allcomm_WIP.wl.df, 
                     aes(x = year, y = DWM.total, color = SpCluster, label = SpCluster),
                     method = "gam", boxlinewidth = 0, size = 5) +
    geom_text(aes(x = 2000, y = 12500, label = "Nutrient Reduction"), 
              stat = "unique", size = 8, color = "darkslateblue") +
    labs(y = expression(paste("SAV Community Area\n (density-weighted mean)")), x = "") +
    theme_bw(base_size=30) + 
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    scale_y_continuous(breaks = seq(0, 15000, 2500)) +
    scale_color_manual(values = colorme) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
  AllCommsTotalDWM_WIP.wland


  
#ZWIP_wol basins####
  ZWIP.wolbasin = Zo_WIP.woland_Predict %>% 
    add_column(., SpCluster = "Zostera") %>%
    mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
    left_join(Basins, by = c("STATION")) %>%
    group_by(BASIN, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  RWIP.wolbasin = Ru_WIP.woland_Predict %>% 
    add_column(., SpCluster = "Ruppia") %>%
    mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
    left_join(Basins, by = c("STATION")) %>%
    group_by(BASIN, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  MWIP.wolbasin = MM_WIP.woland_Predict %>% 
    add_column(., SpCluster = "MixedMeso") %>%
    mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
    left_join(Basins, by = c("STATION")) %>%
    group_by(BASIN, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  FWIP.wolbasin = F_WIP.woland_Predict %>% 
    add_column(., SpCluster = "Fresh") %>%
    mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
    left_join(Basins, by = c("STATION")) %>%
    group_by(BASIN, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))

  #All comms, WIP.woland####
  ZWIP.wol = Zo_WIP.woland_Predict %>% 
    add_column(., SpCluster = "Zostera") %>%
    mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  #RwipWoL####
  RWIP.wol = Ru_WIP.woland_Predict %>% 
    add_column(., SpCluster = "Ruppia") %>%
    mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  MWIP.wol = MM_WIP.woland_Predict %>% 
    add_column(., SpCluster = "MixedMeso") %>%
    mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  FWIP.wol = F_WIP.woland_Predict %>% 
    add_column(., SpCluster = "Fresh") %>%
    mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
    group_by(SpCluster, year, simnum) %>% 
    summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
              Area.total = sum(Area, na.rm = T), 
              DPC.mean = mean(dens.percomp, na.rm = T), 
              DPCC.mean = mean(dens.percomp.change, na.rm = T))
  
  
allcomm_WIP.wol.df = ZWIP.wol %>% full_join(RWIP.wol) %>% full_join(MWIP.wol) %>% full_join(FWIP.wol) %>% filter(!year == "2020") %>% ungroup()
  #group_by(SpCluster, simnum) %>% slice_sample(., n = 42, replace = F) %>%
  #group_by(year, SpCluster) %>% summarize(DWM.total = mean(DWM.total, na.rm = T))
allcommbasin_WIP.wol.df = ZWIP.wolbasin %>% full_join(RWIP.wolbasin) %>% full_join(MWIP.wolbasin) %>% full_join(FWIP.wolbasin) %>% filter(!year == "2020") %>% ungroup()
  
  colorme = c("darkgoldenrod1", "darkorchid", "brown2",  "chartreuse2") #alpha order: Fresh, MM, Ru, Zo
  #AllComms_WIP.woland####
  AllComms_WIP.woland =
    ggplot(data = Zo_WIP.woland_Predict) + 
    stat_summary(data = Zo_WIP.woland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = .1, alpha = .2, color = "chartreuse2") +
    stat_summary(data = Ru_WIP.woland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = .1, alpha = .2, color = "brown2") +
    stat_summary(data = MM_WIP.woland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1),
                 size = .1, alpha = .2, color = "darkorchid") +
    stat_summary(data = F_WIP.woland_Predict, 
                 aes(x = year, y = dens.weight.mean, group = simnum), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = .1, alpha = .2, color = "darkgoldenrod1") +
    geom_smooth(data = Zo_WIP.woland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "chartreuse2") +
    geom_smooth(data = Ru_WIP.woland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "brown2") +
    geom_smooth(data = MM_WIP.woland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "darkorchid") +
    geom_smooth(data = F_WIP.woland_Predict, 
                aes(x = year, y = dens.weight.mean), 
                method = "gam", size = 1.2, color = "darkgoldenrod1") +
    theme_bw(base_size=30) + 
    ylab("Predicted Grass Change HA") + xlab("") +
    # geom_text(aes(x = 2030, y = 6900, label = "Climate change + land use change"), stat = "unique", size = 8, color = "coral") +
    labs(y = expression(paste("Baywide SAV area \n (density-weighted HA)")), x = "") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
  AllComms_WIP.woland
  
  #AllCommsDWM_WIP WO####
AllCommsTotalDWM_WIP.w0land =
    ggplot(data = allcomm_past.df) + 
    stat_summary(data = allcomm_past.df, 
                 aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 3, alpha = .8) +
    stat_summary(data = allcomm_WIP.wol.df, 
                 aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 3, alpha = .8) +
    geom_line(data = allcomm_WIP.wol.df %>% filter(SpCluster == "Zostera"),
              aes(x = year, y = DWM.total, group = simnum), 
              size = .1, alpha = .4, color = "chartreuse2") +
    geom_line(data = allcomm_WIP.wol.df %>% filter(SpCluster == "Ruppia"),
              aes(x = year, y = DWM.total, group = simnum), 
              size = .1, alpha = .4, color = "brown2") +
    geom_line(data = allcomm_WIP.wol.df %>% filter(SpCluster == "MixedMeso"),
              aes(x = year, y = DWM.total, group = simnum),  
              size = .1, alpha = .4, color = "darkorchid") +
    geom_line(data = allcomm_WIP.wol.df %>% filter(SpCluster == "Fresh"),
              aes(x = year, y = DWM.total, group = simnum),
              size = .1, alpha = .4, color = "darkgoldenrod") +
    geom_labelsmooth(data = allcomm_WIP.wol.df, 
                     aes(x = year, y = DWM.total, color = SpCluster, label = SpCluster),
                     method = "gam", boxlinewidth = 0, size = 5) +
    geom_text(aes(x = 2020, y = 13000, label = "Nutrient Reduction and Land-Use Management"), 
              stat = "unique", size = 6, color = "cyan3") +
    labs(y = expression(paste("SAV Community Area\n (density-weighted mean)")), x = "") +
    theme_bw(base_size=30) + 
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    scale_y_continuous(breaks = seq(0, 15000, 2500)) +
    scale_color_manual(values = colorme) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
  AllCommsTotalDWM_WIP.w0land

#Allcoms WIP WO DPC####
  AllCommsDPC_WIP.woland =
    ggplot(data = allcomm_past.dfWIPwl) + 
    stat_smooth(data = allcomm_past.dfWIPwl, 
                aes(x = year, y = DPC.mean, group = SpCluster, color = SpCluster), 
                method = "loess", size = 3) +
    stat_smooth(data = allcomm_WIP.wol.df, 
                aes(x = year, y = DPC.mean, group = SpCluster, color = SpCluster), 
                method = "loess", size = 3) +
  geom_labelsmooth(data = allcomm_WIP.wol.df, 
                   aes(x = year, y = DPC.mean, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
    labs(y = expression(paste("Baywide SAV area \n (density-weighted HA)")), x = "") +
    theme_bw(base_size=30) + 
    ylab("Total Ches Bay SAV Density (HA)") + xlab("") +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    scale_color_manual(values = colorme) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
  AllCommsDPC_WIP.woland
  
TheMultiverse =  AllCommsDWM_CC.wland / AllCommsDWM_WIP.wland / AllCommsDWM_WIP.woland
  

#Total SAV in Bay by Scenario####
##CC, WIP.wl, WIP.wol####
CC = allcomm_CC.df %>% add_column(., Scenario = "CC.wland") 
WIP.wl = allcomm_WIP.wl.df %>% add_column(., Scenario = "WIP.wl") 
WIP.wol = allcomm_WIP.wol.df %>% add_column(., Scenario = "WIP.wol") 


allcomm_byyear_CC = allcomm_CC.df %>% add_column(., Scenario = "CC.wland") %>%
  group_by(Scenario, year, simnum) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcomm_byyear_WIPwl = allcomm_WIP.wl.df %>% add_column(., Scenario = "WIP.wl"w) %>%
  group_by(Scenario, year, simnum) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcomm_byyear_WIPwol = allcomm_WIP.wol.df %>% add_column(., Scenario = "WIP.wol") %>%
  group_by(Scenario, year, simnum) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcomPastFull.DF = allcomm_past.df %>% group_by(year) %>% 
  summarize(DWM.total = sum(DWM.total, na.rm = T), Area.total = sum(Area.total, na.rm = T))


fullFutureSAV.DF = full_join(allcomm_byyear_CC, allcomm_byyear_WIPwl) %>%
  full_join(allcomm_byyear_WIPwol) %>% 
  filter(!year == 2021) %>% #i have no fucking clue why this is a problem but whatever
  mutate(Scenario = case_when(Scenario == "CC.wland" ~ "No Act", 
                              Scenario == "WIP.wl" ~ "Nut Reduce", 
                              Scenario == "WIP.wol" ~ "Nut Land Man"))
  
fullFutureSAV.DF %>% group_by(Scenario, year) %>% summarize(me.Area = mean(Area.total)) %>%
summarize(Ame = mean(me.Area), Amax = max(me.Area))




Scenario        Ame   Amax
  1 No Act       26569. 28266.
2 Nut Land Man 34588. 37406.
3 Nut Reduce   34983. 38053.

allcomPastFull.DF %>% summarize(Ame = mean(Area.total), Amax = max(Area.total))

Ame   Amax
<dbl>  <dbl>
  1 26422. 40566.

View(fullFutureSAV.DF %>% group_by(Scenario, simnum) %>% 
  summarize(Ame = mean(Area.total), Amax = max(Area.total)))

 
scen_colors = c("coral", "cyan3", "deeppink")

fullBay.Scenario =
  ggplot(data = allcomPastFull.DF) + 
  stat_summary(data = allcomPastFull.DF, 
                 aes(x = year, y = Area.total), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 4) +
  stat_summary(data = fullFutureSAV.DF, 
                 aes(x = year, y = Area.total, group = Scenario, color = Scenario), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 4) +
  geom_line(data = fullFutureSAV.DF %>% filter(Scenario == "No Act"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "coral") +
  geom_line(data = fullFutureSAV.DF %>% filter(Scenario == "Nut Reduce"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "deeppink") +
  geom_line(data = fullFutureSAV.DF %>% filter(Scenario == "Nut Land Man"),
            aes(x = year, y = Area.total, group = simnum),
            size = .3, alpha = .2, color = "cyan3") +
  geom_labelsmooth(data = fullFutureSAV.DF, 
                   aes(x = year, y = Area.total, group = Scenario, 
                       color = Scenario, label = Scenario),
                   method = "gam", boxlinewidth = 0, size = 6) +
  geom_hline(yintercept = 74000) +
  geom_text(aes(x = 2020, y = 72000, label = "Baywide SAV Goal (74,000 HA)"), stat = "unique", size = 5, color = "blue") +
  labs(y = "Total SAV area (HA)", x = "") +
  theme_bw(base_size=42) + 
  ylim(0, 78000) +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_color_manual(values = scen_colors) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
fullBay.Scenario

#fullBay.ScenarioBasin #####
#by basin
allcommbasin_byyear_CC = allcommbasin_CC.df %>% 
  add_column(., Scenario = "CC.wland") %>%
  mutate(BASIN = case_when(BASIN == "VA MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD UPPER ES" ~ "MD WS ES", 
                           BASIN == "MD UPPER WS" ~ "MD WS ES",
                           BASIN == "MD LOWER WS" ~ "MD WS ES", 
                           BASIN == "PATAPSCO-BACK" ~ "MD WS ES", 
                           BASIN == "PATUXENT" ~ "POTOMAC",
                           BASIN == "YORK" ~ "YORK JAMES", 
                           BASIN == "JAMES" ~ "YORK JAMES",
                           T ~ BASIN)) %>% 
  group_by(Scenario, year, simnum, BASIN) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcommbasin_byyear_WIPwl = allcommbasin_WIP.wl.df %>% 
  add_column(., Scenario = "WIP.wl") %>%
  mutate(BASIN = case_when(BASIN == "VA MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD UPPER ES" ~ "MD WS ES", 
                           BASIN == "MD UPPER WS" ~ "MD WS ES",
                           BASIN == "MD LOWER WS" ~ "MD WS ES", 
                           BASIN == "PATAPSCO-BACK" ~ "MD WS ES", 
                           BASIN == "PATUXENT" ~ "POTOMAC",
                           BASIN == "YORK" ~ "YORK JAMES", 
                           BASIN == "JAMES" ~ "YORK JAMES",
                           T ~ BASIN)) %>% 
  group_by(Scenario, year, simnum, BASIN) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcommbasin_byyear_WIPwol = allcommbasin_WIP.wol.df %>% add_column(., Scenario = "WIP.wol") %>% 
  mutate(BASIN = case_when(BASIN == "VA MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD UPPER ES" ~ "MD WS ES", 
                           BASIN == "MD UPPER WS" ~ "MD WS ES",
                           BASIN == "MD LOWER WS" ~ "MD WS ES", 
                           BASIN == "PATAPSCO-BACK" ~ "MD WS ES", 
                           BASIN == "PATUXENT" ~ "POTOMAC",
                           BASIN == "YORK" ~ "YORK JAMES", 
                           BASIN == "JAMES" ~ "YORK JAMES",
                           T ~ BASIN)) %>% 
  group_by(Scenario, year, simnum, BASIN) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcombasinPastFull.DF = allcommbasin_past.df %>% 
  mutate(BASIN = case_when(BASIN == "VA MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD MAIN" ~ "VA MD MAIN", 
                           BASIN == "MD UPPER ES" ~ "MD WS ES", 
                           BASIN == "MD UPPER WS" ~ "MD WS ES",
                           BASIN == "MD LOWER WS" ~ "MD WS ES", 
                           BASIN == "PATAPSCO-BACK" ~ "MD WS ES", 
                           BASIN == "PATUXENT" ~ "POTOMAC",
                           BASIN == "YORK" ~ "YORK JAMES", 
                           BASIN == "JAMES" ~ "YORK JAMES",
                           T ~ BASIN)) %>%  
  filter(!BASIN == "NA") %>% 
  group_by(year, BASIN) %>% 
  summarize(DWM.total = sum(DWM.total, na.rm = T), Area.total = sum(Area.total, na.rm = T)) 


fullFutureSAVbasin.DF = full_join(allcommbasin_byyear_CC, allcommbasin_byyear_WIPwl) %>%
  full_join(allcommbasin_byyear_WIPwol) %>% 
  filter(!year == 2021) %>% #i have no fucking clue why this is a problem but whatever
  filter(!BASIN == "NA") %>%
  mutate(Scenario = case_when(Scenario == "CC.wland" ~ "No Act", 
                              Scenario == "WIP.wl" ~ "Nut Reduce", 
                              Scenario == "WIP.wol" ~ "Nut Land Man")) 

basinfuture = fullFutureSAVbasin.DF %>% group_by(Scenario, BASIN, year) %>%
  summarize(me.Area = mean(Area.total)) %>%
  summarize(Ame = mean(me.Area), Amax = max(me.Area))

fullBay.Scenariobasin =
  ggplot(data = allcombasinPastFull.DF) +
  geom_line(data = fullFutureSAVbasin.DF,
            aes(x = year, y = Area.total, group = simnum, color = BASIN), 
            size = .3, alpha = .2) +
  stat_summary(data = allcombasinPastFull.DF, 
               aes(x = year, y = Area.total, group = BASIN, color = BASIN ), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                        size = 2) +
  stat_summary(data = fullFutureSAVbasin.DF, 
                      aes(x = year, y = Area.total, group = BASIN, color = BASIN),
                        fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                        size = 2) +
  facet_grid(BASIN ~ Scenario, scales = "free_y", space = "free") + 
  labs(y = "Total Baywide SAV area (HA)", x = "") +
  theme_bw(base_size=10) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  # scale_color_manual(values = scen_colors) +
  theme(#plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
fullBay.Scenariobasin

ggplot(data = allcombasinPastFull.DF) +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "CHOPTANK"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#F8766D") +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "LOWER ES"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#C49A00") +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "MD WS ES"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#53B400") +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "POTOMAC"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#00C094") +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "RAPPAHANNOCK"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#00B6EB") +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "VA MD MAIN"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#A58AFF") +
  geom_line(data = fullFutureSAVbasin.DF %>% filter(BASIN == "YORK JAMES"),
            aes(x = year, y = Area.total, group = simnum), 
            size = .3, alpha = .2, color = "#FB61D7") +
#  geom_labelsmooth(data = fullFutureSAVbasin.DF, 
 #                  aes(x = year, y = Area.total, group = Scenario, 
  #                     color = Scenario, label = Scenario),
   #                method = "gam", boxlinewidth = 0, size = 6) +
  # geom_hline(yintercept = 74000) +
  labs(y = "Total Baywide SAV area (HA)", x = "") +
  theme_bw(base_size=30) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
 # scale_color_manual(values = scen_colors) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")



#View(ggplot_build(fullBay.Scenariobasin)$data)



ggplot( data = ZoDensWQsem.No0_NEW) + 
  stat_summary(data = ZoDensWQsem.No0_NEW, 
               aes(x = year, y = Temp.sumy1med, color = Temp.sumy1med), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 2) +
  stat_summary(data = RuDensWQsem.No0_NEW, 
               aes(x = year, y = Chla.spme, color = Chla.spme), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 2) +
  geom_line(data = ZoDensWQsem.No0_NEW, 
               aes(x = year, y = Temp.sumy1med, group = STATION, color = Temp.sumy1med), 
               size = .5, alpha = .5) +

######
#extras  ######
ggplot(data = Zo_CC.wland_Predict) + 
  # stat_summary(data = Zo_CC.wland_Predict, aes(x = year, y = dens.percomp.change), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2) +
  stat_summary(data = Zo_CC.wland_Predict, aes(x = year, y = dens.percomp.change, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3, color = "green") +
  geom_smooth(data = Zo_CC.wland_Predict, aes(x = year, y = dens.percomp.change), method = "lm", size = 1.2, color = "green") +
  # stat_summary(data = Ru_CC.wland_Predict, aes(x = year, y = dens.percomp.change), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "brown") +
  stat_summary(data = Ru_CC.wland_Predict, aes(x = year, y = dens.percomp.change, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3, color = "brown") +
  geom_smooth(data = Ru_CC.wland_Predict, aes(x = year, y = dens.percomp.change), method = "lm", size = 1.2, color = "brown") +
  #stat_summary(data = MM_CC.wland_Predict, aes(x = year, y = dens.percomp.change), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "pink") +
  stat_summary(data = MM_CC.wland_Predict, aes(x = year, y = dens.percomp.change, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3, color = "pink") +
  geom_smooth(data = MM_CC.wland_Predict, aes(x = year, y = dens.percomp.change), method = "lm", size = 1.2, color = "pink") +
  # stat_summary(data = F_CC.wland_Predict, aes(x = year, y = dens.percomp.change), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "light blue") +
  stat_summary(data = F_CC.wland_Predict, aes(x = year, y = dens.percomp.change, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3, color = "light blue") +
  geom_smooth(data = F_CC.wland_Predict, aes(x = year, y = dens.percomp.change), method = "lm", size = 1.2, color = "light blue") +
  theme_bw(base_size=12) + 
  ylab("Predicted Grass Change HA") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

#Zostera predictions by Scenario####
Zo_CC.wland_Predict
Zo_WIP.wland_Predict
a=
ggplot(data = Zo_CC.wland_Predict) +
  stat_summary(data = Zo_CC.wland_Predict, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "green") +
  stat_summary(data = Zo_CC.wland_Predict, aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3,) +
  geom_smooth(data = Zo_CC.wland_Predict, aes(x = year, y = dens.weight.mean), method = "lm", size = 1.2, color = "green") +
  theme_bw(base_size=12) + 
  ylab("Predicted Zostera Grass Change HA") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
b=
ggplot(data = Zo_WIP.wland_Predict) +
  stat_summary(data = Zo_CC.wland_Predict, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "green") +
  stat_summary(data = Zo_CC.wland_Predict, aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3,) +
  geom_smooth(data = Zo_CC.wland_Predict, aes(x = year, y = dens.weight.mean), method = "lm", size = 1.2, color = "green") +
  theme_bw(base_size=12) + 
  ylab("Predicted Zostera Grass Change HA") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
c=
ggplot(data = Zo_WIP.woland_Predict) +
  stat_summary(data = Zo_WIP.woland_Predict, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "green") +
  stat_summary(data = Zo_WIP.woland_Predict, aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3,) +
  geom_smooth(data = Zo_WIP.woland_Predict, aes(x = year, y = dens.weight.mean), method = "lm", size = 1.2, color = "green") +
  theme_bw(base_size=12) + 
  ylab("Predicted Zostera Grass Change HA") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

library(patchwork)
a / b / c

FW
ggplot(data = F_CC.wland_Predict) +
  stat_summary(data = F_CC.wland_Predict, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "light blue") +
  stat_summary(data = F_CC.wland_Predict, aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3,) +
  geom_smooth(data = F_CC.wland_Predict, aes(x = year, y = dens.weight.mean), method = "lm", size = 1.2, color = "light blue") +
  theme_bw(base_size=12) + 
  ylab("Predicted FW Grass Change HA") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

ggplot(data = F_CC.wland_Predict) +
  stat_summary(data = F_CC.wland_Predict, aes(x = year, y = dens.percomp.change), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .2, color = "light blue") +
  stat_summary(data = F_CC.wland_Predict, aes(x = year, y = dens.percomp.change, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .3,) +
  geom_smooth(data = F_CC.wland_Predict, aes(x = year, y = dens.percomp.change), method = "lm", size = 1.2, color = "light blue") +
  theme_bw(base_size=12) + 
  ylab("Predicted FW Grass Change HA") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

Ru

  ggplot(data = Ru_WIP.woland_Predict %>% filter(STATION == "CB7.3E")) +
  stat_summary(data = Ru_WIP.woland_Predict %>% filter(STATION == "CB7.3E"), 
               aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .5, color = "black") +
  stat_summary(data = Ru_WIP.woland_Predict %>% filter(STATION == "CB7.3E"), 
               aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .7,color = "black") +
    stat_summary(data = Zo_WIP.woland_Predict %>% filter(STATION == "CB7.3E"), 
                 aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .5, color = "blue") +
    stat_summary(data = Zo_WIP.woland_Predict %>% filter(STATION == "CB7.3E"), 
                 aes(x = year, y = dens.weight.mean, group = simnum), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .7, color = "blue") +
 # geom_smooth(aes(x = year, y = dens.percomp.change), method = "lm", size = 1.2, color = "brown") +
  theme_bw(base_size=12) + 
  ylab("Ru / Zo Change at CB7.3E") + xlab("") +
  #scale_fill_manual(values = wes_palette("Royal1")) +
  #scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

  
  
  
  
  


#PAST######
#load DFs, but need to write these eventually
RuDensWQsem.No0
ZoDensWQ69sem.No0
MixMesoDensWQ69sem.No0
FreshDensWQ69sem.No0

DWM.ZoZone =
  ggplot(data = ZoDensWQ69sem.No0) + 
  stat_summary(aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, color = "green") +
  stat_summary(aes(x = year, y = dens.weight.mean), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1.5, color = "green") +
  geom_smooth(aes(x = year, y = dens.weight.mean), method = "lm", size = 1.5, color = "black") +
  theme_bw(base_size=12) + 
  ylab("Zostera area (dens weight mean)") + xlab("") +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

Temp.ZoZone =
ggplot(data = ZoDensWQ69sem.No0) + 
  stat_summary(aes(x = year, y = Temp.summed), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, color = "red") +
  stat_summary(aes(x = year, y = Temp.summed), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1.5, color = "red") +
  geom_smooth(aes(x = year, y = Temp.summed), method = "lm", size = 1.5, color = "black") +
  theme_bw(base_size=12) + 
  ylab("Summer temp (median)") + xlab("") +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

Secc.ZoZone =
  ggplot(data = ZoDensWQ69sem.No0) + 
  stat_summary(aes(x = year, y = Secc.summe), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, color = "brown") +
  stat_summary(aes(x = year, y = Secc.summe), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1.5, color = "brown") +
  geom_smooth(aes(x = year, y = Secc.summe), method = "lm", size = 1.5, color = "black") +
  theme_bw(base_size=12) + 
  ylab("Summer secchi depth (mean)") + xlab("") +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")


ZoZone_DTS = DWM.ZoZone / Temp.ZoZone / Secc.ZoZone

TempDWM.ZoZone =
  ggplot(data = ZoDensWQ69sem.No0) + 
  stat_summary(aes(x = Temp.sumy1med, y = dens.weight.mean - dens.weight.mean.y1), fun.data = mean_cl_normal, geom = "point", fun.args = list(mult = 1), size = 1.4, color = "black") +
  geom_smooth(aes(x = Temp.sumy1med, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1.5, color = "dark green") +
  theme_bw(base_size=12) + 
  ylab("Change in Zostera density weighted area (HA)") + xlab(expression(paste("Previous summer median temperature", degree,"C"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

SeccDWM.ZoZone =
  ggplot(data = ZoDensWQ69sem.No0) + 
  stat_summary(aes(x = Secc.summe, y = dens.weight.mean - dens.weight.mean.y1), fun.data = mean_cl_normal, geom = "point", fun.args = list(mult = 1), size = 1.4, color = "black") +
  geom_smooth(aes(x = Secc.summe, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1.5, color = "dark green") +
  theme_bw(base_size=12) + 
  ylab("Change in Zostera density weighted area (HA)") + xlab("Summer secchi depth") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

Zo.TempSeccDWM = TempDWM.ZoZone / SeccDWM.ZoZone


ggplot() + 
  #stat_summary(data = ZoDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "green") +
  stat_summary(data = ZoDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "green") +
 # stat_summary(data = RuDensWQsem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "brown") +
  stat_summary(data = RuDensWQsem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "brown") +
 # stat_summary(data = MixMesoDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "purple") +
  stat_summary(data = MixMesoDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "purple") +
 # stat_summary(data = FreshDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "light blue") +
  stat_summary(data = FreshDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "light blue") +
  theme_bw(base_size=12) + 
  ylab("Vegetation area (dens weight mean)") + xlab("") +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

ggplot() + 
  #stat_summary(data = ZoDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "green") +
  stat_summary(data = ZoDensWQ69sem.No0, aes(x = year, y = Temp.summe), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "green") +
  # stat_summary(data = RuDensWQsem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "brown") +
  stat_summary(data = RuDensWQsem.No0, aes(x = year, y = Temp.spme), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "brown") +
  # stat_summary(data = MixMesoDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "purple") +
  stat_summary(data = MixMesoDensWQ69sem.No0, aes(x = year, y = Temp.summe), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "purple") +
  # stat_summary(data = FreshDensWQ69sem.No0, aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .6, color = "light blue") +
  stat_summary(data = FreshDensWQ69sem.No0, aes(x = year, y = Temp.summe), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1, color = "light blue") +
  theme_bw(base_size=12) + 
  ylab("Summer Temp (median)") + xlab("") +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

ggplot() +
  geom_smooth(data = ZoDensWQ69sem.No0, aes(x = Temp.sumy1med, y = dens.percomp.change), method = "lm", size = 1, color = "green") +
  geom_smooth(data = RuDensWQsem.No0, aes(x = Temp.spme, y = dens.percomp.change), method = "lm", size = 1, color = "brown") +
  geom_smooth(data = MixMesoDensWQ69sem.No0, aes(x = Temp.summin, y = dens.percomp.change), method = "lm", size = 1, color = "purple") +
  geom_smooth(data = FreshDensWQ69sem.No0, aes(x = Temp.sumy1me, y = dens.percomp.change), method = "lm", size = 1, color = "light blue") +
  theme_bw(base_size=12) + 
  ylab("Vegetation Change") + xlab("Temperature") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")



ggplot()+
  geom_smooth(data = ZoDensWQ69sem.No0, aes(x = Temp.sumy1med, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1, color = "dark green") + 
  annotate(geom="text", x=22, y=50, label="Eelgrass", color="dark green") +
  geom_smooth(data = RuDensWQsem.No0, aes(x = Temp.spme, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1, color = "brown") +
  annotate(geom="text", x=10, y=35, label="Widgeongrass", color="brown") +
  geom_smooth(data = MixMesoDensWQ69sem.No0, aes(x = Temp.summin, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1, color = "purple") +
  annotate(geom="text", x=13, y=15, label="Mixed Meso", color="purple") +
  geom_smooth(data = FreshDensWQ69sem.No0, aes(x = Temp.sumy1me, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1, color = "blue") +
  annotate(geom="text", x=18, y=42, label="Fresh/Oligo", color= "blue") +
  theme_bw(base_size=12) + 
  ylab("Vegetation change/station (mean HA)") + xlab(expression(paste("Temperature", degree,"C"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

DWMvTemp.ZoZone =
  ggplot(data = ZoDensWQ69sem.No0) + 
  stat_summary(aes(x = Temp.sumy1med, y = dens.weight.mean - dens.weight.mean.y1), fun.data = mean_se, geom = "point", fun.args = list(mult = 1), size = 1.2, color = "green") +
  geom_smooth(aes(x = Temp.sumy1med, y = dens.weight.mean - dens.weight.mean.y1), method = "lm", size = 1.5, color = "black") +
  theme_bw(base_size=12) + 
  ylab("Zostera area (dens weight mean)") + xlab("") +
 # scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")

DWMvTemp.MMZone =
  ggplot(data = MixMesoDensWQ69sem.No0) + 
  stat_summary(aes(x = Temp.summin, y = dens.percomp.change), fun.data = mean_se, geom = "point", fun.args = list(mult = 1), size = 1.2, color = "purple") +
  geom_smooth(aes(x = Temp.summin, y = dens.percomp.change), method = "lm", size = 1.5, color = "black") +
  theme_bw(base_size=12) + 
  ylab("Mixed Meso area (dens weight mean)") + xlab("") +
  # scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")








