#Use this file to visualize the predicted data. originally created to separate the viz from the initial data flow
library(tidyverse); library(ggsci); library(ggthemes); library(patchwork); library(geomtextpath); library(vroom)
colorme = c("darkgoldenrod1", "darkorchid", "brown2",  "chartreuse2")


#OneTrueBay are these ones in the ONEBAY. This is with every community getting the same sequence of years per simulation chunk. 100 sims

Zo_CC.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBae.csv")
Ru_CC.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBae.csv")
MM_CC.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_CC.PredOneTrueBae.csv")
F_CC.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBae.csv")

Zo_WIP.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBae.csv")
Ru_WIP.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBae.csv")
MM_WIP.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_WIP.PredOneTrueBae.csv")
F_WIP.PredOneTrueBae = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_WIP.PredOneTrueBae.csv")

#SeaLevelRise
Zo_CC.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_CC.PredOneTrueBaeSLR.csv")
Ru_CC.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_CC.PredOneTrueBaeSLR.csv")
MM_CC.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_CC.PredOneTrueBaeSLR.csv")
F_CC.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_CC.PredOneTrueBaeSLR.csv")

Zo_WIP.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Zo_WIP.PredOneTrueBaeSLR.csv")
Ru_WIP.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/Ru_WIP.PredOneTrueBaeSLR.csv")
MM_WIP.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/MM_WIP.PredOneTrueBaeSLR.csv")
F_WIP.PredOneTrueBaeSLR = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/F_WIP.PredOneTrueBaeSLR.csv")



#Remember ONEBAY has both past data AND the lmer input as 2000-2020####
#This OneBay doesnt have the same simnum though.
Zo_CC.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Zo_CC.wland_PredictONEBAY.csv") 
Ru_CC.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Ru_CC.wland_PredictONEBAY.csv", col_types = c(dens.percomp.change = "d", dens.weight.mean = "d"))
MM_CC.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/MM_CC.wland_PredictONEBAY.csv", col_types = c(dens.percomp.change = "d", dens.weight.mean = "d"))
F_CC.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/F_CC.wland_PredictONEBAY.csv")

Zo_WIP.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Zo_WIP.wland_PredictONEBAY.csv")
Ru_WIP.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/Ru_WIP.wland_PredictONEBAY.csv", col_types = c(dens.percomp.change = "d", dens.weight.mean = "d"))
MM_WIP.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/MM_WIP.wland_PredictONEBAY.csv", col_types = c(dens.percomp.change = "d", dens.weight.mean = "d"))
F_WIP.wland_PredictONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures SMOO/F_WIP.wland_PredictONEBAY.csv")

#Past data####
#Just density, but try not to use this
SAVCommunityDens_AllStations =read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommunityDens_AllStations.csv")

#use this one, w the 0s filtered
#SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_semForPredictions.csv") #this is named SAVCommDensWQ_ForPred on the other file
SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv") 


dwm.to.HA_Zo = lm(SAVArea ~ dens.weight.mean, data = SAVCommDensWQ_ForPred %>% 
                    filter(SpCluster == "Zostera"))
dwm.to.HA_Ru = lm(SAVArea ~ dens.weight.mean, data = SAVCommDensWQ_ForPred %>% 
                    filter(SpCluster == "Ruppia"))
dwm.to.HA_MM = lm(SAVArea ~ dens.weight.mean, data = SAVCommDensWQ_ForPred %>% 
                    filter(SpCluster == "MixedMeso"))
dwm.to.HA_F = lm(SAVArea ~ dens.weight.mean, data = SAVCommDensWQ_ForPred %>% 
                   filter(SpCluster == "Fresh"))


#testing out the new DFs to see if they diff (they are)####
#CBPall vs CBPall_DETREND

ggplot(data = CBPall_DETREND) + #
  stat_summary(data = CBPall_DETREND, 
               aes(x = year, y = detreTN), #group = Station, color = Station), 
               fun.data = mean_se, geom = "line", 
               size = 2, alpha = .8) +
  stat_summary(data = CBPall_DETREND, 
               aes(x = year, y = detreTN), #group = Station, color = Station), 
               fun.data = mean_se, geom = "line", 
               size = 2, alpha = .8, color = "blue") +
  stat_summary(data = CBP.WQ_forPredictions, 
               aes(x = year, y = TN.spme), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = 2, alpha = .8, color = "green") +
  labs(y = expression(paste("TN")), x = "") +
  theme_bw(base_size=30) + 
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")


#
F_WIP.wl_PreOLD = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures 100simnum/F_WIP.wland_Predict.csv")

fww_test =
F_WIP.PredOneTrueBae %>% 
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  filter(!STATION == "RET5.1A")   %>% 
  group_by(year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T), 
            TN = mean(TN.summe, na.rm = T), 
            Temp = mean(Temp.summe, na.rm = T)) %>%
  filter(!year %in% c("2020")) 
 
fwwOLD_test =
  F_WIP.wl_PreOLD %>% 
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(year, simnum) %>% 
  filter(!STATION == "RET5.1A")   %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T), 
            TN = mean(TN.summe, na.rm = T), #check TNsumme
            Temp = mean(Temp.summe, na.rm = T)) %>%
  filter(!year %in% c("2020"))

seeSmooth = 
ggplot(data = fww_test) + 
  stat_summary(data = fwwOLD_test, 
               aes(x = year, y = TN, group = simnum), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = .4, alpha = .4, color = "cyan1") +
  stat_summary(data = fww_test, 
               aes(x = year, y = TN, group = simnum_OB), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = .4, alpha = .4, color = "darkorchid1") +
  stat_summary(data = fwwOLD_test, 
               aes(x = year, y = TN), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = 1.2, color = "black") +
  stat_summary(data = fww_test, 
               aes(x = year, y = TN), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = 1.2, color = "black") +
  stat_summary(data = allcomm_pastSAVWQ.df %>% filter(SpCluster == "Fresh"), 
               aes(x = year, y = TN.summe), 
               fun.data = mean_se, geom = "line", 
               size = 1.4, color = "darkslateblue") +
#  labs(y = expression(paste("Area")), x = "") +
  theme_bw(base_size=30) + 
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

seeSmooth

ggplot(data = fww_test) + 
  stat_summary(data = fwwOLD_test, 
               aes(x = year, y = Temp, group = simnum), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = .4, alpha = .4, color = "cyan1") +
  stat_summary(data = fww_test, 
               aes(x = year, y = Temp, group = simnum), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = .4, alpha = .4, color = "darkorchid1") +
  stat_summary(data = fwwOLD_test, 
               aes(x = year, y = Temp), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = 1.2, color = "black") +
  stat_summary(data = fww_test, 
               aes(x = year, y = Temp), #group = STATION, color = STATION), 
               fun.data = mean_se, geom = "line", 
               size = 1.2, color = "black") +
  stat_summary(data = allcomm_pastSAVWQ.df %>% filter(SpCluster == "Fresh"), 
               aes(x = year, y = Temp.summe), 
               fun.data = mean_se, geom = "line", 
               size = 1.4, color = "darkslateblue") +
  #  labs(y = expression(paste("Area")), x = "") +
  theme_bw(base_size=30) + 
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

#Checking ONEBAY####
ggplot(data = F_WIP.PredOneTrueBae %>% filter(!year == 2020)) + 
#  geom_line(aes(x = year, y = Temp.summe), alpha = .3, color = "coral1") +
  stat_summary(aes(x = year, y = Temp.summe), geom = "line", 
               size = 3, color = "coral2") +
#  geom_line(data = F_WIP.wland_Predict, aes(x = year, y = Temp.summe), alpha = .3, color = "cyan2") +
  stat_summary(data = F_WIP.wl_PreOLD %>% filter(!year == 2020), 
               aes(x = year, y = Temp.summe), geom = "line", 
               size = 3, color = "cyan4") +
  stat_summary(data = SAVWQallClean %>% filter(SpCluster == "Fresh") %>% group_by(year) %>% 
                 summarize(Temp = mean(Temp.summe, na.rm = T)),
               aes(x = year, y = Temp), geom = "line", 
               size = 3, color = "brown") +
  geom_smooth(data = SAVWQallClean %>% filter(SpCluster == "Fresh") %>% group_by(year) %>% 
                summarize(Temp = mean(Temp.summe, na.rm = T)), 
              method = "lm",
              aes(x = year, y = Temp), alpha = .3, color = "black") +
  theme_bw(base_size=38) + 
  labs(y = expression(paste("mean summer Temperature (", degree ~ C, ")")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")


qplot(x = year, y = DWM.total, group = simnum, data = RCC_ONE, geom = "line") 

#Summary DFs for figures, can run these next 4 chunks at once!####
#Create ZCC, RCC, MCC, FCC: Summarize CC.wland_Predicts Area####
ZCC_ONE = Zo_CC.wland_PredictONEBAY %>% 
  add_column(., SpCluster = "Zostera") %>%
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
RCC_ONE = Ru_CC.wland_PredictONEBAY %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
MCC_ONE = MM_CC.wland_PredictONEBAY %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
FCC_ONE = F_CC.wland_PredictONEBAY %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

#allcomm_CC.df: All CC.wland Communities bound####
allcomm_CC.dfONE = ZCC_ONE %>% bind_rows(RCC_ONE) %>% bind_rows(MCC_ONE) %>% 
  bind_rows(FCC_ONE) %>% 
  filter(!year %in% c("2020")) %>% ungroup()

#ZWIP, RWIP,: Summarize WIP.wland_Predicts Area####
ZWIP_ONE = Zo_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "Zostera") %>%
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

RWIP_ONE = Ru_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

MWIP_ONE = MM_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

FWIP_ONE = F_WIP.PredOneTrueBae  %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(SpCluster, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))



allcomm_WIP.wl.dfONE1 = ZWIP_ONE %>% full_join(RWIP_ONE) %>% full_join(MWIP_ONE) %>% 
  full_join(FWIP_ONE) %>% filter(!year %in% c("2020")) %>% ungroup()

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

#Build Figures of Interest####

#Figures to show OneBay Scenario effects on Env Vars####
CC.temp = Zostera.OneBay_CC %>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(Temp.sumy1med = mean(Temp.sumy1med, na.rm = T), 
            Temp.spmed = mean(Temp.spmed))
WIP.temp = Zostera.OneBay_WIP %>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(Temp.sumy1med = mean(Temp.sumy1med, na.rm = T), 
            Temp.spmed = mean(Temp.spmed))

CC.TN = Ruppia.OneBay_CC%>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(TN.spme = mean(TN.spme, na.rm = T), 
            TP.spme = mean(TP.spme), 
            Chla.spme = mean(Chla.spme), 
            Sal.spme = mean(Sal.spme))
WIP.TN = Ruppia.OneBay_WIP%>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(TN.spme = mean(TN.spme, na.rm = T), 
            TP.spme = mean(TP.spme), 
            Chla.spme = mean(Chla.spme), 
            Sal.spme = mean(Sal.spme))

Temp.Change = 
  ggplot(data = CC.temp) + 
  geom_line(aes(x = year, y = Temp.sumy1med, group = simnum_OB), alpha = .3, color = "darkorchid") +
  stat_summary(aes(x = year, y = Temp.sumy1med), geom = "line", 
               size = 3, color = "black") +
  stat_summary(data = SAVWQallClean %>% filter(SpCluster == "Zostera") %>% group_by(year) %>% 
                 summarize(Temp = mean(Temp.sumy1med, na.rm = T)),
               aes(x = year, y = Temp), geom = "line", 
               size = 3, color = "brown") +
  geom_smooth(data = SAVWQallClean %>% filter(SpCluster == "Zostera") %>% group_by(year) %>% 
                summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
              method = "lm",
              aes(x = year, y = Temp), alpha = .3, color = "black") +
  theme_bw(base_size=34) + 
  labs(y = expression(paste("Low Bay Summer Temp (mean", degree ~ C, ")")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Temp.Change


TN.Change = 
  ggplot(data = CC.TN) + 
  geom_line(aes(x = year, y = TN.spme, group = simnum_OB), alpha = .3, color = "darkorchid") +
  geom_line(data = WIP.TN, aes(x = year, y = TN.spme, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = TN.spme), geom = "line", 
               size = 3, color = "black") +
  stat_summary(data = WIP.TN, aes(x = year, y = TN.spme), geom = "line", 
               size = 3, color = "black") +
  stat_summary(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia"),
               aes(x = year, y = TN.spme), geom = "line", 
               size = 3, color = "black") +
  geom_smooth(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia") %>% group_by(year) %>% 
                summarize(TN = mean(TN.spme, na.rm = T)),
              method = "lm",
              aes(x = year, y = TN), alpha = .3, color = "cyan3") +
   geom_labelsmooth(data = CC.TN, label = "No Act",
                    aes(x = year, y = TN.spme), color = "darkorchid",
                    method = "gam", boxlinewidth = 0, size = 8) +
  geom_labelsmooth(data = WIP.TN, label = "Nut Reduct",
                   aes(x = year, y = TN.spme), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 8) +
  theme_bw(base_size=32) + 
  labs(y = expression(paste("Midbay Spring TN (mean)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
TN.Change

TP.Change = 
  ggplot(data = CC.TN) + 
  geom_line(aes(x = year, y = TP.spme, group = simnum_OB), alpha = .3, color = "darkorchid") +
  geom_line(data = WIP.TN, aes(x = year, y = TP.spme, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = TP.spme), geom = "line", 
               size = 3, color = "black") +
  stat_summary(data = WIP.TN, aes(x = year, y = TP.spme), geom = "line", 
               size = 3, color = "black") +
  stat_summary(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia"),
               aes(x = year, y = TP.spme), geom = "line", 
               size = 3, color = "black") +
  geom_smooth(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia") %>% group_by(year) %>% 
                summarize(TP = mean(TP.spme, na.rm = T)),
              method = "lm",
              aes(x = year, y = TP), alpha = .3, color = "cyan3") +
  geom_labelsmooth(data = CC.TN, label = "No Act",
                   aes(x = year, y = TP.spme), color = "darkorchid",
                   method = "gam", boxlinewidth = 0, size = 8) +
  geom_labelsmooth(data = WIP.TN, label = "Nut Reduct",
                   aes(x = year, y = TP.spme), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 8) +
  theme_bw(base_size=32) + 
  labs(y = expression(paste("Midbay Spring TP (mean)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
TP.Change

Chla.Change = 
  ggplot(data = CC.TN) + 
  geom_line(aes(x = year, y = Chla.spme, group = simnum_OB), alpha = .3, color = "darkorchid") +
  geom_line(data = WIP.TN, aes(x = year, y =Chla.spme, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = Chla.spme), geom = "line", 
               size = 3, color = "darkorchid") +
  stat_summary(data = WIP.TN, aes(x = year, y = Chla.spme), geom = "line", 
               size = 3, color = "blue4") +
  stat_summary(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia"),
               aes(x = year, y = Chla.spme), geom = "line", 
               size = 3, color = "black") +
  geom_smooth(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia") %>% group_by(year) %>% 
                summarize(Chla = mean(Chla.spme, na.rm = T)),
              method = "lm",
              aes(x = year, y = Chla), alpha = .3, color = "cyan3") +
  geom_labelsmooth(data = CC.TN, label = "No Act",
                   aes(x = year, y = Chla.spme), color = "darkorchid",
                   method = "gam", boxlinewidth = 0, size = 8) +
  geom_labelsmooth(data = WIP.TN, label = "Nut Reduct",
                   aes(x = year, y = Chla.spme), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 8) +
  theme_bw(base_size=32) + 
  labs(y = expression(paste("Midbay Spring Chla (mean)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Chla.Change

Sal.Change = 
  ggplot(data = CC.TN) + 
  geom_line(aes(x = year, y = Sal.spme, group = simnum_OB), alpha = .3, color = "darkorchid") +
  geom_line(data = WIP.TN, aes(x = year, y =Sal.spme, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = Sal.spme), geom = "line", 
               size = 3, color = "darkorchid") +
  stat_summary(data = WIP.TN, aes(x = year, y = Sal.spme), geom = "line", 
               size = 3, color = "blue4") +
  stat_summary(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia"),
               aes(x = year, y = Sal.spme), geom = "line", 
               size = 3, color = "black") +
  geom_smooth(data = SAVWQallClean_85 %>% filter(SpCluster == "Ruppia") %>% group_by(year) %>% 
                summarize(Sal = mean(Sal.spme, na.rm = T)),
              method = "lm",
              aes(x = year, y = Sal), alpha = .3, color = "cyan3") +
  geom_labelsmooth(data = CC.TN, label = "No Act",
                   aes(x = year, y = Sal.spme), color = "darkorchid",
                   method = "gam", boxlinewidth = 0, size = 8) +
  geom_labelsmooth(data = WIP.TN, label = "Nut Reduct",
                   aes(x = year, y = Sal.spme), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 8) +
  theme_bw(base_size=32) + 
  labs(y = expression(paste("Midbay Spring Sal (mean)")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Sal.Change


#summary WQ DFs by year and zone for env vars. Not as good as above####
Temp = Zo_WIP.PredOneTrueBae %>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(Temp.sumy1med = mean(Temp.sumy1med, na.rm = T), 
            Temp.spmed = mean(Temp.spmed))
TNWIP = Ru_WIP.PredOneTrueBae%>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(TN.spme = mean(TN.spme, na.rm = T), 
            TP.spme = mean(TP.spme), 
            Chla.spme = mean(Chla.spme))
TNCC = Ru_CC.PredOneTrueBae %>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(TN.spme = mean(TN.spme, na.rm = T), 
            TP.spme = mean(TP.spme), 
            Chla.spme = mean(Chla.spme))
TNWIP_F = F_WIP.PredOneTrueBae %>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(TN.summe = mean(TN.summe, na.rm = T), 
            TP.summe = mean(TP.summe), 
            Chla.summe = mean(Chla.summe), 
            Sal.summe = mean(Sal.summe))
TNCC_F = F_CC.PredOneTrueBae %>% 
  filter(!year %in% c(2020)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(TN.summe = mean(TN.summe, na.rm = T), 
            TP.summe = mean(TP.summe), 
            Chla.summe = mean(Chla.summe), 
            Sal.summe = mean(Sal.summe))

#Temperature Change over time (Zostera zone)####
Temp.Zo_allScenarios = 
  ggplot(data = Temp) + 
  geom_line(aes(x = year, y = Temp.sumy1med, group = simnum_OB), alpha = .3, color = "coral1") +
  stat_summary(aes(x = year, y = Temp.sumy1med), geom = "line", 
              size = 3, color = "darkslateblue") +
  stat_summary(data = SAVWQallClean %>% filter(SpCluster == "Zostera") %>% group_by(year) %>% 
                 summarize(Temp = mean(Temp.sumy1med, na.rm = T)),
               aes(x = year, y = Temp), geom = "line", 
               size = 3, color = "brown") +
  geom_smooth(data = SAVWQallClean %>% filter(SpCluster == "Zostera") %>% group_by(year) %>% 
                summarize(Temp = mean(Temp.sumy1med, na.rm = T)), 
              method = "lm",
              aes(x = year, y = Temp), alpha = .3, color = "black") +
  theme_bw(base_size=38) + 
  labs(y = expression(paste("mean summer Temperature (", degree ~ C, ")")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Temp.Zo_allScenarios

#TN Ru zone####
TN.Ru_bothScens = 
  ggplot(data = TNWIP) + 
  geom_line(aes(x = year, y = TN.spme, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = TN.spme), geom = "line", 
               size = 3, color = "brown3") +
 # geom_line(data = TNCC, aes(x = year, y = TN.spme, group = simnum_OB), alpha = .3, color = "deepskyblue3") +
 # stat_summary(data = TNCC, aes(x = year, y = TN.spme), geom = "line", 
  #             size = 3, color = "burlywood") +
  stat_summary(data = SAVWQallClean %>% filter(SpCluster == "Ruppia"),
               aes(x = year, y = TN.spme), geom = "line", 
               size = 3, color = "cyan3") +
  geom_smooth(data = SAVWQallClean %>% filter(SpCluster == "Ruppia") %>% group_by(year) %>% 
                summarize(TN = mean(TN.spme, na.rm = T)),
              method = "lm",
              aes(x = year, y = TN), alpha = .3, color = "black") +
 # geom_labelsmooth(data = TNCC, label = "No Act",
 #                  aes(x = year, y = TN.spme), color = "deepskyblue",
 #                  method = "gam", boxlinewidth = 0, size = 8) +
  geom_labelsmooth(data = TNWIP, label = "Nut Reduct",
                   aes(x = year, y = TN.spme), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 8) +
  theme_bw(base_size=38) + 
  labs(y = expression(paste("mean spring N")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
TN.Ru_bothScens

#TP Ru Zone####
TP.Ru_bothScens = 
  ggplot(data = TNWIP) + 
  geom_line(aes(x = year, y = TP.spme, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = TP.spme), geom = "line", 
               size = 3, color = "brown3") +
  geom_line(data = TNCC, aes(x = year, y = TP.spme, group = simnum_OB), alpha = .3, color = "deepskyblue3") +
  stat_summary(data = TNCC, aes(x = year, y = TP.spme), geom = "line", 
               size = 3, color = "burlywood") +
  geom_line(data = RuDensWQsem.No0_NEW %>% group_by(year) %>% 
              summarize(TP = mean(TP.spme, na.rm = T)),
            aes(x = year, y = TP), alpha = .3, color = "cyan3") +
  stat_summary(data = RuDensWQsem.No0_NEW,
               aes(x = year, y = TP.spme), geom = "line", 
               size = 3, color = "black") +
  geom_labelsmooth(data = TNCC, label = "No Act",
                   aes(x = year, y = TP.spme), color = "deepskyblue",
                   method = "gam", boxlinewidth = 0, size = 3) +
  geom_labelsmooth(data = TNWIP, label = "Nut Reduct",
                   aes(x = year, y = TP.spme), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 3) +
  theme_bw(base_size=30) + 
  labs(y = expression(paste("mean spring P")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
TP.Ru_bothScens




#Sal F Zone####
Sal.F_bothScens = 
  ggplot(data = TNWIP_F) + 
  geom_line(aes(x = year, y = Sal.summe, group = simnum_OB), alpha = .3, color = "cyan2") +
  stat_summary(aes(x = year, y = Sal.summe), geom = "line", 
               size = 3, color = "brown3") +
  geom_line(data = TNCC_F, aes(x = year, y = Sal.summe, group = simnum_OB), alpha = .3, color = "deepskyblue3") +
  stat_summary(data = TNCC_F, aes(x = year, y = Sal.summe), geom = "line", 
               size = 3, color = "burlywood") +
  geom_line(data = SAVCommDensWQ_69sem.No0 %>% filter(SpCluster == "Fresh") %>% group_by(year) %>% 
              summarize(Sal.summe = mean(Sal.summe, na.rm = T)),
            aes(x = year, y = Sal.summe), alpha = .3, color = "cyan3") +
  stat_summary(data = SAVCommDensWQ_69sem.No0 %>% filter(SpCluster == "Fresh"),
               aes(x = year, y = Sal.summe), geom = "line", 
               size = 3, color = "black") +
  geom_labelsmooth(data = TNCC_F, label = "No Act",
                   aes(x = year, y = Sal.summe), color = "deepskyblue",
                   method = "gam", boxlinewidth = 0, size = 3) +
  geom_labelsmooth(data = TNWIP_F, label = "Nut Reduct",
                   aes(x = year, y = Sal.summe), color = "cyan4",
                   method = "gam", boxlinewidth = 0, size = 3) +
  theme_bw(base_size=30) + 
  labs(y = expression(paste("mean summer Salinity")), x = "") +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.25, 1, .25, 1), "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")
Sal.F_bothScens

#Viz Station Area over time####
#all zos stations, example
Zo_CC.wland_PredictONEBAY %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(TN = mean(TN.spme), Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION)) + 
  geom_line(data = SAVCommDensWQ_ForPred %>% filter(SpCluster == "Zostera") %>%
              mutate(Area = predict(dwm.to.HA_Zo, newdata = .)),
            aes(x = year, y = Area, group = STATION, color = STATION)) +
  labs(y = expression(paste("SAV Area\n (HA/Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

Zo_CC.PredOneTrueBae %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(TN = mean(TN.spme), Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION)) + 
  geom_line(data = SAVCommDensWQ_ForPred %>% filter(SpCluster == "Zostera") %>%
              mutate(Area = predict(dwm.to.HA_Zo, newdata = .)),
            aes(x = year, y = Area, group = STATION, color = STATION)) +
  labs(y = expression(paste("SAV Area\n (HA/Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

Zo_CC.PredOneTrueBaeSLR %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(TN = mean(TN.spme), Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION)) + 
  geom_line(data = SAVCommDensWQ_ForPred %>% filter(SpCluster == "Zostera") %>%
              mutate(Area = predict(dwm.to.HA_Zo, newdata = .)),
            aes(x = year, y = Area, group = STATION, color = STATION)) +
  labs(y = expression(paste("SAV Area\n (HA/Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

#allRuStations
Ru_WIP.wland_PredictONEBAY %>% #filter(simnum == 69) %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(TN = mean(TN.spme), Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION)) + 
  geom_line(data = SAVCommDensWQ_ForPred %>% filter(SpCluster == "Ruppia") %>%
              mutate(Area = predict(dwm.to.HA_Ru, newdata = .)),
            aes(x = year, y = Area, group = STATION, color = STATION)) +
  labs(y = expression(paste("SAV Area\n (HA/Ru Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

Ru_WIP.PredOneTrueBae %>% filter(simnum_OB == 69) %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(TN = mean(TN.spme), Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION)) + 
  geom_line(data = SAVCommDensWQ_ForPred %>% filter(SpCluster == "Ruppia") %>%
              mutate(Area = predict(dwm.to.HA_Ru, newdata = .)),
            aes(x = year, y = Area, group = STATION, color = STATION)) +
  labs(y = expression(paste("SAV Area\n (HA/Ru Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

Ru_WIP.PredOneTrueBaeSLR %>% filter(simnum_OB == 69) %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(TN = mean(TN.spme), Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION)) + 
  geom_line(data = SAVCommDensWQ_ForPred %>% filter(SpCluster == "Ruppia") %>%
              mutate(Area = predict(dwm.to.HA_Ru, newdata = .)),
            aes(x = year, y = Area, group = STATION, color = STATION)) +
  labs(y = expression(paste("SAV Area\n (HA/Ru Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

F_WIP.PredOneTrueBae %>% filter(STATION == "CB1.1") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(STATION, year) %>%
  summarize(Area = mean(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = STATION, color = STATION))

F_WIP.PredOneTrueBae %>% #filter(STATION == "CB1.1") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  group_by(year, simnum_OB) %>%
  summarize(Area = sum(Area), dwm = mean(dens.weight.mean)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area, group = simnum_OB))

#Proportion time####
pastA = allcomm_past.df %>% group_by(year) %>% summarize(A = sum(Area.total))
past_Prop = allcomm_past.df %>% group_by(year, SpCluster) %>% 
  summarize(Prop = Area.total/pastA$A)
#futA = allcomm_WIP.wl.dfONE %>% group_by(year, simnum_OB) %>% summarize(A = sum(Area.total))


ggplot(data = past_Prop) + 
  stat_summary(data = past_Prop, 
               aes(x = year, y = Prop, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8)+ 
  geom_labelsmooth(data = past_Prop, 
                   aes(x = year, y = Prop, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
  labs(y = "Proportion of Bay", x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
 # scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")



#Flats
FWIP.wl_FLATS = 
  F_WIP.wland_Predict %>% filter(STATION == "CB1.1") %>% 
  filter(!year %in% c("2020", "2021")) %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  group_by(year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  ggplot(data = .) +
  geom_line(aes(x = year, y = Area.total, group = simnum_OB)) + 
  geom_line(data = allcomm_past.df %>% filter(Station == "CB1.1"), 
            aes(x = year, y = Area.total)) +
  labs(y = expression(paste("Flats Area\n (HA/Station)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  #scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
FWIP.wl_FLATS

#####GOOD GRAPHS HERE!!!!!!!!#####
#Nutrient Reduction, All communities, Arae####
NutRed_Allcomm.Area = 
  ggplot(data = allcomm_past.df) + 
    stat_summary(data = allcomm_past.df, #%>% filter(!SpCluster == "MixedMeso"), 
                 aes(x = year, y = Area.total, group = SpCluster, color = SpCluster), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 3, alpha = .8) +
    stat_summary(data = allcomm_WIP.wl.dfONE, 
                 aes(x = year, y = Area.total, group = SpCluster, color = SpCluster), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 3, alpha = .8) +
    geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "Zostera"),
              aes(x = year, y = Area.total, group = simnum), 
              size = .1, alpha = .6, color = "chartreuse2") +
    geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "Ruppia"),
              aes(x = year, y = Area.total, group = simnum), 
              size = .1, alpha = .6, color = "brown2") +
    geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "MixedMeso"),
              aes(x = year, y = Area.total, group = simnum),  
              size = .1, alpha = .6, color = "darkorchid") +
    geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "Fresh"),
              aes(x = year, y = Area.total, group = simnum),
              size = .1, alpha = .6, color = "darkgoldenrod1") +
    geom_labelsmooth(data = allcomm_WIP.wl.dfONE, 
                     aes(x = year, y = Area.total, color = SpCluster, label = SpCluster),
                     method = "gam", boxlinewidth = 0, size = 5) +
  geom_text(aes(x = 2020, y = 35000, label = "Nutrient Reduction"), stat = "unique", size = 12, color = "blue4") +
    labs(y = expression(paste("SAV Area, Nut Reduction \n (HA/Community)")), x = "") +
    theme_bw(base_size=24) + 
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
    scale_y_continuous(limits = c(0, 40000), breaks= c(0, 10000, 20000, 30000, 40000)) +
    scale_color_manual(values = c("darkgoldenrod1", "darkorchid","brown2", "chartreuse2")) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
NutRed_Allcomm.Area

aniNut= 
  ggplot(data = allcomm_WIP.wl.dfONE) + 
  stat_summary(data = allcomm_WIP.wl.dfONE, 
               aes(x = year, y = Area.total, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  labs(y = expression(paste("SAV Area\n (HA/Community)")), x = "") +
  theme_bw(base_size=24) + 
  #scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  scale_color_manual(values = c("darkgoldenrod1", "brown2",  "chartreuse2")) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
aniNut
library(gganimate)
#aN = aniNut  + transition_reveal(year)


#Nutrient Reduction, All communities, DWM####
NutRed_Allcomm.DWM = 
ggplot(data = allcomm_past.df) + 
  stat_summary(data = allcomm_past.df, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  stat_summary(data = allcomm_WIP.wl.dfONE, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "Zostera"),
            aes(x = year, y = DWM.total, group = simnum), 
            size = .1, alpha = .4, color = "chartreuse2") +
  geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "Ruppia"),
            aes(x = year, y = DWM.total, group = simnum), 
            size = .1, alpha = .4, color = "brown2") +
  geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "MixedMeso"),
            aes(x = year, y = DWM.total, group = simnum),  
            size = .1, alpha = .4, color = "darkorchid") +
  geom_line(data = allcomm_WIP.wl.dfONE %>% filter(SpCluster == "Fresh"),
            aes(x = year, y = DWM.total, group = simnum),
            size = .1, alpha = .4, color = "darkgoldenrod") +
  geom_labelsmooth(data = allcomm_WIP.wl.dfONE, 
                   aes(x = year, y = DWM.total, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
  labs(y = expression(paste("SAV DW Area, Nut Reduction\n (HA/Community)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
#  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
NutRed_Allcomm.DWM



#No Action, All communities, Arae####
NoAct_Allcomm.Area = 
  ggplot(data = allcomm_past.df) + 
  stat_summary(data = allcomm_past.df, 
               aes(x = year, y = Area.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  stat_summary(data = allcomm_CC.dfONE, 
               aes(x = year, y = Area.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "Zostera"),
            aes(x = year, y = Area.total, group = simnum_OB), 
            size = .1, alpha = .4, color = "chartreuse2") +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "Ruppia"),
            aes(x = year, y = Area.total, group = simnum_OB), 
            size = .1, alpha = .4, color = "brown2") +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "MixedMeso"),
            aes(x = year, y = Area.total, group = simnum_OB),  
            size = .1, alpha = .4, color = "darkorchid") +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "Fresh"),
            aes(x = year, y = Area.total, group = simnum_OB),
            size = .1, alpha = .6, color = "darkgoldenrod4") +
  geom_labelsmooth(data = allcomm_CC.dfONE, 
                   aes(x = year, y = Area.total, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
  geom_text(aes(x = 2010, y = 30000, label = "No Action"), stat = "unique", size = 12, color = "deeppink") +
  labs(y = expression(paste("SAV Area, No Action\n (HA/Community)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_y_continuous(limits = c(0, 40000), breaks= c(0, 10000, 20000, 30000, 40000)) +
  scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
NoAct_Allcomm.Area

#No Action, All communities, DWM####
NoAct_Allcomm.DWM = 
  ggplot(data = allcomm_past.df) + 
  stat_summary(data = allcomm_past.df, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  stat_summary(data = allcomm_CC.dfONE, 
               aes(x = year, y = DWM.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "Zostera"),
            aes(x = year, y = DWM.total, group = simnum_OB), 
            size = .1, alpha = .4, color = "chartreuse2") +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "Ruppia"),
            aes(x = year, y = DWM.total, group = simnum_OB), 
            size = .1, alpha = .4, color = "brown2") +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "MixedMeso"),
            aes(x = year, y = DWM.total, group = simnum_OB),  
            size = .1, alpha = .4, color = "darkorchid") +
  geom_line(data = allcomm_CC.dfONE %>% filter(SpCluster == "Fresh"),
            aes(x = year, y = DWM.total, group = simnum_OB),
            size = .1, alpha = .4, color = "darkgoldenrod") +
  geom_labelsmooth(data = allcomm_CC.dfONE, 
                   aes(x = year, y = DWM.total, color = SpCluster, label = SpCluster),
                   method = "gam", boxlinewidth = 0, size = 5) +
  labs(y = expression(paste("SAV DWAream No Action \n (DWM/Community)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_y_continuous(breaks = seq(0, 25000, 2500)) +
  scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
NoAct_Allcomm.DWM

####Patch All Community On one Total Area Graphs####
AllComm_HA = NoAct_Allcomm.Area / NutRed_Allcomm.Area
AllComm_HA
  
#Past SAV Area by community####
ggplot(data = allcomm_past.df) + 
  stat_summary(data = allcomm_past.df, 
               aes(x = year, y = Area.total, group = SpCluster, color = SpCluster), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 3, alpha = .8) + 
  geom_labelline(data = allcomm_past.df, 
                   aes(x = year, y = Area.total, color = SpCluster, label = SpCluster),
                   boxlinewidth = 0, size = 5, 
                  hjust = "ymax", text_smoothing = 30) +
  labs(y = expression(paste("SAV Area\n (DWM/Community)")), x = "") +
  theme_bw(base_size=24) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  scale_color_manual(values = colorme) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")

#SummarizeWholeBay Together####
sumBaeCC = allcomm_CC.dfONE %>% 
  group_by(year, SpCluster) %>%
  summarize(Areamax = max(Area.total, na.rm = T), 
            Areamin = min(Area.total, na.rm = T), 
            Areamean = mean(Area.total, na.rm = T)) %>%
  group_by(year) %>%
  summarize(AreaMAX = sum(Areamax), 
            AreaMIN = sum(Areamin), 
            AreaMEAN = sum(Areamean))
sumBaeWIP = allcomm_WIP.wl.dfONE %>% 
  group_by(year, SpCluster) %>%
  summarize(Areamax = max(Area.total, na.rm = T), 
            Areamin = min(Area.total, na.rm = T), 
            Areamean = mean(Area.total, na.rm = T)) %>%
  group_by(year) %>%
  summarize(AreaMAX = sum(Areamax), 
            AreaMIN = sum(Areamin), 
            AreaMEAN = sum(Areamean))

fullBaypast = allcomm_past.df %>%
  group_by(year) %>%
  summarize(DWM_BAE = sum(DWM.total, na.rm = T), 
            Area_BAE = sum(Area.total, na.rm = T))

TotalBayCC = 
ggplot(data = fullBaypast) + 
  stat_summary(data = fullBaypast, 
               aes(x = year, y = Area_BAE), 
               fun.data = mean_se, geom = "line",  
               size = 4) +
  stat_summary(data = sumBaeCC, 
               aes(x = year, y = AreaMAX), 
               fun.data = mean_se, geom = "line",  
               size = 4, color = "deeppink") +
  stat_summary(data = sumBaeCC, 
               aes(x = year, y = AreaMEAN), 
               fun.data = mean_se, geom = "line", 
               size = 2, color = "black") +
  stat_summary(data = sumBaeCC, 
               aes(x = year, y = AreaMIN), 
               fun.data = mean_se, geom = "line", 
               size = 4, color = "deeppink") +
  geom_labelsmooth(data = sumBaeCC, label = "Max Area",
                   aes(x = year, y = AreaMAX), color = "black",
                   method = "gam", boxlinewidth = 0, size = 8) +
  geom_labelsmooth(data = sumBaeCC, label = "Min Area",
                   aes(x = year, y = AreaMIN), color = "black",
                   method = "gam", boxlinewidth = 0, size = 8) +
  geom_ribbon(data = sumBaeCC, 
              aes(x = year, ymin = AreaMEAN - (sd(AreaMEAN)*3), ymax = AreaMEAN + (sd(AreaMEAN)*3)), 
              fill = "deeppink", alpha = .3) +
  geom_hline(yintercept = 74000) +
  geom_text(aes(x = 2010, y = 55000, label = "No Action"), stat = "unique", size = 12, color = "deeppink") +
  geom_text(aes(x = 2020, y = 72000, label = "Baywide SAV Goal (74,000 HA)"), stat = "unique", size = 8, color = "blue") +
  labs(y = "Total SAV area (HA)", x = "") +
  theme_bw(base_size=22) + 
  ylim(0, 78000) +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  # scale_color_manual(values = scen_colors) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
TotalBayCC

TotalBayWIP = 
ggplot(data = fullBaypast) + 
  stat_summary(data = fullBaypast, 
               aes(x = year, y = Area_BAE), 
               fun.data = mean_se, geom = "line",  
               size = 4) +
  stat_summary(data = sumBaeWIP, 
               aes(x = year, y = AreaMAX), 
               fun.data = mean_se, geom = "line",  
               size = 4, color = "darkgreen") +
  stat_summary(data = sumBaeWIP, 
               aes(x = year, y = AreaMEAN), 
               fun.data = mean_se, geom = "line", 
               size = 2, color = "black") +
  stat_summary(data = sumBaeWIP, 
               aes(x = year, y = AreaMIN), 
               fun.data = mean_se, geom = "line", 
               size = 4, color = "darkgreen") +
  geom_labelsmooth(data = sumBaeWIP, label = "Max Area",
                   aes(x = year, y = AreaMAX), color = "black",
                   method = "gam", boxlinewidth = 0, size = 9) +
  geom_labelsmooth(data = sumBaeWIP, label = "Min Area",
                   aes(x = year, y = AreaMIN), color = "black",
                   method = "gam", boxlinewidth = 0, size = 9) +
  geom_ribbon(data = sumBaeWIP, 
              aes(x = year, ymin = AreaMEAN - (sd(AreaMEAN)*3), ymax = AreaMEAN + (sd(AreaMEAN)*3)), 
              fill = "darkgreen", alpha = .3) +
  geom_hline(yintercept = 74000) +
  geom_text(aes(x = 2010, y = 58000, label = "Nutrient Reductions"), stat = "unique", size = 10, color = "darkgreen") +
  geom_text(aes(x = 2020, y = 72000, label = "Baywide SAV Goal (74,000 HA)"), stat = "unique", size = 8, color = "blue") +
  labs(y = "Total SAV area (HA)", x = "") +
  theme_bw(base_size=22) + 
  ylim(0, 78000) +
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  # scale_color_manual(values = scen_colors) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
TotalBayWIP

#ONEBAY both scenarios####
TotalBay = TotalBayCC / TotalBayWIP
TotalBay

#ONEBAY w ribbons for error, maxmin on same graph####
ggplot(data = fullBaypast) + 
  stat_summary(data = fullBaypast, 
               aes(x = year, y = Area_BAE), 
               fun.data = mean_se, geom = "line",  
               size = 4) +
  geom_ribbon(data = sumBaeWIP, 
              aes(x = year, ymin = AreaMEAN - (sd(AreaMEAN)*3), ymax = AreaMEAN + (sd(AreaMEAN)*3)), 
              fill = "darkgreen", alpha = .7) +
  geom_ribbon(data = sumBaeCC, 
              aes(x = year, ymin = AreaMEAN - (sd(AreaMEAN)*3), ymax = AreaMEAN + (sd(AreaMEAN)*3)), 
              fill = "deeppink", alpha = .7) +
  stat_summary(data = sumBaeWIP, 
               aes(x = year, y = AreaMAX), 
               fun.data = mean_se, geom = "line",  
               size = 4, color = "darkgreen") +
  stat_summary(data = sumBaeWIP, 
               aes(x = year, y = AreaMIN), 
               fun.data = mean_se, geom = "line", 
               size = 4, color = "darkgreen") +
  stat_summary(data = sumBaeCC, 
               aes(x = year, y = AreaMAX), 
               fun.data = mean_se, geom = "line",  
               size = 4, color = "deeppink") +
  stat_summary(data = sumBaeCC, 
               aes(x = year, y = AreaMIN), 
               fun.data = mean_se, geom = "line", 
               size = 4, color = "deeppink") +
  geom_labelsmooth(data = sumBaeWIP, label = "Nut Reduce",
                   aes(x = year, y = AreaMEAN), color = "darkgreen",
                   method = "lm", boxlinewidth = 0, size = 5) +
  geom_labelsmooth(data = sumBaeCC, label = "No Action",
                   aes(x = year, y = AreaMEAN), color = "deeppink",
                   method = "lm", boxlinewidth = 0, size = 5) +
  geom_text(aes(x = 2020, y = 72000, label = "Baywide SAV Goal (74,000 HA)"), stat = "unique", size = 8, color = "blue") +
  labs(y = "Total SAV area (HA)", x = "") +
  theme_bw(base_size=42) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")


#FullbayScen, simnum issue####
#grouping by simnum doesnt really work here, remember
fullBaymeanCC = allcomm_CC.dfONE %>% 
  group_by(year, simnum) %>%
  summarize(DWM_BAE = sum(DWM.total, na.rm = T), 
            Area_BAE = sum(Area.total, na.rm = T))
fullBaymeanWIP = allcomm_WIP.wl.dfONE %>% 
  group_by(year, simnum) %>%
  summarize(DWM_BAE = sum(DWM.total, na.rm = T), 
            Area_BAE = sum(Area.total, na.rm = T))
fullBaypast = allcomm_past.df %>%
  group_by(year) %>%
  summarize(DWM_BAE = sum(DWM.total, na.rm = T), 
            Area_BAE = sum(Area.total, na.rm = T))
  


fullBay.Scenario =
    ggplot(data = fullBaypast) + 
    stat_summary(data = fullBaypast, 
                 aes(x = year, y = Area_BAE), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 4) +
    stat_summary(data = fullBaymeanCC, 
                 aes(x = year, y = Area_BAE), 
                 fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
                 size = 4, color = "deeppink") +
  stat_summary(data = fullBaymeanWIP , 
               aes(x = year, y = Area_BAE), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 4, color = "darkgreen") +
  geom_smooth(data = fullBaymeanCC,
            aes(x = year, y = Area_BAE), 
            size = .3, alpha = .2, color = "deeppink") +
  geom_smooth(data = fullBaymeanWIP,
            aes(x = year, y = Area_BAE), 
            size = .3, alpha = .2, color = "darkgreen") +
    geom_line(data = fullBaymeanCC,
              aes(x = year, y = Area_BAE, group = simnum), 
              se = T, size = .3, alpha = .2, color = "deeppink") +
    geom_line(data = fullBaymeanWIP,
              aes(x = year, y = Area_BAE, group = simnum), 
              se = T, size = .3, alpha = .2, color = "darkgreen") +
  geom_labelsmooth(data = fullBaymeanCC, label = "No Act",
                   aes(x = year, y = Area_BAE), color = "deeppink",
                   method = "gam", boxlinewidth = 0, size = 4) +
    geom_labelsmooth(data = fullBaymeanWIP, label = "Nut Reduct",
                     aes(x = year, y = Area_BAE), color = "darkgreen",
                     method = "gam", boxlinewidth = 0, size = 8) +
    geom_hline(yintercept = 74000) +
    geom_text(aes(x = 2020, y = 72000, label = "Baywide SAV Goal (74,000 HA)"), stat = "unique", size = 5, color = "blue") +
    labs(y = "Total SAV area (HA)", x = "") +
    theme_bw(base_size=42) + 
    ylim(0, 78000) +
    scale_x_continuous(breaks=seq(1980, 2070, 10)) +
   # scale_color_manual(values = scen_colors) +
    theme(plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
  fullBay.Scenario
  
  

#Basin data####
seg_info = readxl::read_excel("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Stations_byBasinGroup.xlsx", col_names = T, skip = 1)

Basins = seg_info %>% select(station, latitude, longitude, BasinSummaryGroup, waterbody) %>%
  rename(STATION = station, BASIN = BasinSummaryGroup)

ZCCbasin = Zo_CC.PredOneTrueBae %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  add_column(., SpCluster = "Zostera")

RCCbasin = Ru_CC.PredOneTrueBae %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
MCCbasin = MM_CC.PredOneTrueBae %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
FCCbasin = F_CC.PredOneTrueBae %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

allcommbasin_CC.df = ZCCbasin %>% bind_rows(RCCbasin) %>% bind_rows(MCCbasin) %>% 
  bind_rows(FCCbasin) %>% filter(!year %in% c("2020")) %>% ungroup()

ZWIPbasin = Zo_WIP.PredOneTrueBae %>% 
  mutate(Area = predict(dwm.to.HA_Zo, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  add_column(., SpCluster = "Zostera")

RWIPbasin = Ru_WIP.PredOneTrueBae %>% 
  add_column(., SpCluster = "Ruppia") %>%
  mutate(Area = predict(dwm.to.HA_Ru, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
MWIPbasin = MM_WIP.PredOneTrueBae %>% 
  add_column(., SpCluster = "MixedMeso") %>%
  mutate(Area = predict(dwm.to.HA_MM, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))
FWIPbasin = F_WIP.PredOneTrueBae %>% 
  add_column(., SpCluster = "Fresh") %>%
  mutate(Area = predict(dwm.to.HA_F, newdata = .)) %>%
  left_join(Basins, by = c("STATION")) %>%
  group_by(BASIN, year, simnum_OB) %>% 
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(Area, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T))

allcommbasin_WIP.df = ZWIPbasin %>% bind_rows(RWIPbasin) %>% bind_rows(MWIPbasin) %>% 
  bind_rows(FWIPbasin) %>% filter(!year %in% c("2020")) %>% ungroup()


allcommbasin_past.df = SAVWQallClean %>% #rename(STATION = Station) %>% 
  left_join(Basins, by = c("STATION")) %>%
  group_by(SpCluster, BASIN, year) %>%
  summarize(DWM.total = sum(dens.weight.mean, na.rm = T), 
            Area.total = sum(SAVArea, na.rm = T), 
            DPC.mean = mean(dens.percomp, na.rm = T), 
            DPCC.mean = mean(dens.percomp.change, na.rm = T)) %>%
  filter(!SpCluster == "RuZo") %>% ungroup()

allcommbasin_byyear_CC = allcommbasin_CC.df %>% 
  add_column(., Scenario = "No Act") %>%
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
  group_by(Scenario, year, simnum_OB, BASIN) %>% 
  summarise(across(DWM.total:Area.total, ~ sum(.x, na.rm = TRUE)))

allcommbasin_byyear_WIPwl = allcommbasin_WIP.df %>% 
  add_column(., Scenario = "Nut Reduct") %>%
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
  group_by(Scenario, year, simnum_OB, BASIN) %>% 
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
  filter(!year %in% c(2020, 2021)) %>% #i have no fucking clue why this is a problem but whatever
  filter(!BASIN == "NA")

basinfuture = fullFutureSAVbasin.DF %>% group_by(Scenario, BASIN, year) %>%
  summarize(me.Area = mean(Area.total), sum.Area = sum(Area.total)) %>%
  summarize(Ame = mean(me.Area), Amax = max(me.Area))

fullBay.Scenariobasin =
  ggplot(data = fullFutureSAVbasin.DF) +
  geom_line(data = fullFutureSAVbasin.DF,
            aes(x = year, y = Area.total, group = simnum_OB, color = BASIN), 
            size = .3, alpha = .2) +
  stat_summary(data = allcombasinPastFull.DF, 
               aes(x = year, y = Area.total, group = BASIN, color = BASIN ), 
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 2) +
  stat_summary(data = fullFutureSAVbasin.DF, 
               aes(x = year, y = Area.total, group = BASIN, color = BASIN),
               fun.data = mean_se, geom = "line", fun.args = list(mult = 1), 
               size = 2) +
  facet_grid(BASIN~Scenario, scales = "free_y", space = "free") + 
  labs(y = "Total Baywide SAV area (HA)", x = "") +
  theme_bw(base_size=25) + 
  scale_x_continuous(breaks=seq(1980, 2070, 10)) +
  # scale_color_manual(values = scen_colors) +
  theme(#plot.margin = unit(c(.3, 1, .3, 1.5), "cm"), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
fullBay.Scenariobasin
