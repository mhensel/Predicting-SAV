#Chesapeake Bay Program Water Quality Station data 2020
#This file generates the master environmental database for 143 (?) WQ Stations throughougt all of the Chesapeake Bay. 

#Raw data by sampling date can be found CBP_WQ_2020.csv, EnvC.CBP_WQ is what i used for Ruppia analys.......update!

library(tidyverse); library(car); library(lme4)
library(readxl); library(lubridate); library(naniar)

####Load in datasets####
#load in the rda files from CBP WQ Stations up to 2020 
#from Marcs computer
load("~/Documents/R projects/Predicting-SAV/data/wtemp_sal_sec_chla_to2020.rda")
#from the R drive
load("/Volumes/savshare2/Current Projects/Predicting-SAV/data/wtemp_sal_sec_chla_to2020.rda")
# data1  contains fairly self-explanatory columns including DATE, STATION, PARAMETER and VALUE. The only processing I did of this set from the data hub was to average duplicates and remove a couple erroneous values

#from Marcs computer
load("~/Documents/R projects/Predicting-SAV/data/tn_tp_tss_to2020.rda")
#from the R drive
load("/Volumes/savshare2/Current Projects/Predicting-SAV/data/tn_tp_tss_to2019.rda")
#data2. This has fewer columns than the other because I pulled this from already processed data sets. Duplicates are averaged also.There are some cases of values reported at <DL or a range due to the components being <DL. Those will be reported here in the middle of the range (or ½ DL). Also, you’ll see the RAW_VALUE and FINAL_VALUE columns. FINAL_VALUE has the adjustments I mentioned in my email below – some data values cut-out, some adjusted due to method changes. All units of these parameters are mg/L. I recommend you use FINAL_VALUE column

#also load lat/longs
LatLongCBPWQ <- read_csv("~/Documents/R projects/Predicting-SAV/data/TidalLongTermStations.csv")

###format the dates so we can get month, day, year seperate (day prob isnt important but whatever)
#tbh joining LatLong now seems not worth it, bc have to take it out for summarize() later
CBPstationNPSS <- data2 %>% 
  mutate(date = ymd(DATE)) #%>% full_join(LatLongCBPWQ)
CBPstationNPSS$year <- year(CBPstationNPSS$date)
CBPstationNPSS$month <- month(CBPstationNPSS$date)

CBPstationWTSSC <- data1 %>% 
  mutate(date = ymd(DATE)) #%>% full_join(LatLongCBPWQ)
CBPstationWTSSC$year <- year(CBPstationWTSSC$date)
CBPstationWTSSC$month <- month(CBPstationWTSSC$date)

#CBPstation_all = full_join(CBPstationNPSS, CBPstationWTSSC)

#example####
env1 <- tibble(year = c(1999,1999,2000,2000,1999,1999,2000,2000),
               station = c(1, 2, 1, 2, 1, 2, 1, 2),
               measure = c("TN", "TN", "TN", "TN", "TP", "TP", "TP", "TP"),
                  val = c(2, 0, 15, 12, 8, 5, 4, 1))
env_wide <- pivot_wider(env1, names_from = "measure", values_from = "val")
#####

#####Create df for each WQ variable####
#TN####
CBPtn = CBPstationNPSS %>%
  unique() %>%
  filter(PARAMETER == "tn") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "FINAL_VALUE") %>%
  rename(TN = tn) %>% group_by(STATION, date, year, month) %>% 
  summarize(TN = mean(TN, na.rm = T))%>% ungroup() #some stations have 2 numbers, so take means (might only be CB7.4N)

#TN Notes: some stations were corrected, but FINAL_VALUE fixed it. 
colsums(is.na(CBPtn)) #NA: 8336 NA mostly in the 80s and early 90s
qplot(x = date, y = TN, data = CBPtn) #Outliers: 22 values over 6.
#CBPtn[!is.na(CBPtn$TN) & CBPtp=n$TN > 6, "TN"] <- 6

#TP####
CBPtp <- CBPstationNPSS %>%
  unique() %>%
  filter(PARAMETER == "tp") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "FINAL_VALUE") %>%
  rename(TP = tp) %>% group_by(STATION, date, year, month) %>% 
  summarize(TP = mean(TP, na.rm = T))%>% ungroup() #some dates have 2 numbers, so take means

#TP Notes:
colsums(is.na(CBPtp)) #NA: 932
qplot(x = date, y = TP, data = CBPtp) #Outliers: 8 points out of 70804 are above 1.
CBPtp[!is.na(CBPtp$TP) & CBPtp$TP > 0.6, "TP"] <- 0.6

#TSS####
CBPtss <- CBPstationNPSS %>%
  unique() %>%
  filter(PARAMETER == "tss") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "FINAL_VALUE") %>%
  rename(TSS = tss) %>% group_by(STATION, date, year, month) %>% 
  summarize(TSS = mean(TSS, na.rm = T))%>% ungroup() #some dates have 2 numbers, so take means

#creating a second DF that goes back to 84. 0 NAs
CBPtssr = CBPstationNPSS %>%
  unique() %>%
  filter(PARAMETER == "tss") %>%
  mutate(TSS= case_when(PARAMETER == "tss" ~ RAW_VALUE)) %>%
  group_by(STATION, date, year, month) %>% 
  summarize(TSS = mean(TSS, na.rm = T))%>% ungroup() #some dates have 2 numbers, so take means

#TSS Notes: 
#RAW_VALUE is same as FINAL_VALUE, F_V just starts after 1999. The following stations are safe to use R_V from: CB4.1E CB5.1 CB5.2CB5.3LE2.3EE1.1EE2.1EE2.2EE3.0EE3.1EE3.2EE3.3ET4.2ET5.2ET8.1ET9.1LE2.2RET2.4LE3.2LE3.3 LE3.4 CB7.3E 

colsums(is.na(CBPtss)) #NA: 30828, all before 99
qplot(x = date, y = TSS, data = CBPtss) #Outliers: ~20 negative values, and some gigantic ones too
CBPtss[!is.na(CBPtss$TSS) & CBPtss$TSS < 0, "TSS"] <- 0 #correct negative outliers

colsums(is.na(CBPtssr)) #NA: 0 outliers
qplot(x = date, y = TSS, data = CBPtssr) #Outliers: ~20 negative values, and 5 values over 500
CBPtssr[!is.na(CBPtssr$TSS) & CBPtssr$TSS < 0, "TSS"] <- 0 #correct negative outliers
CBPtssr[!is.na(CBPtssr$TSS) & CBPtssr$TSS > 400, "TSS"] <- 400 #correct big outliers

#Temp####
CBPtemp <- CBPstationWTSSC %>%
  unique() %>%
  filter(PARAMETER == "WTEMP") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "VALUE") %>%
  rename(Temp = WTEMP) %>% group_by(STATION, date, year, month) %>% 
  summarize(Temp = mean(Temp, na.rm = T))%>% ungroup() #summarize over DEPTH

#Temp Notes:
#DEPTH column has a handfull of 3s and 2s, then all 0,1,.5 but its not like there were multiples taken from diff depths so just means taken
colsums(is.na(CBPtemp)) #NA: 0
qplot(x = date, y = Temp, data = CBPtemp) #Outliers: None but 1984 and 1985 have some stations (eg WT2.1) where only winter months are measured, making any mean calculations really small
#CBPtp[!is.na(CBPtp$TP) & CBPtp$TP > 0.6, "TP"] <- 0.6

#Salinity####
CBPsal <- CBPstationWTSSC %>%
  unique() %>%
  filter(PARAMETER == "SALINITY") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "VALUE") %>%
  rename(Sal = SALINITY) %>% group_by(STATION, date, year, month) %>%
  summarize(Sal = mean(Sal, na.rm = T))%>% ungroup()

#Salinity Notes:
#DEPTH column has a handfull of 3s and 2s, then all 0,1,.5 but its not like there were multiples taken from diff depths so just ignore I think
colsums(is.na(CBPsal)) #NA: 0
qplot(x = date, y = Sal, data = CBPsal) #Outliers: None
#CBPtp[!is.na(CBPtp$TP) & CBPtp$TP > 0.6, "TP"] <- 0.6

#Secchi####
CBPsecc <- CBPstationWTSSC %>%
  unique() %>%
  filter(PARAMETER == "SECCHI") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "VALUE") %>%
  rename(Secc = SECCHI) %>% group_by(STATION, date, year, month) %>%
  summarize(Secc = mean(Secc, na.rm = T))%>% ungroup()

#SSecchi Notes:
#DEPTH column has a handfull of 3s and 2s, then all 0,1,.5 but its not like there were multiples taken from diff depths so just ignore I think
colsums(is.na(CBPsecc)) #NA: 0
qplot(x = date, y = Secc, data = CBPsecc) #Outliers: 8 points above 6 secchi, one above 10
CBPsecc[!is.na(CBPsecc$Secc) & CBPsecc$Secc > 6, "Secc"] <- 6

#ChlA####
CBPchla <- CBPstationWTSSC %>%
  unique() %>%
  filter(PARAMETER == "CHLA") %>%
  pivot_wider(names_from = "PARAMETER", values_from = "VALUE") %>%
  rename(Chla = CHLA) %>% group_by(STATION, date, year, month) %>%
  summarize(Chla = mean(Chla, na.rm = T)) %>% ungroup()

#Chla Notes:
#DEPTH column has a handfull of 3s and 2s, then all 0,1,.5 but its not like there were multiples taken from diff depths so just ignore I think
colsums(is.na(CBPchla)) #NA: 0
qplot(x = date, y = Chla, data = CBPchla) #Outliers: 1 points above 300 chla, two above 600
CBPchla[CBPchla$Chla > 250, "Chla"] <- 250 

#Master all CBP WQ dataframe####
CBPall <- full_join(CBPtn, CBPtp) %>% 
  full_join(CBPtssr) %>%
  full_join(CBPtemp) %>%
  full_join(CBPsal) %>%
  full_join(CBPsecc) %>%
  full_join(CBPchla) %>% #add the Delta Change metric. ChlA.d = change in ChlA between now and last sampling point.
  mutate(Chla.D = Chla - lag(Chla), Secc.D = Secc - lag(Secc), Sal.D = Sal - lag(Sal), Temp.D = Temp - lag(Temp), TP.D = TP - lag(TP), TN.D = TN - lag(TN), TSS.D = TSS - lag(TSS))

CBPnas= CBPall %>% group_by(STATION, year) %>% summarise_all(~sum(is.na(.)))
View(CBPnas)

#write it back into the R drive
write_csv(CBPall, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBPall_2020.csv")
write_csv(CBPall, "~/Documents/R projects/Predicting-SAV/data/CBPall_2020.csv")


#visualize one
ggplot(data = CBPall %>% filter(STATION == "CB5.1")) + 
  stat_summary(aes(x = date, y = Temp, color = Temp), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .4) +
  stat_summary(aes(x = date, y = Temp), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = .6, color = "black") +
  scale_color_gradient(low="blue", high="red") +
  #ylim(10,25) +
  ylab("Temperature (daily C)") + 
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "")


####summarizing Env Variables of interest MH######
CBPall= read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBPall_2020.csv")

#Springtime mean WQ DF CBP.WQ_spme ####
#(145 stations * 37 years = 5365 possible obs)
CBP.WQ_spme = CBPall %>% filter(between(month, 3, 5)) %>%
  group_by(year, STATION) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), 
            TSS.spme = mean(TSS, na.rm = T)) %>% ungroup()

#I recommend using this code to standardize NAs
is.na(CBP.WQ_spme) <- CBP.WQ_spme == "NaN"
is.na(CBP.WQ_spme) <- CBP.WQ_spme == "Inf"
is.na(CBP.WQ_spme) <- CBP.WQ_spme == "-Inf"

write_csv(CBP.WQ_spme, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_spme.csv")
write_csv(CBP.WQ_spme, "~/Documents/R projects/Predicting-SAV/data/CBP.WQ_spme.csv")

#CBP.WQ_yearly####
#yearly, spring, summer mean, max, mins and range. also calculate D change max min mean
CBP.WQ_yearly <- CBPall %>%
  # na.omit() %>%
  group_by(year, STATION) %>% 
  summarise(Chla.max = max(Chla, na.rm = T), Chla.min = min(Chla, na.rm = T), 
            Chla.me = mean(Chla, na.rm = T),  
            Chla.ran = Chla.max - Chla.min, 
            Chla.Dpos = max(Chla.D, na.rm = T), Chla.Dneg = min(Chla.D, na.rm = T), 
            Chla.Dme = mean(Chla.D, na.rm = T), 
            Secc.max = max(Secc, na.rm = T), Secc.min = min(Secc, na.rm = T), 
            Secc.me = mean(Secc, na.rm = T), 
            Secc.ran = Secc.max - Secc.min, 
            Secc.Dpos = max(Secc.D, na.rm = T), Secc.Dneg = min(Secc.D, na.rm = T), 
            Secc.Dme = mean(Secc.D, na.rm = T),
            Sal.max = max(Sal, na.rm = T), Sal.min = min(Sal, na.rm = T), 
            Sal.me = mean(Sal, na.rm = T), 
            Sal.ran = Sal.max - Sal.min, 
            Sal.Dpos = max(Sal.D, na.rm = T), Sal.Dneg = min(Sal.D, na.rm = T), 
            Sal.Dme = mean(Sal.D, na.rm = T),
            Temp.max = max(Temp, na.rm = T), Temp.min = min(Temp, na.rm = T), 
            Temp.me = mean(Temp, na.rm = T), 
            Temp.ran = Temp.max - Temp.min, 
            Temp.Dpos = max(Temp.D, na.rm = T), Temp.Dneg = min(Temp.D, na.rm = T), 
            Temp.Dme = mean(Temp.D, na.rm = T),
            TP.max = max(TP, na.rm = T), TP.min = min(TP, na.rm = T), 
            TP.me = mean(TP, na.rm = T), 
            TP.ran = TP.max - TP.min, 
            TP.Dpos = max(TP.D, na.rm = T), TP.Dneg = min(TP.D, na.rm = T), 
            TP.Dme = mean(TP.D, na.rm = T),
            TN.max = max(TN, na.rm = T), TN.min = min(TN, na.rm = T), 
            TN.me = mean(TN, na.rm = T), 
            TN.ran = TN.max - TN.min,
            TN.Dpos = max(TN.D, na.rm = T), TN.Dneg = min(TN.D, na.rm = T), 
            TN.Dme = mean(TN.D, na.rm = T), 
            TSS.max = max(TSS, na.rm = T), TSS.min = min(TSS, na.rm = T), 
            TSS.me = mean(TSS, na.rm = T), 
            TSS.ran = TSS.max - TSS.min, 
            TSS.Dpos = max(TSS.D, na.rm = T), TSS.Dneg = min(TSS.D, na.rm = T), 
            TSS.Dme = mean(TSS.D, na.rm = T),
            TSS.med = median(TSS, na.rm = T),TN.med = median(TN, na.rm = T),
            TP.med = median(TP, na.rm = T),Temp.med = median(Temp, na.rm = T),
            Sal.med = median(Sal, na.rm = T),Chla.med = median(Chla, na.rm = T), 
            Secc.med = median(Chla, na.rm = T)) %>%
  ungroup() %>% group_by(STATION) %>%
  mutate(Chla.y1max = lag(Chla.max), Chla.y1min = lag(Chla.min), 
         Chla.y1me = lag(Chla.me), Chla.y1ran = lag(Chla.ran), 
         Chla.y1Dpos = lag(Chla.Dpos), Chla.y1Dneg = lag(Chla.Dneg), Chla.y1Dme = lag(Chla.Dme),
         Secc.y1max = lag(Secc.max), Secc.y1min = lag(Secc.min), 
         Secc.y1me = lag(Secc.me), Secc.y1ran = lag(Secc.ran), 
         Secc.y1Dpos = lag(Secc.Dpos), Secc.y1Dneg = lag(Secc.Dneg), Secc.y1Dme = lag(Secc.Dme),
         Sal.y1max = lag(Sal.max), Sal.y1min = lag(Sal.min), 
         Sal.y1me = lag(Sal.me), Sal.y1ran = lag(Sal.ran), 
         Sal.y1Dpos = lag(Sal.Dpos), Sal.y1Dneg = lag(Sal.Dneg), Sal.y1Dme = lag(Sal.Dme), 
         Temp.y1max = lag(Temp.max), Temp.y1min = lag(Temp.min), 
         Temp.y1me = lag(Temp.me), Temp.y1ran = lag(Temp.ran), 
         Temp.y1Dpos = lag(Temp.Dpos), Temp.y1Dneg = lag(Temp.Dneg), Temp.y1Dme = lag(Temp.Dme),
         TN.y1max = lag(TN.max), TN.y1min = lag(TN.min), 
         TN.y1me = lag(TN.me), TN.y1ran = lag(TN.ran), 
         TN.y1Dpos = lag(TN.Dpos), TN.y1Dneg = lag(TN.Dneg), TN.y1Dme = lag(TN.Dme), 
         TP.y1max = lag(TP.max), TP.y1min = lag(TP.min), 
         TP.y1me = lag(TP.me), TP.y1ran = lag(TP.ran), 
         TP.y1Dpos = lag(TP.Dpos), TP.y1Dneg = lag(TP.Dneg), TP.y1Dme = lag(TP.Dme),
         TSS.y1max = lag(TSS.max), TSS.y1min = lag(TSS.min), 
         TSS.y1me = lag(TSS.me), TSS.y1ran = lag(TSS.ran), 
         TSS.y1Dpos = lag(TSS.Dpos), TSS.y1Dneg = lag(TSS.Dneg), TSS.y1Dme = lag(TSS.Dme),
         TSS.y1med = lag(TSS.med),TN.y1med = lag(TN.med),
         TP.y1med= lag(TP.med), Temp.y1med = lag(Temp.med),
         Sal.y1med = lag(Sal.med), Chla.y1med = lag(Sal.med), 
         Secc.y1med = lag(Secc.med)) %>% 
  ungroup()

colsums(is.na(CBP.WQ_yearly))

#CBP.WQ_summer####
#calc sum means
CBP.WQ_summer <- CBPall %>% 
  filter(between(month, 5, 8)) %>%
  group_by(year, STATION) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), Chla.sumran = Chla.summax - Chla.summin, 
            Chla.sumDpos = max(Chla.D, na.rm = T), Chla.sumDneg = min(Chla.D, na.rm = T), 
            Chla.sumDme = mean(Chla.D, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), Secc.sumran = Secc.summax - Secc.summin, 
            Secc.sumDpos = max(Secc.D, na.rm = T), Secc.sumDneg = min(Secc.D, na.rm = T), 
            Secc.sumDme = mean(Secc.D, na.rm = T),
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), Sal.sumran = Sal.summax - Sal.summin, 
            Sal.sumDpos = max(Sal.D, na.rm = T), Sal.sumDneg = min(Sal.D, na.rm = T), 
            Sal.sumDme = mean(Sal.D, na.rm = T),
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), Temp.sumran = Temp.summax - Temp.summin, 
            Temp.sumDpos = max(Temp.D, na.rm = T), Temp.sumDneg = min(Temp.D, na.rm = T), 
            Temp.sumDme = mean(Temp.D, na.rm = T),
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), TP.sumran = TP.summax - TP.summin, 
            TP.sumDpos = max(TP.D, na.rm = T), TP.sumDneg = min(TP.D, na.rm = T), 
            TP.sumDme = mean(TP.D, na.rm = T),
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), TN.sumran = TN.summax - TN.summin,
            TN.sumDpos = max(TN.D, na.rm = T), TN.sumDneg = min(TN.D, na.rm = T), 
            TN.sumDme = mean(TN.D, na.rm = T), 
            TSS.summax = max(TSS, na.rm = T), TSS.summin = min(TSS, na.rm = T), 
            TSS.summe = mean(TSS, na.rm = T), TSS.sumran = TSS.summax - TSS.summin, 
            TSS.sumDpos = max(TSS.D, na.rm = T), TSS.sumDneg = min(TSS.D, na.rm = T), 
            TSS.sumDme = mean(TSS.D, na.rm = T), 
            TSS.summed = median(TSS, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(STATION) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), Chla.sumy1ran = lag(Chla.sumran), 
         Chla.sumy1Dpos = lag(Chla.sumDpos), Chla.sumy1Dneg = lag(Chla.sumDneg), Chla.sumy1Dme = lag(Chla.sumDme),
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), Secc.sumy1ran = lag(Secc.sumran), 
         Secc.sumy1Dpos = lag(Secc.sumDpos), Secc.sumy1Dneg = lag(Secc.sumDneg), Secc.sumy1Dme = lag(Secc.sumDme),
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), Sal.sumy1ran = lag(Sal.sumran), 
         Sal.sumy1Dpos = lag(Sal.sumDpos), Sal.sumy1Dneg = lag(Sal.sumDneg), Sal.sumy1Dme = lag(Sal.sumDme), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), Temp.sumy1ran = lag(Temp.sumran), 
         Temp.sumy1Dpos = lag(Temp.sumDpos), Temp.sumy1Dneg = lag(Temp.sumDneg), Temp.sumy1Dme = lag(Temp.sumDme),
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), TN.sumy1ran = lag(TN.sumran), 
         TN.sumy1Dpos = lag(TN.sumDpos), TN.sumy1Dneg = lag(TN.sumDneg), TN.sumy1Dme = lag(TN.sumDme), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), TP.sumy1ran = lag(TP.sumran), 
         TP.sumy1Dpos = lag(TP.sumDpos), TP.sumy1Dneg = lag(TP.sumDneg), TP.sumy1Dme = lag(TP.sumDme),
         TSS.sumy1max = lag(TSS.summax), TSS.sumy1min = lag(TSS.summin), 
         TSS.sumy1me = lag(TSS.summe), TSS.sumy1ran = lag(TSS.sumran), 
         TSS.sumy1Dpos = lag(TSS.sumDpos), TSS.sumy1Dneg = lag(TSS.sumDneg), TSS.sumy1Dme = lag(TSS.sumDme), 
         TSS.sumy1med = lag(TSS.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))
 
 
#CBP.WQ_spring####
#All springtime metrics
CBP.WQ_spring <- CBPall %>% 
filter(between(month, 3, 5)) %>%
  group_by(year, STATION) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), Chla.spran = Chla.spmax - Chla.spmin, 
            Chla.spDpos = max(Chla.D, na.rm = T), Chla.spDneg = min(Chla.D, na.rm = T), 
            Chla.spDme = mean(Chla.D, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), Secc.spran = Secc.spmax - Secc.spmin, 
            Secc.spDpos = max(Secc.D, na.rm = T), Secc.spDneg = min(Secc.D, na.rm = T), 
            Secc.spDme = mean(Secc.D, na.rm = T),
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), Sal.spran = Sal.spmax - Sal.spmin, 
            Sal.spDpos = max(Sal.D, na.rm = T), Sal.spDneg = min(Sal.D, na.rm = T), 
            Sal.spDme = mean(Sal.D, na.rm = T),
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), Temp.spran = Temp.spmax - Temp.spmin, 
            Temp.spDpos = max(Temp.D, na.rm = T), Temp.spDneg = min(Temp.D, na.rm = T), 
            Temp.spDme = mean(Temp.D, na.rm = T),
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), TP.spran = TP.spmax - TP.spmin, 
            TP.spDpos = max(TP.D, na.rm = T), TP.spDneg = min(TP.D, na.rm = T), 
            TP.spDme = mean(TP.D, na.rm = T),
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spran = TN.spmax - TN.spmin,
            TN.spDpos = max(TN.D, na.rm = T), TN.spDneg = min(TN.D, na.rm = T), 
            TN.spDme = mean(TN.D, na.rm = T), 
            TSS.spmax = max(TSS, na.rm = T), TSS.spmin = min(TSS, na.rm = T), 
            TSS.spme = mean(TSS, na.rm = T), TSS.spran = TSS.spmax - TSS.spmin, 
            TSS.spDpos = max(TSS.D, na.rm = T), TSS.spDneg = min(TSS.D, na.rm = T), 
            TSS.spDme = mean(TSS.D, na.rm = T), 
            TSS.spmed = median(TSS, na.rm = T),TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T)) %>% ungroup()

#CBP.WQ_grow####
#growing season
CBP.WQ_grow <- CBPall %>% 
  filter(between(month, 3, 7)) %>%
  group_by(year, STATION) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), Chla.growran = Chla.growmax - Chla.growmin, 
            Chla.growDpos = max(Chla.D, na.rm = T), Chla.growDneg = min(Chla.D, na.rm = T), 
            Chla.growDme = mean(Chla.D, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), Secc.growran = Secc.growmax - Secc.growmin, 
            Secc.growDpos = max(Secc.D, na.rm = T), Secc.growDneg = min(Secc.D, na.rm = T), 
            Secc.growDme = mean(Secc.D, na.rm = T),
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), Sal.growran = Sal.growmax - Sal.growmin, 
            Sal.growDpos = max(Sal.D, na.rm = T), Sal.growDneg = min(Sal.D, na.rm = T), 
            Sal.growDme = mean(Sal.D, na.rm = T),
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), Temp.growran = Temp.growmax - Temp.growmin, 
            Temp.growDpos = max(Temp.D, na.rm = T), Temp.growDneg = min(Temp.D, na.rm = T), 
            Temp.growDme = mean(Temp.D, na.rm = T),
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), TP.growran = TP.growmax - TP.growmin, 
            TP.growDpos = max(TP.D, na.rm = T), TP.growDneg = min(TP.D, na.rm = T), 
            TP.growDme = mean(TP.D, na.rm = T),
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T), TN.growran = TN.growmax - TN.growmin,
            TN.growDpos = max(TN.D, na.rm = T), TN.growDneg = min(TN.D, na.rm = T), 
            TN.growDme = mean(TN.D, na.rm = T), 
            TSS.growmax = max(TSS, na.rm = T), TSS.growmin = min(TSS, na.rm = T), 
            TSS.growme= mean(TSS, na.rm = T), TSS.growran = TSS.growmax - TSS.growmin, 
            TSS.growDpos = max(TSS.D, na.rm = T), TSS.growDneg = min(TSS.D, na.rm = T), 
            TSS.growDme = mean(TSS.D, na.rm = T), 
            TSS.growmed = median(TSS, na.rm = T),TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T)) %>% ungroup() %>% 
  group_by(STATION) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), Chla.growy1ran = lag(Chla.growran), 
         Chla.growy1Dpos = lag(Chla.growDpos), Chla.growy1Dneg = lag(Chla.growDneg), Chla.growy1Dme = lag(Chla.growDme),
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), Secc.growy1ran = lag(Secc.growran), 
         Secc.growy1Dpos = lag(Secc.growDpos), Secc.growy1Dneg = lag(Secc.growDneg), Secc.growy1Dme = lag(Secc.growDme),
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), Sal.growy1ran = lag(Sal.growran), 
         Sal.growy1Dpos = lag(Sal.growDpos), Sal.growy1Dneg = lag(Sal.growDneg), Sal.growy1Dme = lag(Sal.growDme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), Temp.growy1ran = lag(Temp.growran), 
         Temp.growy1Dpos = lag(Temp.growDpos), Temp.growy1Dneg = lag(Temp.growDneg), Temp.growy1Dme = lag(Temp.growDme),
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), TN.growy1ran = lag(TN.growran), 
         TN.growy1Dpos = lag(TN.growDpos), TN.growy1Dneg = lag(TN.growDneg), TN.growy1Dme = lag(TN.growDme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TP.growy1ran = lag(TP.growran), 
         TP.growy1Dpos = lag(TP.growDpos), TP.growy1Dneg = lag(TP.growDneg), TP.growy1Dme = lag(TP.growDme),
         TSS.growy1max = lag(TSS.growmax), TSS.growy1min = lag(TSS.growmin), 
         TSS.growy1me = lag(TSS.growme), TSS.growy1ran = lag(TSS.growran), 
         TSS.growy1Dpos = lag(TSS.growDpos), TSS.growy1Dneg = lag(TSS.growDneg), TSS.growy1Dme = lag(TSS.growDme), 
         TSS.growy1med = lag(TSS.growmed),TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed)) %>% ungroup()

#Joining dfs togehter####
#Depends on how you want to do this but we have CBP.WQ_summer, CBP.WQ_spring, and CBP.WQ_yearly
#
CBP.WQ_combined = full_join(CBP.WQ_yearly, CBP.WQ_spring) %>% full_join(CBP.WQ_summer) %>%
  full_join(CBP.WQ_grow)
  #mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(CBP.WQ_combined) <- CBP.WQ_combined == "NaN"
is.na(CBP.WQ_combined) <- CBP.WQ_combined == "Inf"
is.na(CBP.WQ_combined) <- CBP.WQ_combined == "-Inf"

write_csv(CBP.WQ_combined, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/CBP.WQ_combined.csv")
write_csv(CBP.WQ_combined, "~/Documents/R projects/Predicting-SAV/data/CBP.WQ_combined.csv")

