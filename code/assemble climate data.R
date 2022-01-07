#exploring the projected data 
library(tidyverse); library(readxl)

#load CB 5.4 base
######Reading in code (Used Daves though)####
base_cb5.4 = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/Base_Chris/Base_1991_2000_CB5.4.csv", sep = "", header = T)

#load CB 5.4 something else 

fiftytosixty_cb5.4 = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC55_WithLandUseChange_2051_2060/CC55_WithLandUseChange_2051_2060_CB5.4.csv", sep = "", header = T)


library(data.table)

wif.df <- data.table()

file_list <- list.files(path="/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030", pattern="*csv", full.names=TRUE, recursive=FALSE)

for (i in 1:length(file_list)){
  file <- file_list[i]
  station <- substr(file,148,nchar(file)-4)
  temp_data <- read.table(file, header=TRUE) # load file
  temp_data$Station = station
  wif.df <- rbindlist(list(wif.df, temp_data),use.names = TRUE) }
  

#Codes for this shit: WIP25_LUC_startyear_endyear

wif.df <- data.table()

file_list <- list.files(path="/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030", pattern="*csv", full.names=TRUE, recursive=FALSE)

for (i in 1:length(file_list)){
  file <- file_list[i]
  station <- substr(file,148,nchar(file)-4)
  temp_data <- read.table(file, header=TRUE) # load file
  temp_data$Station = station
  wif.df <- rbindlist(list(wif.df, temp_data),use.names = TRUE) }

library(data.table)



wif.df <- data.table()

file_list <- list.files(path="R:/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030", pattern="*csv", full.names=TRUE, recursive=FALSE)

for (i in 1:length(file_list)){
  file <- file_list[i]
  filebase = basename(file)
  station <- substr(filebase,35,nchar(file)-4)
  temp_data <- read.table(file, header=TRUE) # load file
  temp_data$Station = station
  wif.df <- rbindlist(list(wif.df, temp_data),use.names = TRUE)
  
}

######
  
#Tidy projected dataframe#####

#Baseline 1991-2000 dataframe####
baseline_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/Base_Chris.csv")

baseline = baseline_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., 
         TP = TP___.mg.l.) %>% #rename cols, note that K isnt really Secchi
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>% ungroup() #get daily means
  
bascb1.1TN = baseline %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

#load in each of the scenarios by the 9 year chunks

#CC With land use change dataframe####
#CC25 = with land use change, 2021-2030
CC25.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC25_WithLandUseChange_2021_2030.csv")

CC25.wland = CC25.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 30) %>% ungroup() 

#CC35 = with land use change, 2031-2040
CC35.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC35_WithLandUseChange_2031_2040.csv")

CC35.wland = CC35.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 40) %>% ungroup() 

#CC45 = with land use change, 2041-2050
CC45.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC45_WithLandUseChange_2041_2050.csv")

CC45.wland = CC45.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 50) %>% ungroup() 

#CC55 = with land use change, 2051-2060
CC55.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC55_WithLandUseChange_2051_2060.csv")

CC55.wland = CC55.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 60) %>% ungroup() 

#bind them all toghether
CC.wland_2021_2060 = bind_rows(CC25.wland, CC35.wland) %>%
  bind_rows(CC45.wland) %>% bind_rows(CC55.wland)

ggplot(data = CC.wland_2021_2060) +
  stat_summary(aes(x = Year, y = Temp), geom = "smooth", fun = max)

CCcb1.1TN = CC.wland_2021_2060 %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

#WIP With land use change dataframe####
#WIP25 = with land use change, 2021-2030
WIP25.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030.csv")

WIP25.wland = WIP25.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 30) %>% ungroup() 

#WIP35 = with land use change, 2031-2040
WIP35.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP35_WithLandUseChange_2031_2040.csv")

WIP35.wland = WIP35.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 40) %>% ungroup() 

#WIP45 = with land use change, 2041-2050
WIP45.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP45_WithLandUseChange_2041_2050.csv")

WIP45.wland = WIP45.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 50) %>% ungroup() 

#WIP55 = with land use change, 2051-2060
WIP55.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP55_WithLandUseChange_2051_2060.csv")

WIP55.wland = WIP55.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 60) %>% ungroup() 

#bind them all toghether
WIP.wland_2021_2060 = bind_rows(WIP25.wland, WIP35.wland) %>%
  bind_rows(WIP45.wland) %>% bind_rows(WIP55.wland)

WIPwcb1.1TN = WIP.wland_2021_2060 %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

#WIP Without land use change dataframe####
#WIP25 = without land use change, 2021-2030 THIS DOES NOT EXIST!!!!
#WIP25.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithoutLandUseChange_2021_2030.csv")

##WIP25.woland = WIP25.woland_all %>% 
#  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
#         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
#  select(Year, Month, Station, everything()) %>%
#  group_by(Year, Month, Day, Station) %>%
#  summarize(across(Depth:TP, mean)) %>%
#  mutate(Year = Year + 30) %>% ungroup() 

#WIP35 = with land use change, 2031-2040
WIP35.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP35_WithoutLandUseChange_2031-2040.csv")

WIP35.woland = WIP35.woland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 40) %>% ungroup() 

#WIP45 = with land use change, 2041-2050
WIP45.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP45_WithoutLandUseChange_2041-2050.csv")

WIP45.woland = WIP45.woland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 50) %>% ungroup() 

#WIP55 = with land use change, 2051-2060
WIP55.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP55_WithoutLandUseChange_2051-2060.csv")

WIP55.woland = WIP55.woland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 60) %>% ungroup() 

#bind them all toghether
WIP.woland_2031_2060 = bind_rows(WIP35.woland, WIP45.woland) %>%
  bind_rows(WIP55.woland)

WIPwocb1.1TN = WIP.woland_2031_2060 %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

###Build temporal variable dataframes 

#Build Temporal variable DFs, reminder that 69 Vars has longer grow and spring####
#baseline_1990-2000####
baselinesummer = baseline %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
baselinegrow = baseline %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, Chla.growy1max:Depth.growy1med) #we dont use the grow data. its too soon

####Summarize Spring
baselinesp = baseline %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together##

baseline_69vars = full_join(baselinegrow, baselinesummer) %>% full_join(baselinesp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(baseline_69vars) <- baseline_69vars == "NaN"
is.na(baseline_69vars) <- baseline_69vars == "Inf"
is.na(baseline_69vars) <- baseline_69vars == "-Inf"

colSums(is.na(baseline_69vars))

write_csv(baseline_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/baseline_69vars.csv")


#CC.wland_2021_2060####
#summarize Summer
CC.wland_2021_2060summer = CC.wland_2021_2060 %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
CC.wland_2021_2060grow = CC.wland_2021_2060 %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, everything())


####Summarize Spring
CC.wland_2021_2060sp = CC.wland_2021_2060 %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together##

CC.wland_2021_2060_69vars = full_join(CC.wland_2021_2060grow, CC.wland_2021_2060summer) %>% full_join(CC.wland_2021_2060sp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(CC.wland_2021_2060_69vars) <- CC.wland_2021_2060_69vars == "NaN"
is.na(CC.wland_2021_2060_69vars) <- CC.wland_2021_2060_69vars == "Inf"
is.na(CC.wland_2021_2060_69vars) <- CC.wland_2021_2060_69vars == "-Inf"

colSums(is.na(CC.wland_2021_2060_69vars))

write_csv(CC.wland_2021_2060_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/CC.wland_2021_2060_69vars.csv")


#WIP.wland_2021_2060####
#summarize Summer
WIP.wland_2021_2060summer = WIP.wland_2021_2060 %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
WIP.wland_2021_2060grow = WIP.wland_2021_2060 %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, everything())

####Summarize Spring
WIP.wland_2021_2060sp = WIP.wland_2021_2060 %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together#

WIP.wland_2021_2060_69vars = full_join(WIP.wland_2021_2060grow, WIP.wland_2021_2060summer) %>% full_join(WIP.wland_2021_2060sp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(WIP.wland_2021_2060_69vars) <- WIP.wland_2021_2060_69vars == "NaN"
is.na(WIP.wland_2021_2060_69vars) <- WIP.wland_2021_2060_69vars == "Inf"
is.na(WIP.wland_2021_2060_69vars) <- WIP.wland_2021_2060_69vars == "-Inf"

colSums(is.na(WIP.wland_2021_2060_69vars))

write_csv(WIP.wland_2021_2060_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.wland_2021_2060_69vars.csv")


#WIP.woland_2031_2060####
#summarize Summer
WIP.woland_2031_2060summer = WIP.woland_2031_2060 %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
WIP.woland_2031_2060grow = WIP.woland_2031_2060 %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, everything())

####Summarize Spring
WIP.woland_2031_2060sp = WIP.woland_2031_2060 %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together#

WIP.woland_2031_2060_69vars = full_join(WIP.woland_2031_2060grow, WIP.woland_2031_2060summer) %>% full_join(WIP.woland_2031_2060sp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(WIP.woland_2031_2060_69vars) <- WIP.woland_2031_2060_69vars == "NaN"
is.na(WIP.woland_2031_2060_69vars) <- WIP.woland_2031_2060_69vars == "Inf"
is.na(WIP.woland_2031_2060_69vars) <- WIP.woland_2031_2060_69vars == "-Inf"

colSums(is.na(WIP.woland_2031_2060_69vars))

write_csv(WIP.woland_2031_2060_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.woland_2031_2060_69vars.csv")

#Vizualize this shit####

CC.wland_2021_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CC.wland_2021_2060_69vars.csv")

WIP.wland_2021_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.wland_2021_2060_69vars.csv")

WIP.woland_2031_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.woland_2031_2060_69vars.csv")

CC.wland_2021_2060_69vars
WIP.wland_2021_2060_69vars
WIP.woland_2031_2060_69vars

temptime.cc = lmer(Temp.summax ~ Year + (1|Station), data = CC.wland_2021_2060_69vars)
temptime.wipw = lm(Temp.summax ~ Year, data = WIP.wland_2021_2060_69vars)
temptime.wipwo = lm(Temp.summax ~ Year, data = WIP.woland_2031_2060_69vars)

summary(temptime.cc)
car::Anova(temptime.cc)
car::Anova(temptime.wipw)
car::Anova(temptime.wipwo)

check_wip =
WIP.woland_2031_2060_69vars %>%
  select(Year, Station, Temp.summed, TN.spme, Chla.spme, Sal.growy1med, TP.spmax, Secc.growmax) %>%
  filter(Station %in% c("EE3.1", "LE5.4", "CB7.1")) %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)) %>%
  group_by(Station) %>%
  summarise(across(Temp.summed:Secc.growmax, ~max(.x)-min(.x)))
          


ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
             geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "CB7.1"), aes(x = year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  labs(x = "", y = expression("TN, summer mean, station CB7.1")) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  geom_point(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe), size = 1, color = "blue") +
  geom_point(data = WIP.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe), size = 1, color = "black") +
  geom_point(data = WIP.woland_2031_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),size = 1, color = "red") +
  geom_point(data = baseline_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe), size = 1, color = "purple") +
  geom_point(data = CBP.WQ_69vars %>% filter(STATION == "CB7.1"), aes(x = year, y = TN.summe),size = 1, color = "pink") +
  labs(x = "", y = expression("TN, summer mean, station CB7.1")) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "CB6.4"), aes(x = year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  ylim(20, 35) +
  labs(x = "", y = expression("Temp, summer max at CB6.4")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars, aes(x = year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  labs(x = "", y = expression("Temp baywide, spring mean")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#why do mean and max look different
ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "EE3.1"), aes(x = year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  ylim(0, 100) +
  labs(x = "", y = expression("Chl-a, spring max, Station EE3.1")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "EE3.1"), aes(x = year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "pink") +
 # ylim(0, 100) +
  labs(x = "", y = expression("Chl-a, spring mean, Station EE3.1")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

